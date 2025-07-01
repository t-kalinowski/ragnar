#' Creates an `ellmer::Chat` with increased capabilities
#' powered by `ragnar::RagnarStore`.
#'
#' @param chat_fun A function that returns an `ellmer::Chat` object,
#'   sunch as [ellmer::chat_openai()] , etc.
#' @param ... Additional parameters passed to `chat_fun`.
#' @param .store An `ragnar::RagnarStore` object that contains the knowledge base that powers
#'   this chat.
#' @param .on_user_turn A function that is called when the user sends a message.
#'   It's called with `self` (the instance of `RagnarChat`), and `...` the message
#'   sent by the user. It's output is passed to `ellmer::Chat$chat()`.
#'   Eg, the identity is simply `function(self, ...) list(...)`.
#'   The default callback prunes the previous tool calls from the chat history and
#'   inserts a tool call request, so that the LLM always sees retrieval results.
#' @param .retrieve A function that takes `self` (the instance of `RagnarChat`) and `query`
#'   (the query to retrieve results for) and returns a data.frame of chunks as results.
#'   The default implementation calls `ragnar_retrieve()` on chunks, after filtering those
#'   already present in the chat history.
#' @export
chat_ragnar <- function(
  chat_fun,
  ...,
  .store,
  .register_store = TRUE,
  .on_user_turn = function(self, ...) {
    # prunes previously inserted tool calls
    self$turns_prune_chunks(keep_last_n = 0)
    # inserts a new tool call request with the user's input
    self$turns_insert_tool_call_request(..., query = paste(..., collapse = " "))
  },
  .retrieve = function(self, query) {
    retrieved_ids <- self$turns_list_chunks() |>
      sapply(\(x) x$id)

    self$ragnar_store |>
      dplyr::tbl() |>
      dplyr::filter(!.data$id %in% retrieved_ids) |>
      ragnar::ragnar_retrieve(query, top_k = 10)
  }
) {
  chat <- chat_fun(...)
  RagnarChat$new(chat, .store, .register_store, .on_user_turn, .on_retrieval, .retrieve)
}

#' Adds extra capabilities to a `ellmer::Chat` object.
RagnarChat <- R6::R6Class(
  "RagnarChat",
  inherit = ellmer:::Chat,
  public = list(
    #' @field ragnar_store An `ragnar::RagnarStore` object that this tool retrieves from.
    ragnar_store = NULL,

    #' @field ragnar_tool_def An `ellmer::Tool` object that is registered with the chat.
    ragnar_tool_def = NULL,

    #' @field on_user_turn A function that is called when the user sends a message.
    #'  It's called with `self` (the instance of `RagnarChat`), and `...` the message
    #'  sent by the user. It's output is passed to `ellmer::Chat$chat()`.
    #'  Eg, the identity is simply `function(self, ...) list(...)`.
    on_user_turn = NULL,

    #' @field ragnar_retrieve A function that retrieves relevant chunks given a query and a store.
    #'  Can be set to any function taking `self` and `query`` as parameters and returning a data.frame
    #'  of results.
    ragnar_retrieve = NULL,

    initialize = function(
      chat,
      store,
      register_store,
      on_user_turn,
      on_retrieval,
      ragnar_retrieve
    ) {
      self$ragnar_store <- store
      super$initialize(
        chat$get_provider(),
        chat$get_system_prompt(),
        echo = chat$.__enclos_env__$private$echo
      )

      self$ragnar_tool_def = ellmer::tool(
        .fun = self$ragnar_tool,
        .description = "Given a string, retrieve the most relevant excerpts from the knowledge store.",
        query = ellmer::type_string(
          "The text to find most relevant matches for."
        )
      )
      if (register_store) {
        self$register_tool(self$ragnar_tool_def)
      }
      self$on_user_turn <- on_user_turn
      self$ragnar_retrieve <- ragnar_retrieve
    },

    chat = function(..., echo = NULL) {
      result <- private$callback_user_turn(...)
      do.call(super$chat, append(result, list(echo = echo)))
    },

    chat_async = function(..., tool_mode = c("concurrent", "sequential")) {
      result <- private$callback_user_turn(...)
      do.call(super$chat_async, append(result, list(tool_mode = tool_mode)))
    },

    chat_structured = function(..., type, echo = "none", convert = TRUE) {
      result <- private$callback_user_turn(...)
      do.call(
        super$chat_structured,
        append(result, list(type = type, echo = echo, convert = convert))
      )
    },

    chat_structured_async = function(..., type, echo = "none") {
      result <- private$callback_user_turn(...)
      do.call(
        super$chat_structured_async,
        append(result, list(type = type, echo = echo))
      )
    },

    #' @field ragnar_tool A function that retrieves relevant chunks from the store.
    #'  This is the function that is registered as a tool in the chat.
    ragnar_tool = function(query) {
      results <- self$ragnar_retrieve(self, query)

      if (!is.data.frame(results)) {
        stop("The ragnar_retrieve function must return a data.frame.")
      }

      results |>
        jsonlite::toJSON()
    },

    # Turns modifications API -----------

    #' @description
    #' Clears tool calls from the chat history. Usually called after the LLM response,
    #' so follow up questions do not include the tools calls, saving some tokens.
    #' This only removes calls for the registered store.
    #'
    #' @param keep_last_n Keep the last `n` tools call pairs. Ie, if you set this to 2,
    #'  the last two assistant tool call requests and their respective results will be kept.
    #'  - Parallel tools calls are considered a 'single request'.
    #'  - Tool results are expected to be in the user turn right after the assistant's turn
    #'  containing the tool call request.
    #'  - We drop the entire turns, even (in the rare case) if they contain other content
    #'  besides the tool call.
    turns_prune_tool_calls = function(keep_last_n = 0) {
      turns <- self$get_turns()

      skipped <- 0
      drop_turns <- integer(0)
      for (i in rev(seq_along(turns))) {
        turn <- turns[[i]]
        if (turn@role != "assistant") {
          next
        }

        for (content in turn@contents) {
          if (!S7::S7_inherits(content, ellmer::ContentToolRequest)) {
            next
          }
          if (content@name != self$ragnar_tool_def@name) {
            next
          }

          # This is a tool call turn. We now check if we alreaddy skipped
          # enough to keep the last n.
          if (skipped < keep_last_n) {
            skipped <- skipped + 1
            break
          }

          # Mark this turn and the user turn after it for removal
          drop_turns <- c(drop_turns, c(i, i + 1))
          break
        }
      }

      self$set_turns(turns[-drop_turns])
    },

    #' @description
    #' Finds all the chunks that are currently in the chat history.
    #' Requires chunks returned by the tools to be formatted as json.
    turns_list_chunks = function() {
      turns <- self$get_turns()
      chunks <- list()

      for (turn in turns) {
        if (turn@role != "user") {
          next
        }
        for (content in turn@contents) {
          chunks <- append(
            chunks,
            content_get_chunks(content, tool_name = self$ragnar_tool_def@name)
          )
        }
      }

      chunks
    },

    #' @description
    #' Prunes the chunks in the chat history, keeping only the last `n` chunks.
    #' This is useful to reduce the size of the chat history, especially if you have
    #' many tool calls that return large chunks of text.
    #' @param keep_last_n The number of chunks to keep. If `0`, all chunks are removed.
    #'  - Removes chunks from the tool result. If by doing this, the tool result becomes
    #'  empty, then entire turn is and the assistant tool call request is removed.
    turns_prune_chunks = function(keep_last_n = 0) {
      turns <- self$get_turns()

      drop_turn_idx <- integer(0)
      for (ti in rev(seq_along(turns))) {
        turn <- turns[[ti]]

        if (turn@role != "user") {
          next
        }

        contents <- turn@contents

        drop_content_idx <- integer(0)
        for (ci in rev(seq_along(contents))) {
          content <- contents[[ci]]
          chunks <- content_get_chunks(content, tool_name = self$ragnar_tool_def@name)
          if (is.null(chunks)) {
            next
          }

          # We still have to skip some chunks, we check if we can skip everything from this
          # tool result.
          if (length(chunks) <= keep_last_n) {
            keep_last_n <- keep_last_n - length(chunks)
            if (keep_last_n < 0) {
              keep_last_n <- 0
            }
            next
          }

          # We'll need to remove some chunks. Keep only what we can.
          chunks <- tail(chunks, keep_last_n)

          # If we have no chunks left, we remove the entire content from the list.
          if (length(chunks) == 0) {
            drop_content_idx[[length(drop_content_idx) + 1]] <- ci
            turns_to_drop <- if (
              S7::S7_inherits(content, ellmer::ContentToolResult)
            ) {
              # when it's a tool result we drop the assistant turn that (usually a tool call)
              c(ti, ti - 1)
            } else if (S7::S7_inherits(content, ContentRagnarDocuments)) {
              # when it's content ragnar documents, we drop the next turn - because we proactively
              # inserted the documents and the next turn is just the LLM acknowledging them.
              c(ti, ti + 1)
            }
            next
          }

          # Restore the content if some chunks remained.
          contents[[ci]] <- content_set_chunks(content, chunks)
        }

        turn@contents <- contents
        if (length(drop_content_idx) > 0) {
          turn@contents <- contents[-drop_content_idx]
        }

        # If we removed all contents from the turn, we remove the entire turn.
        # and the assistant turn that came before or after it, dependning on what
        # triggered the removal - a tool call result or a proactively added set of documents
        if (length(turn@contents) == 0) {
          drop_turn_idx <- c(drop_turn_idx, turns_to_drop)
          next
        }

        turns[[ti]] <- turn
      }

      # Remove the turns that we marked for removal.
      if (length(drop_turn_idx) > 0) {
        turns <- turns[-drop_turn_idx]
      }

      self$set_turns(turns)
    },

    #' @description
    #' Removes chunks from the history by id.
    #' Rewrites the LLm context remving the chunks with the given ids. It will also
    #' enitrely remove the tool call request and results if all chunks are removed.
    #'
    #' @param chunk_ids A vector of chunk ids to remove from the chat history.
    turns_remove_chunks = function(chunk_ids) {
      turns <- self$get_turns()
      drop_turn_idx <- integer(0)

      for (ti in seq_along(turns)) {
        turn <- turns[[ti]]
        if (turn@role != "user") {
          next
        }

        contents <- turn@contents
        drop_content_idx <- integer(0)

        for (ci in seq_along(contents)) {
          content <- contents[[ci]]

          chunks <- content_get_chunks(content, self$ragnar_tool_def@name)
          if (is.null(chunks)) {
            next
          }

          # Remove the chunks with the given ids.
          chunks <- chunks[!sapply(chunks, function(x) x$id %in% chunk_ids)]

          # If we have no chunks left, we remove the entire content from the list.
          if (length(chunks) == 0) {
            drop_content_idx[[length(drop_content_idx) + 1]] <- ci
            turns_to_drop <- if (
              S7::S7_inherits(content, ellmer::ContentToolResult)
            ) {
              # when it's a tool result we drop the assistant turn that (usually a tool call)
              c(ti, ti - 1)
            } else if (S7::S7_inherits(content, ContentRagnarDocuments)) {
              # when it's content ragnar documents, we drop the next turn - because we proactively
              # inserted the documents and the next turn is just the LLM acknowledging them.
              c(ti, ti + 1)
            }
            next
          }

          # Restore the content if some chunks remained.
          contents[[ci]] <- content_set_chunks(content, chunks)
        }

        turn@contents <- contents
        if (length(drop_content_idx) > 0) {
          turn@contents <- contents[-drop_content_idx]
        }

        # If we removed all contents from the turn, we remove the entire turn.
        # and the assistant turn that came before or after it, dependning on what
        # triggered the removal - a tool call result or a proactively added set of documents
        if (length(turn@contents) == 0) {
          drop_turn_idx <- c(drop_turn_idx, turns_to_drop)
          next
        }

        turns[[ti]] <- turn
      }

      # Remove the turns that we marked for removal.
      if (length(drop_turn_idx) > 0) {
        turns <- turns[-drop_turn_idx]
      }

      self$set_turns(turns)
    },

    #' @description
    #' Some LLM's are lazy at tool calling, and for applications to be
    #' robust, it's great to append context for the LLM, even if
    #' it didn't really ask for.
    #' This inserts a tool call request and it's results in the chat turns, so
    #' the LLM can use to respond the user question.
    #' @param ... Passed to `elmer:::user_turn()` to insert the user turn that generated the tool call.
    #' @param query The query to pass to the tool.
    #' @returns A `ellmer::ContentToolResult` object that should be included in the next call to `$chat()`.
    turns_insert_tool_call_request = function(..., query) {
      user_turn <- ellmer:::user_turn(...)

      tool_request <- ellmer::ContentToolRequest(
        id = rlang::hash(Sys.time()),
        name = self$ragnar_tool_def@name,
        arguments = list(
          query = query
        ),
        self$ragnar_tool_def
      )

      assistant_turn <- ellmer::Turn(
        role = "assistant",
        contents = list(tool_request)
      )

      self$add_turn(user_turn, assistant_turn)

      ellmer::ContentToolResult(
        self$ragnar_tool(query),
        request = tool_request
      )
    },

    #' @description Some models do not support tool calling, so instead of adding a tool call
    #' request (faking that the model asked for some search results) we actually
    #' proactively insert context into the chat user - as if the user did it had done it.
    #' - User: {documents}
    #' - LLM: {llm_answer}.
    #' - {...} The contents of the user turn that generated the tool call.
    #' @param ... The contents of the user turn that generated the tool call.
    #' @param llm_answer The answer that the LLM should give after the documents are inserted.
    #' @param after The index in the turns list to insert the user and assistant turns.
    turns_insert_documents = function(
      ...,
      query,
      llm_answer = "Thanks for the information. I'll use the documents to answer your question. Please ask a question.",
      after = 0L
    ) {
      documents <- self$ragnar_tool(query)

      user_turn <- ellmer::Turn(
        role = "user",
        contents = list(
          ContentRagnarDocuments(text = documents)
        )
      )

      assistant_turn <- ellmer::Turn(
        role = "assistant",
        contents = list(
          ellmer::ContentText(text = llm_answer)
        )
      )

      turns <- self$get_turns()
      turns <- append(turns, list(user_turn, assistant_turn), after = after)
      self$set_turns(turns)

      list(...)
    }
  ),

  private = list(
    callback_user_turn = function(...) {
      result <- self$on_user_turn(self, ...)
      if (!is.list(result)) {
        result <- list(result)
      }
      result
    }
  )
)

ContentRagnarDocuments <- S7::new_class(
  "ContentRagnarDocuments",
  parent = ellmer::ContentText
)

content_get_chunks <- function(x, tool_name) {
  value <- if (S7::S7_inherits(x, ContentRagnarDocuments)) {
    x@text
  } else if (S7::S7_inherits(x, ellmer::ContentToolResult)) {
    if (x@request@name != tool_name) {
      return(NULL)
    }
    x@value
  } else {
    return(NULL)
  }
  jsonlite::fromJSON(value, simplifyVector = FALSE)
}

content_set_chunks <- function(x, chunks) {
  chunks <- jsonlite::toJSON(chunks, pretty = TRUE)
  if (S7::S7_inherits(x, ContentRagnarDocuments)) {
    x@text <- chunks
  } else if (S7::S7_inherits(x, ellmer::ContentToolResult)) {
    x@value <- chunks
  } else {
    stop("Unsupported content type for setting chunks.")
  }
  x
}
