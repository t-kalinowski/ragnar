test_that("RagnarChat", {
  store <- test_store()
  chat <- chat_ragnar(ellmer::chat_openai, .store = store)

  out <- chat$chat("advanced R")

  # 2 turns from the chat + 2 turns of forced tool calls
  expect_equal(length(chat$get_turns()), 2 + 2)

  # pruning clears the tool calls
  chat$turns_prune_chunks()
  expect_equal(length(chat$get_turns()), 2)
  
  out <- chat$chat("advanced R")
  expect_equal(length(chat$get_turns()), 2 + 2)

  # the default pruning will clear the previous tool calls.
  # so we end up with 6 turns
  out <- chat$chat("more chatting")
  expect_equal(length(chat$get_turns()), 2 + 2 + 2)
})

test_that("Implementing query rewriting", {
  # By default we're sending the full user input as a retrieval query.
  # this might not be ideal, and the user may want to implement a query
  # rewriting strategy:

  query_rewriter <- function(...) {
    # ... takes user input and returns a new query
    "hello world"
  }

  store <- test_store()
  chat <- chat_ragnar(
    ellmer::chat_openai, 
    .store = store,
    .on_user_turn = function(self, ...) {
      
      self$turns_prune_tool_calls()
      self$turns_insert_tool_call_request(
        ...,
        query = query_rewriter(...)
      )
    }
  )

  out <- chat$chat("advanced R")
  expect_equal(length(chat$get_turns()), 2 + 2)

  tool_call_request <- chat$get_turns()[[2]]@contents[[1]]
  expect_equal(tool_call_request@arguments$query, "hello world")
})
