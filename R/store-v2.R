ragnar_store_create_v2 <- function(
  location = ":memory:",
  embed = embed_ollama(model = "snowflake-arctic-embed2:568m"),
  embedding_size = ncol(embed("foo")),
  overwrite = FALSE,
  ...,
  extra_cols = NULL,
  name = NULL,
  title = NULL
) {
  rlang::check_dots_empty()

  check_string(location)
  check_store_overwrite(location, overwrite)
  name <- name %||% unique_store_name()
  check_string(name)
  stopifnot(grepl("^[a-zA-Z0-9_-]+$", name))
  check_string(title, allow_null = TRUE)

  con <- if (is_motherduck_location(location)) {
    motherduck_connection(location, create = TRUE, overwrite)
  } else {
    dbConnect(
      duckdb::duckdb(),
      dbdir = location,
      array = "matrix",
      overwrite = overwrite
    )
  }

  if (is.null(embed)) {
    embedding_size <- NULL
  } else {
    # make sure to force and process `embed()` before forcing `embedding_size`
    embed <- process_embed_func(embed)
    check_number_whole(embedding_size, min = 0)
    embedding_size <- as.integer(embedding_size)

    if (!inherits(embed, "crate")) {
      environment(embed) <- baseenv()
      embed <- rlang::zap_srcref(embed)
    }
  }

  metadata <- data_frame(
    embedding_size = embedding_size,
    embed_func = blob::blob(serialize(embed, NULL)),
    name = name,
    title = title
  )
  dbWriteTable(con, "metadata", metadata, overwrite = TRUE)

  # read back in embed, so any problems with an R function that doesn't
  # serialize correctly flush out early.
  metadata <- dbReadTable(con, "metadata")
  embed <- unserialize(metadata$embed_func[[1L]])
  name <- metadata$name

  # attach function to externalptr, so we can retrieve it from just the connection.
  ptr <- con@conn_ref
  attr(ptr, "embed_function") <- embed

  dbExecute(
    con,
    glue(
      r"--(
      DROP VIEW IF EXISTS chunks CASCADE;
      DROP TABLE IF EXISTS embeddings CASCADE;
      DROP TABLE IF EXISTS documents CASCADE;
      DROP SEQUENCE IF EXISTS chunk_id_seq CASCADE;
      DROP SEQUENCE IF EXISTS doc_id_seq CASCADE;
      )--"
    )
  )

  if (length(extra_cols)) {
    schema <- extra_cols_to_schema(extra_cols)
    extra_col_types <- DBI::dbDataType(con, schema)
    extra_col_names <- dbQuoteIdentifier(con, names(extra_col_types))
    extra_cols <- paste0(
      extra_col_names,
      " ",
      extra_col_types,
      ",",
      collapse = "\n"
    )
  } else {
    schema <- NULL
    extra_cols <- ""
  }

  embedding <- if (is.null(embed)) {
    ""
  } else {
    glue("embedding FLOAT[{embedding_size}]")
  }

  dbExecute(
    con,
    glue(
      r"--(
      CREATE SEQUENCE chunk_id_seq START 1; -- need a unique id for fts

      CREATE OR REPLACE TABLE documents (
        origin VARCHAR NOT NULL PRIMARY KEY, -- default  hash(text)??
        text VARCHAR
      );

      CREATE OR REPLACE TABLE embeddings (
        origin VARCHAR NOT NULL,
        FOREIGN KEY (origin) REFERENCES documents (origin),
        id INTEGER DEFAULT nextval('chunk_id_seq'),
        start INTEGER,
        "end" INTEGER,
        PRIMARY KEY (origin, start, "end"),
        context VARCHAR,
        {extra_cols}
        {embedding}
      );

      CREATE OR REPLACE VIEW chunks AS (
        SELECT
          e.*,
          d.text[ e.start : e."end" ] as text
        FROM
          documents d
        JOIN
          embeddings e
        USING
          (origin)
      );
    )--"
    )
  )

  DuckDBRagnarStore(
    embed = embed,
    con = con,
    name = name,
    title = title,
    schema = schema,
    version = 2L
  )
}

extra_cols_to_schema <- function(extra_cols) {
  ptype <- vctrs::vec_ptype(extra_cols)

  disallowd_cols <- c(
    "origin",
    "text",
    "start",
    "end",
    "context",
    "embedding"
  )

  if (any(names(ptype) %in% disallowd_cols)) {
    stop(
      "The following column names are not allowed in `extra_cols`: ",
      paste(disallowd_cols, collapse = ", ")
    )
  }

  ptype
}

#
# ragnar_store_connect_v2 <- function(location, read_only = TRUE) {
#   con <- dbConnect(
#     duckdb::duckdb(),
#     dbdir = location,
#     read_only = read_only,
#     array = "matrix"
#   )
#   dbExecute(con, "INSTALL fts; INSTALL vss;")
#   dbExecute(con, "LOAD fts; LOAD vss;")
#   metadata <- dbReadTable(con, "metadata")
#   embed <- unserialize(metadata$embed_func[[1L]])
#
#   ptr <- con@conn_ref
#   attr(ptr, "embed_function") <- embed
#
#   DuckDBRagnarStore(
#     embed = embed,
#     con = con,
#     name = metadata$name,
#     title = metadata$title,
#     version = 2L
#   )
# }

ragnar_store_build_index_v2 <- function(store, type = c("vss", "fts")) {
  if (S7_inherits(store, DuckDBRagnarStore)) {
    con <- store@con
  } else if (methods::is(store, "DBIConnection")) {
    con <- store
  } else {
    stop("`store` must be a RagnarStore")
  }

  if ("vss" %in% type && !is.null(store@embed)) {
    # TODO: duckdb has support for three different distance metrics that can be
    # selected when building the index: l2sq, cosine, and ip. Expose these as options
    # in the R interface. https://duckdb.org/docs/stable/core_extensions/vss#usage

    # TODO: expose way to select vss index metric types in api
    if (is_motherduck_con(store@con)) {
      warning("MotherDuck does not support VSS index, skipping.")
    } else {
      dbExecute(con, "INSTALL vss; LOAD vss;")
      dbExecute(
        con,
        r"--(
        SET hnsw_enable_experimental_persistence = true;

        DROP INDEX IF EXISTS store_hnsw_cosine_index;
        DROP INDEX IF EXISTS store_hnsw_l2sq_index;
        DROP INDEX IF EXISTS store_hnsw_ip_index;

        CREATE INDEX store_hnsw_cosine_index ON embeddings USING HNSW (embedding) WITH (metric = 'cosine');
        CREATE INDEX store_hnsw_l2sq_index   ON embeddings USING HNSW (embedding) WITH (metric = 'l2sq'); -- array_distance?
        CREATE INDEX store_hnsw_ip_index     ON embeddings USING HNSW (embedding) WITH (metric = 'ip');  -- array_dot_product
        )--"
      )
    }
  }

  if ("fts" %in% type) {
    dbExecute(con, "INSTALL fts; LOAD fts;")
    # fts index builder takes many options, e.g., stemmer, stopwords, etc.
    # Expose a way to pass along args. https://duckdb.org/docs/stable/core_extensions/full_text_search
    dbWithTransaction2(con, {
      dbExecute(
        con,
        r"--(
        ALTER VIEW chunks RENAME TO chunks_view;

        CREATE TABLE chunks AS
          SELECT id, context, text FROM chunks_view;
        )--"
      )
      dbExecute(
        con,
        r"--(
        PRAGMA create_fts_index(
          'chunks',            -- input_table
          'id',                -- input_id
          'context', 'text',  -- *input_values
          overwrite = 1
        );
        )--"
      )
      dbExecute(
        con,
        r"--(
        DROP TABLE chunks;
        ALTER VIEW chunks_view RENAME TO chunks
        )--"
      )
    })
  }

  invisible(store)
}


ragnar_store_update_v2 <- function(store, chunks) {
  stopifnot(
    store@version == 2,
    S7_inherits(chunks, MarkdownDocumentChunks)
  )

  if ("text" %in% names(chunks)) {
    if (with(chunks, !identical(text, stri_sub(chunks@document, start, end)))) {
      stop("modifying chunks$text is not supported with store@version == 2")
    }
  } else {
    chunks <- chunks |> mutate(text = stri_sub(chunks@document, start, end))
  }

  con <- store@con

  existing <- tbl(con, "chunks") |>
    filter(origin == !!chunks@document@origin) |>
    select(start, end, context, text, !!!names(store@schema)) |>
    collect()

  new_chunks <- anti_join(
    chunks,
    existing,
    by = join_by(start, end, context, text, !!!names(store@schema))
  )
  if (!nrow(new_chunks)) {
    return(invisible(store))
  }

  documents <- tibble(origin = chunks@document@origin, text = chunks@document)
  embeddings <- chunks |>
    mutate(
      origin = chunks@document@origin,
      embedding = store@embed(stri_c(context, "\n", text)),
      text = NULL
    ) |>
    select(any_of(dbListFields(con, "embeddings")))
  local_duckdb_register(con, "documents_to_upsert", documents)

  dbWithTransaction2(con, {
    dbExecute(
      con,
      "
      INSERT OR REPLACE INTO documents BY NAME
      SELECT origin, text FROM documents_to_upsert;
      "
    )
    dbExecute(
      con,
      "DELETE FROM embeddings WHERE origin = ?;",
      params = list(chunks@document@origin)
    )
    dbAppendTable(con, "embeddings", embeddings)
  })
}


ragnar_store_insert_v2 <- function(store, chunks, replace_existing = FALSE) {
  if (!S7_inherits(chunks, MarkdownDocumentChunks)) {
    stop(glue::trim(
      "Invalid input for store. `store@version == 2`, but input provided is store version 1.,
       Either call `ragnar_store_create(..., version = 1)` or use `markdown_chunk()` to
       prepare inputs."
    ))
  }
  stopifnot(
    store@version == 2,
    S7_inherits(chunks, MarkdownDocumentChunks)
  )

  if ("text" %in% names(chunks)) {
    if (
      !identical(
        chunks$text,
        stri_sub(chunks@document, chunks$start, chunks$end)
      )
    ) {
      stop("modifying chunks$text is not supported with store@version == 2")
    }
  } else {
    chunks$text <- stri_sub(chunks@document, chunks$start, chunks$end)
  }

  if (!is.null(store@embed) && !"embedding" %in% names(chunks)) {
    chunks$embedding <- store@embed(with(chunks, stri_c(context, "\n", text)))
  }

  con <- store@con
  documents <- tibble(
    origin = chunks@document@origin,
    text = as.character(chunks@document)
  )

  embeddings <- chunks |>
    select(any_of(dbListFields(con, "embeddings"))) |>
    mutate(origin = chunks@document@origin)

  # TODO: rename embeddings -> chunks_info?
  dbWithTransaction2(con, {
    dbAppendTable(con, "documents", documents)
    dbAppendTable(con, "embeddings", embeddings)
  })
  invisible(store)
}


dbWithTransaction2 <- function(con, code, ...) {
  ## DBI::dbWithTransaction() swallows the actual user-meaningful error message,
  ## and replaces it with something cryptic and non-actionable like:
  ##   TransactionContext Error: Current transaction is aborted (please ROLLBACK).
  ## Here, we capture and rethrow the inner (first thrown) error which is more helpful,
  ## e.g.,:
  ##   Constraint Error: Duplicate key "origin" violates primary key constraint.
  errors <- list()
  collect_error <- function(e) errors <<- c(errors, list(e))
  tryCatch(
    dbWithTransaction(
      con,
      withCallingHandlers(code, error = collect_error),
      ...
    ),
    error = function(e) {
      # rethrow just the first error
      stop(errors[[1L]])
    }
  )
}
