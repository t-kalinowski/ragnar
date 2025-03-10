
#' Create and connect to a vector store
#'
#' @param location filepath, or `:memory:`
#' @param embed A function that is called with a character vector and returns a
#'   matrix of embeddings. Note this function will be serialized and then
#'   deserialized in new R sessions, so it cannot reference to any objects in
#'   the global or parent environments. Make sure to namespace all function
#'   calls with `::`. If additional R objects must be available in the function,
#'   you can optionally supply a `carrier::crate()` with packaged data.
#' @param embedding_size integer
#' @param overwrite logical, what to do if `location` already exists
#'
#' @returns a `DuckDBRagnarStore` object
#' @export
ragnar_store_create <- function(
    location = ":memory:",
    embed = embed_ollama(),
    embedding_size = ncol(embed("foo")),
    overwrite = FALSE
) {

  if (any(file.exists(c(location, location.wal <- paste0(location, ".wal"))))) {
    if (overwrite) {
      unlink(c(location, location.wal), force = TRUE)
    } else {
      stop("File already exists: ", location)
    }
  }
  con <- dbConnect(duckdb::duckdb(), dbdir = location)

  check_number_whole(embedding_size, min = 1)
  embedding_size <- as.integer(embedding_size)

  if(!inherits(embed, "crate")) {
    environment(embed) <- baseenv()
    embed <- rlang::zap_srcref(embed)
  }

  metadata <- tibble::tibble(
    embedding_size,
    embed_func = blob::blob(serialize(embed, NULL)),
  )
  if (overwrite)
    dbExecute(con, glue::trim("
      DROP TABLE IF EXISTS metadata;
      DROP TABLE IF EXISTS chunks;
      DROP SEQUENCE IF EXISTS id_sequence;
      "))

  dbWriteTable(con, "metadata", metadata)

  # duckdb R interface does not support array columns yet,
  # so we hand-write the sql.
  dbExecute(con, glue("
    CREATE SEQUENCE id_sequence START 1;
    CREATE TABLE chunks (
      id INTEGER DEFAULT nextval('id_sequence'),
      embedding FLOAT[{embedding_size}],
      text VARCHAR
    )"))

  # read back in embed, so any problems with an R function that doesn't serialize
  # correctly flush out early.
  metadata <- dbReadTable(con, "metadata")
  embed <- unserialize(metadata$embed_func[[1L]])

  DuckDBRagnarStore(embed = embed, .con = con)
}


#' Connect to `RagnarStore`
#'
#' @param location string, a filepath location.
#' @param ... unused; must be empty.
#' @param read_only logical, whether the returned connection can be used to
#'   modify the store.
#' @param build_index logical, whether to call `ragnar_store_build_index()` when
#'   creating the connection
#'
#' @returns a `RagnarStore` object.
#' @export
#'
#' @rdname rangar_store_create
ragnar_store_connect <- function(location = ":memory:",
                                 ...,
                                 read_only = FALSE,
                                 build_index = FALSE) {

  check_dots_empty()
  # mode = c("retrieve", "insert")
  # mode <- match.arg(mode)
  # read_only <- mode == "retrieve"

  con <- dbConnect(duckdb::duckdb(), dbdir = location, read_only = read_only)

  # can't use dbExistsTable() because internally it runs:
  # > dbGetQuery(conn, sqlInterpolate(conn, "SELECT * FROM ? WHERE FALSE", dbQuoteIdentifier(conn, name)))
  # which fails with:
  # > Error in dbSendQuery(conn, statement, ...) :
  # >  rapi_prepare: Unknown column type for prepare: FLOAT[384]
  if (!all(c("chunks", "metadata") %in% dbListTables(con))) {
    stop("Store must be created with ragnar_store_create()")
  }
  dbExecute(con, "LOAD fts; LOAD vss;")

  metadata <- dbReadTable(con, "metadata")
  embed <- unserialize(metadata$embed_func[[1L]])

  if (build_index)
    ragnar_store_build_index(con)

  DuckDBRagnarStore(embed = embed, .con = con)
}



#' Insert chunks into a `RagnarStore`
#'
#' @param store a `RagnarStore` object
#' @param chunks a character vector or a dataframe with a `text` column, and
#'   optionally, a pre-computed `embedding` matrix column. If `embedding` is not
#'   present, then `store@embed()` is used. `chunks` can also be a character
#'   vector.
#'
#' @returns `store`, invisibly.
#' @export
ragnar_store_insert <- function(store, chunks) {

  # ?? swap arg order? piping in df will be more common...
  # -- can do df |> ragnar_store_insert(store = store)
  if (!S7_inherits(store, RagnarStore)) {
    stop("store must be a RagnarStore")
  }

  if(is.character(chunks))
    chunks <- data_frame(text = chunks)

  stopifnot(
    is.data.frame(chunks),
    is.character(chunks$text)
  )

  if (!nrow(chunks)) {
    # warning("ragnar_store_insert() called empty `chunks`")
    return(invisible(store))
  }

  if (!"embedding" %in% names(chunks))
    chunks$embedding <- store@embed(chunks$text)

  stopifnot(
    is.matrix(chunks$embedding)
    # ncol(df$embedding) == store@embedding_size
  )

  # duckdb-r does not support array columns yet.
  # hand-write the SQL for now
  # hopefully replace all this with a DBI::dbAppendTable() once
  # https://github.com/duckdb/duckdb-r/issues/102 is resolved.
  # TODO: insert in batches?
  rows <- sprintf(
    "(array_value(%s), %s)",
    chunks$embedding |> asplit(1) |> map_chr(stri_flatten, ", "),
    DBI::dbQuoteString(store@.con, chunks$text)
  ) |> paste0(collapse = ",\n")

  stmt <- sprintf("INSERT INTO chunks (embedding, text) VALUES \n%s;", rows)
  dbExecute(store@.con, stmt)
  invisible(store)
}



#' Build a Ragnar Store index
#'
#' A search index must be built before calling `ragnar_retrieve()`. If
#' additional entries are added to the store with `ragnar_store_insert()`,
#' `ragnar_store_build_index()` must be called again to rebuild the index.
#'
#' @param store a `RagnarStore` object
#' @param type The retrieval search type to build an index for.
#'
#' @returns `store`, invisibly.
#' @export
ragnar_store_build_index <- function(store, type = c("vss", "fts")) {

  if(S7_inherits(store, DuckDBRagnarStore))
    con <- store@.con
  else if (methods::is(store, "DBIConnection"))
    con <- store
  else
    stop("`store` must be a RagnarStore")

  if ("vss" %in% type) {
    # TODO: duckdb has support for three different distance metrics that can be
    # selected when building the index: l2sq, cosine, and ip. Expose these as options
    # in the R interface. https://duckdb.org/docs/extensions/vss.html#usage
    dbExecute(con, "INSTALL vss;")
    dbExecute(con, "LOAD vss;")
    dbExecute(con, paste(
      "SET hnsw_enable_experimental_persistence = true;",
      "DROP INDEX IF EXISTS my_hnsw_index;",
      "CREATE INDEX my_hnsw_index ON chunks USING HNSW (embedding);"
    ))
  }

  if ("fts" %in% type) {
    dbExecute(con, "INSTALL fts;")
    dbExecute(con, "LOAD fts;")
    # fts index builder takes many options, e.g., stemmer, stopwords, etc.
    # Expose a way to pass along args. https://duckdb.org/docs/extensions/full_text_search.html
    dbExecute(con, "PRAGMA create_fts_index('chunks', 'id', 'text', overwrite = 1);")
  }

  invisible(store)
}

# @export
RagnarStore <- new_class(
  "RagnarStore",
  properties = list(
    embed = class_function
  ),
  abstract = TRUE
)

DuckDBRagnarStore <- new_class(
  "DuckDBRagnarStore",
  RagnarStore,
  properties = list(
    .con = methods::getClass("DBIConnection")
  )
)
