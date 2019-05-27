
#' Compose query to update rows in a table
#'
#' `sqlReplaceInTable()` generates a single SQL string that inserts a
#' data frame into an existing table. `sqlReplaceTableTemplate()` generates
#' a template suitable for use with [dbBind()].
#' The default methods are MariaDB, MySQL compliant.
#' These methods are mostly useful for backend implementers.
#'
#' The `row.names` argument must be passed explicitly in order to avoid
#' a compatibility warning.  The default will be changed in a later release.
#'
#' @inheritParams sqlCreateTable
#' @inheritParams rownames
#' @param values A data frame. Factors will be converted to character vectors.
#'   Character vectors will be escaped with [dbQuoteString()].
#' @family SQL generation
#' @export
#' @examples
#' sqlReplaceInTable(ANSI(), "iris", head(iris))
#'
#' sqlReplaceInTable(ANSI(), "mtcars", head(mtcars))
#' sqlReplaceTable(ANSI(), "mtcars", head(mtcars), row.names = FALSE)
setGeneric("sqlReplaceInTable",
           def = function(con, table, values, row.names = NA, ...) standardGeneric("sqlReplaceInTable")
)

#' @rdname hidden_aliases
#' @export
setMethod("sqlReplaceInTable", signature("DBIConnection"),
          function(con, table, values, row.names = NA, ...) {
            stopifnot(is.list(values))

            if (missing(row.names)) {
              warning("Do not rely on the default value of the row.names argument for sqlReplaceInTable(), it will change in the future.",
                      call. = FALSE
              )
            }

            sql_values <- sqlData(con, values, row.names)
            table <- dbQuoteIdentifier(con, table)
            fields <- dbQuoteIdentifier(con, names(sql_values))

            # Convert fields into a character matrix
            rows <- do.call(paste, c(sql_values, sep = ", "))
            SQL(paste0(
              "REPLACE INTO ", table, "\n",
              "  (", paste(fields, collapse = ", "), ")\n",
              "VALUES\n",
              paste0("  (", rows, ")", collapse = ",\n")
            ))
          }
)

#' @rdname sqlReplaceInTable
#' @inheritParams sqlCreateTable
#' @inheritParams rownames
#' @param prefix Parameter prefix to use for placeholders.
#' @param pattern Parameter pattern to use for placeholders:
#' - `""`: no pattern
#' - `"1"`: position
#' - anything else: field name
#' @export
#' @examples
#' sqlAppendTableTemplate(ANSI(), "iris", iris)
#'
#' sqlAppendTableTemplate(ANSI(), "mtcars", mtcars)
#' sqlAppendTableTemplate(ANSI(), "mtcars", mtcars, row.names = FALSE)
sqlReplaceInTableTemplate <- function(con, table, values, row.names = NA, prefix = "?", ..., pattern = "") {
  if (missing(row.names)) {
    warning("Do not rely on the default value of the row.names argument for sqlAppendTableTemplate(), it will change in the future.",
            call. = FALSE
    )
  }

  table <- dbQuoteIdentifier(con, table)

  values <- sqlRownamesToColumn(values[0, , drop = FALSE], row.names)
  fields <- dbQuoteIdentifier(con, names(values))

  if (pattern == "") {
    suffix <- rep("", length(fields))
  } else if (pattern == "1") {
    suffix <- as.character(seq_along(fields))
  } else {
    suffix <- names(fields)
  }

  placeholders <- lapply(paste0(prefix, suffix), SQL)
  names(placeholders) <- names(values)

  sqlReplaceInTable(
    con = con,
    table = table,
    values = placeholders,
    row.names = row.names
  )
}


#' insert of replacement rows into a table
#'
#' The `dbReplaceInTable()` method assumes that the table has been created
#' beforehand, e.g. with [dbCreateTable()].
#' The default implementation calls [sqlReplaceInTableTemplate()] and then
#' [dbExecute()] with the `param` argument. Backends compliant to
#' ANSI SQL 99 which use `?` as a placeholder for prepard queries don't need
#' to override it. Backends with a different SQL syntax which use `?`
#' as a placeholder for prepared queries can override [sqlReplaceInTable()].
#' Other backends (with different placeholders or with entirely different
#' ways to create tables) need to override the `dbReplaceInTable()` method.
#'
#' The `row.names` argument is not supported by this method.
#' Process the values with [sqlRownamesToColumn()] before calling this method.
#'
#' @param name Name of the table, escaped with [dbQuoteIdentifier()].
#' @param value A data frame of values. The column names must be consistent
#'   with those in the target table in the database.
#' @param row.names Must be `NULL`.
#' @inheritParams sqlReplaceInTableTemplate
#' @inheritParams dbDisconnect
#' @family DBIConnection generics
#' @export
#' @examples
#' con <- dbConnect(RSQLite::SQLite(), ":memory:")
#' dbCreateTable(con, "iris", iris)
#' dbUpdateTable(con, "iris", iris)
#' dbReadTable(con, "iris")
#' dbDisconnect(con)
setGeneric("dbReplaceInTable",
           def = function(conn, name, value, ..., row.names = NULL) standardGeneric("dbReplaceInTable")
);

#' @rdname hidden_aliases
#' @export
setMethod("dbReplaceInTable", signature("DBIConnection"),
          function(conn, name, value, ..., row.names = NULL) {
            stopifnot(is.null(row.names))

            query <- sqlReplaceInTableTemplate(
              con = conn,
              table = name,
              values = value,
              row.names = row.names,
              prefix = "?",
              pattern = "",
              ...
            )
            dbExecute(conn, query, params = unname(as.list(value)))
          }
)
