#' Compose an on clause for a table join
#'
#' `sqlOnClause()` generates a single SQL string that forms the on clause
#' for joining two tables.
#' The default methods are MariaDB, MySQL compliant.
#' These methods are mostly useful for backend implementers.
#'
#'
#' @param x,y names of tbls to join. These will be quoted with dbQuoteIdentifier
#' @param by a character vector of columns to join by.  If `NULL`, the
#'   default, `*_join()` will do a natural join, using all variables with
#'   common names across the two tables. A message lists the variables so
#'   that you can check they're right (to suppress the message, simply
#'   explicitly list the variables that you want to join).
#'
#' @include hidden.R

#'   To join by different variables on x and y use a named vector.
#'   For example, `by = c("a" = "b")` will match `x.a` to
#'   `y.b`.
#'   Character vectors will be escaped with [dbQuoteIdenitifer()].
#' @family SQL generation
#' @export
#' @examples
#'
setGeneric("sqlOnClause",
           def = function(con, x, y, by,...) standardGeneric("sqlOnClause")
)

#' @rdname hidden_aliases
#' @export
setMethod("sqlOnClause", signature("DBIConnection"),
          function(con, x, y, by) {
            t1 <- DBI::dbQuoteIdentifier(con, x)
            t2 <- DBI::dbQuoteIdentifier(con, y)
            t1 = paste0(t1, ".")
            t2 = paste0(t2, ".")
            target <- names(by)
            if (is.null(target))
              target <- by

            matches <- unname(by)

            idx <- sapply(target, function(x) `==`(x, ""))

            target[idx] <- matches[idx]
            target <- as.character(target)
            target <-
              sapply(target, function(x)
                DBI::dbQuoteIdentifier(con, x))

            matches <- as.character(matches)
            matches <-
              sapply(matches, function(x)
                DBI::dbQuoteIdentifier(con, x))

            targets <- paste0(t1, target)
            matched <- paste0(t2, matches)
            tmp <-
              purrr::map2(targets, matched, function(x, y)
                paste(x, y, sep = " = "))

            tmp <- do.call(paste, c(tmp, sep = " AND "))
            DBI::SQL(paste("ON", tmp))

          }
)


#' Compose an on clause for a table join
#'
#' `sqlSetClause()` generates a single SQL string that forms the set clause
#' for an SQL Update statment two tables.
#' The default methods are MariaDB, MySQL compliant.
#' These methods are mostly useful for backend implementers.
#'
#'
#' @param x,y names of tbls to join. These will be quoted with dbQuoteIdentifier
#' @param set a character vector of columns to match .
#'
#'   To set  different variables on x and y use a named vector.
#'   For example, `by = c("a" = "b")` will translate to
#'   SET  `x.a` = `y.b`.
#'
#'   Character vectors will be escaped with [dbQuoteIdenitifer()].
#' @family SQL generation
#' @export
#' @examples
#'
setGeneric("sqlSetClause",
           def = function(con, x,y, set,...) standardGeneric("sqlSetClause")
)
#' generate SET caluses for an UPDATE statement
#' @rdname hidden_aliases
#' @export
setMethod("sqlSetClause", signature("DBIConnection"),
          function(con, x, y, set) {
            t1 <- DBI::dbQuoteIdentifier(con, x)
            t2 <- DBI::dbQuoteIdentifier(con, y)
            t1 = paste0(t1, ".")
            t2 = paste0(t2, ".")
            target <- names(set)
            if (is.null(target)) target <- set

            matches <- unname(set)

            idx <- sapply(target, function(x)`==`(x, ""))
            target[idx] <- matches[idx]

            target <- as.character(target)

            target <-
              sapply(target, function(x)
                DBI::dbQuoteIdentifier(con, x))

            matches <- as.character(matches)

            matches <-
              sapply(matches, function(x)
                DBI::dbQuoteIdentifier(con, x))

            targets <- paste0(t1, target)

            matched <- paste0(t2, matches)

            tmp <-
              purrr::map2(targets, matched, function(x, y)
                paste(x, y, sep = " = "))

            tmp <- paste("SET", tmp)
            tmp <- paste(tmp, collapse = ",\n")
            DBI::SQL(tmp)

          })

#' Compose an on clause for a table join
#'
#' `sqlUpdateTable()` generates a single SQL string that forms the an UPDATE statement
#' for UPDATE join between two tables.
#'
#' The default method is MariaDB, MySQL compliant.
#' These methods are mostly useful for backend implementers.
#'
#'
#' @param x,y names of tbls to join. These will be quoted with dbQuoteIdentifier
#' @param set a character vector of columns to match .
#' @param by a character vector of columns to join by.
#'
#' @family SQL generation
#' @export
#' @examples
#'
setGeneric("sqlUpdateTable",
           def = function(con, x,y, set, by,...) standardGeneric("sqlUpdateTable")
)
#' generate UPDATE statement
#' @rdname hidden_aliases
#' @export
setMethod("sqlUpdateTable", signature("DBIConnection"),
          function(con, x, y, set = NULL, by = NULL) {
            qtable1 <- DBI::dbQuoteIdentifier(con, x)
            qtable2 <- DBI::dbQuoteIdentifier(con, y)

            DBI::SQL(
              paste0(
                "UPDATE ",
                qtable1,
                "\nINNER JOIN ",
                qtable2,
                "\n",
                sqlOnClause(con, qtable1, qtable2, by),
                "\n",
                sqlSetClause(con, qtable1, qtable2, set)
              )
            )
          })



#' generate sql for an alter table statment adding a primary key to a table
#'
#' `sqlAlterTableWithPrimaryKey()` composes a single SQL statement to add a composite primary key
#' to a table in the database
#'
#' suggest that this be done prior to adding any data to the table
#'
#' @param conn a DBIConnector Object
#' @param target.table table in database to which primary key will be added
#' @param pk a character vector of column names which make up the composite primary key for the table
#'
#' @family SQL generation
#' @export
setGeneric("sqlAlterTableWithPrimaryKey",
          def = function(conn, target.table, pk, ...) standardGeneric("sqlAlterTableWithPrimaryKey")
)

#' generate sql for an alter table statment adding a primary key to a table
#' @rdname hidden_aliases
#' @export
setMethod("sqlAlterTableWithPrimaryKey", signature("DBIConnection"),
          function(conn, target.table, pk) {
            target.table.q <- DBI::dbQuoteIdentifier(conn, target.table)

            pk.q <-
              sapply(pk, function(x) {
                DBI::dbQuoteIdentifier(conn, as.character(x))
              })

            sql_alter_table <- DBI::SQL(paste0(
              "ALTER TABLE ",
              target.table.q,
              "\n",
              "ADD PRIMARY KEY (",
              paste0(pk.q, collapse = ", "),
              ");"
            ))
          })



#' generate sql for an alter table statment adding an index to an existing DBMS table
#'
#' suggest that this be done prior to adding any data to the table.
#'
#' `sqlAddIndex()` composes a single SQL statement to add a composite primary key
#' to a table in the database
#'
#' @param conn a DBIConnector Object
#' @param table table in database to which primary key will be added
#' @param index a character vector of column names which make up the index for the table
#' @param unqiue boolean specifying if index is unique (a rows must have a unique value)
#'
#' @family SQL generation
#' @export
setGeneric("sqlAddIndex",
           def = function(conn, table, index, unique = FALSE, ...) standardGeneric("sqlAddIndex")
          )

#' generate sql for an alter table statment adding a primary key to a table
#' @rdname hidden_aliases
#' @export
setMethod("sqlAddIndex", signature("DBIConnection"),
          function(conn, table, index, unique = FALSE) {
            table.q <- DBI::dbQuoteIdentifier(conn, table)

            index.q <-
              sapply(index, function(x) {
                DBI::dbQuoteIdentifier(conn, as.character(x))
              })
            idx.name <- paste0(c("idx_",letters[sample(1:26,10)]),collapse="");
            sql_alter_table <- DBI::SQL(paste0(
              "ALTER TABLE ",
              table.q,
              "\n",
              "ADD ", if (unique) "UNIQUE ", "INDEX ",idx.name,  " (",
              paste0(index.q, collapse = ", "),
              ");"
            ))
          })
