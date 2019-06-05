

#' Drop an existing table
#'
#' The sql generation uses the function `sqlDropTable()` another S4 generic defined in this package.
#' Default sql generation is appropriate for MariaDB and MySQL.
#'
#' @param con a DBConnection oject
#' @param name a character string specifying the name of an existing DBMS table
#' @param primary_key a character vector of column names which will form the primary key
#'
#'     To specify a composite Primary Key for the `symbol` and `date` columns use
#'
#'     primary_key = c("symbol","date" )
#'
#' For a table with a Primary Key defined, each records must have a unqiue primary key. Attempts to `INSERT`
#' records with a duplicate Primary Key to an existing record in the DB will cause an ERROR
#'
#' @family DBIConnection generics
#' @export
setGeneric("dbDropTable",
           def = function(con, name, ...) standardGeneric("dbDropTable")
)

#' @rdname hidden_aliases
#' @export
setMethod("dbDropTable", signature("DBIConnection"),
          function(con, name, ...) {
            stopifnot(dbExistsTable(con, name));
            query <- sqlDropTable(con, name, ...);
            dbExecute(con,query);
          }
)

