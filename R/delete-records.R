
#' delete all records in an existing table
#'
#'
#' The default `dbDeleteAllRecords()` method performs checks that
#' the targeted table exists
#'
#' The sql generation uses the function `sqlDeleteAllRecords()` another S4 generic defined in this package.
#' Default sql generation is appropriate for MariaDB and MySQL.
#'
#' @param con a DBConnection oject
#' @param name a character string specifying the name of an existing DBMS table
#' @param key.df NULL | a dataframe of key value pairs designating records to delete
#'
#'
#' @family DBIConnection generics
#' @export
setGeneric("dbDeleteAllRecords",
           def = function(con, name, key.df = NULL, ...) standardGeneric("dbDeleteAllRecords")
)

#' @rdname hidden_aliases
#' @export
setMethod("dbDeleteAllRecords", signature("DBIConnection"),
          function(con, name, key.df = NULL, ...) {
            stopifnot(dbExistsTable(con, name));
            query <- sqlDeleteAllRecords(con, name , ...);
            dbExecute(con,query);
          }
)
