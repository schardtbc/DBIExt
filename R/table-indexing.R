

#' Addis a Primary Key to an existing table
#'
#' `dbCreateTable` does not directly support adding primary or secondary indexes to the table being created.
#' Properly chosen indexing of tables increases query performance. For
#'
#' @param con a DBConnection oject
#' @param table a character string specifying the name of an existing DBMS table
#' @param primary_key a character vector of column names which will form the primary key
#'
#'     To specify a composite Primary Key for the `symbol` and `dat` columns use
#'
#'     primary_key = c("symbol","date" )
#'
#' For a table with a Primary Key defined, each records must have a unqiue primary key. Attempts to `INSERT`
#' records with a duplicate Primary Key to an existing record in the DB will cause an ERROR
#'
#' @family DBIConnection generics
#' @export
setGeneric("dbAddPrimaryKey",
           def = function(con, table, primary_key, ...) standardGeneric("dbAddPrimaryKey")
)

#' @rdname hidden_aliases
#' @export
setMethod("dbAddPrimaryKey", signature("DBIConnection"),
          function(con, table, primary_key, ...) {
            stopifnot(dbExistsTable(con, table));
            colsInTable <- DBI::dbListFields(con,table);
            pk <- as.character(unname(primary_key));
            stopifnot(setequal(pk,intersect(pk,colsInTable)));
            query <- sqlAlterTableWithPrimaryKey(con, table, pk = primary_key, ...);
            dbExecute(con,query);
          }
)

#' @export
setGeneric("dbAddIndex",
           def = function(con, table, index, unique = FALSE...) standardGeneric("dbAddIndex")
)

#' @rdname hidden_aliases
#' @export
setMethod("dbAddIndex", signature("DBIConnection"),
          function(con, table, index, unique = FALSE, ...) {
            query <- sqlAddIndex(con, table, index = index, unique = unique, ...);
            dbExecute(con,query);
          }
)
