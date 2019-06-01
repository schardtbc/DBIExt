

#' Adds a Primary Key to an existing table
#'
#' `dbCreateTable` does not directly support adding primary or secondary indexes to the table being created.
#' Properly chosen indexing of tables increases query performance.
#'
#' The default `dbAddPrimaryKey()` method performs checks that
#' the targeted table exists and that the columns specified for the primary-key exist in the target table. the method
#' throws an error if either condition is FALSE
#'
#' The sql generation uses the function `sqlAlterTableWithPrimaryKey()` another S4 generic defined in this package.
#' Default sql generation is appropriate for MariaDB and MySQL.
#'
#' @param con a DBConnection oject
#' @param table a character string specifying the name of an existing DBMS table
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


#' Adds a Primary Key to an existing table
#'
#' `dbCreateTable` does not directly support adding primary or secondary indexes to the table being created.
#' Properly chosen indexing of tables increases query performance.
#'
#' The default `dbAddIndex()` method performs checks that
#' the targeted table exists and that the columns specified for the index exist in the target table. the method
#' throws an error if either condition is FALSE
#'
#' The sql generation uses the function `sqlAQddIndex()` another S4 generic defined in this package.
#' Default sql generation is appropriate for MariaDB and MySQL.
#'
#' @param con a DBConnection oject
#' @param table a character string specifying the name of an existing DBMS table
#' @param index a character vector of column names which will form the index
#' @param unique boolean, default = FALSE; If set to true, the constraint that all rows must have unique
#'  values for the index
#'
#'     To specify a composite Indexfor the `symbol` and `date` columns use
#'
#'     primary_key = c("symbol","date" )
#'
#' For a table with a Unique Index defined, each records must have a unqiue index value. Attempting to `INSERT`
#' records into the DB will with  duplicate values on a unique index to an existing record causes an ERROR
#'
#' @family DBIConnection generics
#' @export
setGeneric("dbAddIndex",
           def = function(con, table, index, unique = FALSE, ...) standardGeneric("dbAddIndex")
)

#' @rdname hidden_aliases
#' @export
setMethod("dbAddIndex", signature("DBIConnection"),
          function(con, table, index, unique = FALSE, ...) {
            stopifnot(dbExistsTable(con, table));
            colsInTable <- DBI::dbListFields(con,table);
            idx <- as.character(unname(index));
            stopifnot(setequal(idx,intersect(idx,colsInTable)));
            query <- sqlAddIndex(con, table, index = index, unique = unique, ...);
            dbExecute(con,query);
          }
)
