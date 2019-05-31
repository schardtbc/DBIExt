
#' update records in a database table
#'
#' a call to `dbUpdateTable()` executes the sql to update records for a target database using the data
#' in the `data.update` data-frame. The default method provided, 4 SQL statements are generated to
#' perform the update: a call to `dbCreateTable()`; a call to `dbAlterTabledatecreates and populates a
#' temporary table using the data provided the `data.update` dataframe
#'
#' An UPDATE statement is then constructed by the dbUpdateTable() method. The generated
#'  sql is then passed to dbExecute().
#' @param con an S4 object that inherits from DBIConnection. This object is used to communicate
#'  with the database engine
#' @param target.table A character string specifying the unquoted DBMS table name, or the result
#'  of a call to dbQuoteIdentifier().
#' @param data.update a data.frame (or coercible to data.frame).
#' @param set a named character vector of varibles
#'
#'       If NULL, the default, then the function expects the
#'          data.update to be a grouped data.frame. The UPDATE statement will be constructed to match
#'          the ungrouped columns in the `data.update` dataset with identically names columns in the
#'          target database table.
#'
#'       If not NULL, then a character vector provides the names of the columns to join the database table to the
#'       dataframe. example by = c("symbol", "date"). Use a named vector by = c("col1" = "col2") to match columns
#'       with different names in the database target table and the supplied data.update dataset.
#'
#' @param by a character vector of variables to join.
#'
#'       If NULL, the default, then the function expects the
#'          data.update to be a grouped data.frame. The grouped columns will be matched with identically named columns
#'          the database table to construct the inner join clause the the update statement.
#'          A message lists the variables so that you can check they're right (to suppress
#'          the message, simply explicitly list the variables that you want to join).
#'
#'       If not NULL, then a character vector provides the names of the columns to join the database table to the
#'       dataframe. example by = c("symbol", "date"). Use a named vector by = c("col1" = "col2") to match columns
#'       with different names in the database target table and the supplied data.update dataset.
#'
#' @family DBIConnection generics
#' @export
#' @examples
#'
setGeneric("dbUpdateTable",
           def = function(con, target.table, data.update, set = NULL, by = NULL, ...) standardGeneric("dbUpdateTable")
)

#' @rdname hidden_aliases
#' @export
setMethod("dbUpdateTable", signature("DBIConnection"),
function(con, target.table, data.update, set = NULL, by = NULL) {
  stopifnot(DBI::dbExistsTable(con,target.table));
  colsInTarget <- DBI::dbListFields(con,target.table);
  colsInData <- names(data.update);
  if (is.null(set) && is.null(by))
    stopifnot(setequal(colsInData,intersect(colsInData,colsInTarget)));
  if (is.null(by)){
    by <- dplyr::groups(data.update);
    if (is.null(by)) stop("by argument or grouped data frame required as per documentation")
    target.by <- names(by);
    update.by <- unname(by);
    if (is.null(target.by)) target.by<-update.by;
  } else {
    target.by <- names(by);
    update.by <- unname(by);
    if (is.null(target.by)) target.by<-update.by;
    stopifnot(setequal(target.by,intersect(target.by,colsInTarget)),setequal(update.by,intersect(update.by,colsInData)));
  }
  if (is.null(set)){
    set <- setdiff(colsInData,update.by);
    if (is.null(set)) stop("set argument or grouped data frame required as per documentation")
    target.set <- names(set);
    target.set <- unname(set);
    if (is.null(target.set)) target.set<-update.set;
  } else {
    target.set <- names(set);
    update.set <- unname(set);
    if (is.null(target.set)) target.set<-update.set;
      stopifnot(setequal(taget.set,intersect(target.set,colsInTarget)),setequal(update.set,intersect(update.set,colsInData)))
  }
  # generate ramdon namme for temporary table
  tmp.table <- paste0(c("tmp_",letters[sample(1:26,6)]),collapse="");
  status <- DBI::dbCreateTable(con,tmp.table,data.update,temporary = TRUE);
  if (!status) {
    stop("unable to create tempory table for the update data.frame");
  }
  # add a primary key to table, this will speed up the update for most common use case
  alter_table <- sqlAlterTableWithPrimaryKey(con,tmp.table, pk = update.by );
  DBI::dbExecute(con,alter_table);
  # insert the update data into the temporary table
  DBI::dbAppendTable(con,tmp.table,data.update);
  update_statement <- sqlUpdateTable(con,target.table, tmp.table, set = set, by = by);
  tryCatch(DBI::dbExecute(con,update_statement),
           error = function(c) stop(c),
           finally = function(c) {
             query <- paste0("DROP TEMPORARY TABLE IF EXISTS ",tmp.table,";");
             DBI::dbExecute(con,query);
           }
  )

}
)


