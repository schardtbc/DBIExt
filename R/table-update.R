
#' update records in a database table
#'
#' records in the target table in the database will be updated by the data provided in
#' the `data.update` data-frame. The default method provided, creates and populates a
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
#' @param by a character vector of variables to join. If NULL, the default, *_join() will
#'           do a natural join, using all variables with common names across the two tables.
#'          A message lists the variables so that you can check they're right (to suppress
#'          the message, simply explicitly list the variables that you want to join).
#'
#' To join by different variables on x and y use a named vector. For example, by = c("a" = "b") will match x.a to y.b.
#' @family DBIConnection generics
#' @export
#' @examples
#'
setGeneric("dbUpdateTable",
           def = function(con, target.table, data.update, set, by, ...) standardGeneric("dbUpdateTable")
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
    stopifnot(setequal(target.by,intersect(taget.by,colsInTarget)),setequal(update.by,intersect(update.by,colsInData)));
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
  alter_table <- sqlAlterTableWithPrimaryKey(con,tmp.table, pk = t1.by );
  DBI::dbExecute(con,alter_table);
  # insert the update data into the temporary table
  DBI::dbAppendTable(con,tmp.table,data.update);
  update_statement <- sqlUpdateInTable(con,target.table, tmp.table, set = set, by = by);
  tryCatch(DBI::dbExecute(con,update_statement),
           error = function(c) stop(c),
           finally = function(c) {
             query <- paste0("DROP TEMPORARY TABLE IF EXISTS ",tmp.table,";");
             DBI::dbExecute(con,query);
           }
  )

}
)


