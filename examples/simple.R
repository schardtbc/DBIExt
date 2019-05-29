# test dbUpdateTable
library(DBI)
library(DBIExt)

config <- list(
    HOST = "localhost",
    DATABASE = "mktdata000",
    USER = "test",
    PASSWORD = "test");

#' get the main database connection
getDBConnection <- function(){
  connection <- DBI::dbConnect(RMariaDB::MariaDB(), host = config[["HOST"]],
                               user = config[["USER"]], password = config[["PASSWORD"]], dbname = config[["DATABASE"]])
}

#' close the database connection
closeDBConnection <- function(connection) {
  DBI::dbDisconnect(connection)
}


cid <- 1:26;
cn  <- 100:125;
clc  <- letters[cid];
cuc  <- LETTERS[cid];

target.df <- tibble::as_tibble(list(id = cid, cn = cn, cc = clc));
update.df <- tibble::as_tibble(list(id = cid, cc = cuc));
update.df <- dplyr::group_by(update.df,id)

db <- getDBConnection();
DBI::dbWriteTable(db,"T123",target.df,overwrite = TRUE);

original <- dbReadTable(db, "T123")
original

update.df

dbUpdateTable(db,"T123",update.df);

updated <- dbReadTable(db,"T123");
updated

