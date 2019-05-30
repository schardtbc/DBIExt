An Extentsion Package for DBI. Implementation of new generic function
dbUpdateTable

Installation
------------

``` r
devtools::install_github("schardtbc/DBIExt")
```

Introduction
------------

### S4 generic dbUpdateTable()

This DBI Extention package introduces a new S4 generic `dbUpdateTable`.
Updating tables is not the focus of the DBI package as pure analysis
scripts treat a database tables as read-only.

In other domains, database maintainence requires updating information in
rows of an existing table. Dropping the table and recreating it as is
done by dbWriteTable with the `overwrite` argument set to `TRUE` is not
an optimal solution for many applications.

No single generic update function could cover the diverse variety of
possible UPDATE queries. The `dbUpdateTable` S4 generic introduced in
this package performs a specific type of DBMS UPDATE operation using the
update join syntax. An example of this syntax is shown in the sql
fragment shown below. This syntax is used by MariaDB and MySQL.

``` sql
UPDATE T1
INNER JOIN T2 ON T1.C1 = T2.C1 [AND T1.C2 = T2.C2]
SET T1.CX = T2.CX,
    T1.CY = T2.CY;
```

reference: [tutorial on UPDATE join syntax for
MySQL](http://www.mysqltutorial.org/mysql-update-join/)

Note that no `WHERE` clause is specified for the query. This by design;
the reasoning for which will be explained later in the document.

For `dbUpdateTable()` the table `T1` must be a pre-existing table in the
DBMS. Table `T2` will be created as a temporary table in the DBMS using
a dataset supplied as a function argument. This dataset will usually be
a tibble, a data.frame, or another object coercible to a data.frame.

the `dbUpdateTable` function call has two additional arguements `set`
and `by` used for constructing the `SET` and `ON` clauses of the UPDATE
query.

``` r
dbUpdateTable(con, "T1", T2, set = c("CX","CY"), by = c("C1","C2"))
```

### SQL Helper Functions

There is a helper function sqlUpdateTable which generates the UPDATE sql
but does not execute it. Helper functions are also provided to generate
the sql for the `ON` clause of the join as well as the multiples SET
clauses of the `UPDATE`. These are `sqlOnClause()` and `sqlSetClause()`.

``` r
T2 <- tibble::as_tibble(list(C1=1:26,C2=LETTERS,CX=0,CY=1))
sqlUpdateTable(con, "T1", T2, set = c("CX","CY"), by = c("C1","C2"))
sqlOnClause(con,"T1","T2",by = c("C1","C2"));
sqlSetClause(con,"T1","T2",set = c("CX","CY"));
```

### Table Indexing

It is best practice that the target DBMS table be indexed, with the
indexed columns matching the columns specified by the `by` argument. The
temporary table used store the update dataset is indexed by the columns
in the `by` argument as part of the `dbUpdateTable()` implementation.
The `UPDATE-JOIN` operation with indexed table is generally quite fast.

### Why no `WHERE` caluse?

Update queries can use a `WHERE` clause to limit the scope of the
update. However in an `UPDATE-JOIN` the scope of the query is alreadly
limited by the `ON` clause of the join. This combined with the power of
`R` to precisely form the update dataset makes use of a `WHERE` clause
redundent, IMHO.

A (very) Simple Example
-----------------------

### Connection to local test database

``` r
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

db <- getDBConnection();
db
#> <MariaDBConnection>
#>   Host:    localhost
#>   Server:  8.0.13
#>   Client:  8.0.13
```

### Setup a DBMS table

``` r

cid <- 1:26;
cn  <- 100:125;
clc  <- letters[cid];


target.df <- tibble::as_tibble(list(id = cid, cn = cn, cc = clc));
DBI::dbWriteTable(db,"T123",target.df,overwrite = TRUE);

original <- dbReadTable(db, "T123")
original
#>    id  cn cc
#> 1   1 100  a
#> 2   2 101  b
#> 3   3 102  c
#> 4   4 103  d
#> 5   5 104  e
#> 6   6 105  f
#> 7   7 106  g
#> 8   8 107  h
#> 9   9 108  i
#> 10 10 109  j
#> 11 11 110  k
#> 12 12 111  l
#> 13 13 112  m
#> 14 14 113  n
#> 15 15 114  o
#> 16 16 115  p
#> 17 17 116  q
#> 18 18 117  r
#> 19 19 118  s
#> 20 20 119  t
#> 21 21 120  u
#> 22 22 121  v
#> 23 23 122  w
#> 24 24 123  x
#> 25 25 124  y
#> 26 26 125  z
```

### Setup a data.frame for the update data

``` r
cuc  <- LETTERS[cid];
update.df <- tibble::as_tibble(list(id = cid, cc = cuc));
update.df <- dplyr::group_by(update.df,id)

update.df
#> # A tibble: 26 x 2
#> # Groups:   id [26]
#>       id cc   
#>    <int> <chr>
#>  1     1 A    
#>  2     2 B    
#>  3     3 C    
#>  4     4 D    
#>  5     5 E    
#>  6     6 F    
#>  7     7 G    
#>  8     8 H    
#>  9     9 I    
#> 10    10 J    
#> # … with 16 more rows
```

### Perform the Update In The DBMS

``` r
res <- dbUpdateTable(db,"T123",update.df);
print(glue::glue("number of records modified in DBMS {res}"))
#> number of records modified in DBMS 26

updated <- dbReadTable(db,"T123");
closeDBConnection(db);

updated
#>    id  cn cc
#> 1   1 100  A
#> 2   2 101  B
#> 3   3 102  C
#> 4   4 103  D
#> 5   5 104  E
#> 6   6 105  F
#> 7   7 106  G
#> 8   8 107  H
#> 9   9 108  I
#> 10 10 109  J
#> 11 11 110  K
#> 12 12 111  L
#> 13 13 112  M
#> 14 14 113  N
#> 15 15 114  O
#> 16 16 115  P
#> 17 17 116  Q
#> 18 18 117  R
#> 19 19 118  S
#> 20 20 119  T
#> 21 21 120  U
#> 22 22 121  V
#> 23 23 122  W
#> 24 24 123  X
#> 25 25 124  Y
#> 26 26 125  Z
```
