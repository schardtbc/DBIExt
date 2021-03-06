% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sql-generation.R
\name{sqlOnClause}
\alias{sqlOnClause}
\title{Compose an on clause for a table join}
\usage{
sqlOnClause(con, x, y, by, ...)
}
\arguments{
\item{x, y}{names of tbls to join. These will be quoted with dbQuoteIdentifier}

\item{by}{a character vector of columns to join by.  If `NULL`, the
default, `*_join()` will do a natural join, using all variables with
common names across the two tables. A message lists the variables so
that you can check they're right (to suppress the message, simply
explicitly list the variables that you want to join).}
}
\description{
`sqlOnClause()` generates a single SQL string that forms the on clause
for joining two tables.
The default methods are MariaDB, MySQL compliant.
These methods are mostly useful for backend implementers.
}
\examples{

}
\seealso{
Other SQL generation: \code{\link{sqlAddIndex}},
  \code{\link{sqlAlterTableWithPrimaryKey}},
  \code{\link{sqlDeleteAllRecords}},
  \code{\link{sqlReplaceInTable}},
  \code{\link{sqlSetClause}}, \code{\link{sqlUpdateTable}}
}
\concept{SQL generation}
