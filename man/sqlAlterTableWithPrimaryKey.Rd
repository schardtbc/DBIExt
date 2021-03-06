% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sql-generation.R
\name{sqlAlterTableWithPrimaryKey}
\alias{sqlAlterTableWithPrimaryKey}
\title{generate sql for an alter table statment adding a primary key to a table}
\usage{
sqlAlterTableWithPrimaryKey(conn, target.table, pk, ...)
}
\arguments{
\item{conn}{a DBIConnector Object}

\item{target.table}{table in database to which primary key will be added}

\item{pk}{a character vector of column names which make up the composite primary key for the table}
}
\description{
`sqlAlterTableWithPrimaryKey()` composes a single SQL statement to add a composite primary key
to a table in the database
}
\details{
suggest that this be done prior to adding any data to the table
}
\seealso{
Other SQL generation: \code{\link{sqlAddIndex}},
  \code{\link{sqlDeleteAllRecords}},
  \code{\link{sqlOnClause}},
  \code{\link{sqlReplaceInTable}},
  \code{\link{sqlSetClause}}, \code{\link{sqlUpdateTable}}
}
\concept{SQL generation}
