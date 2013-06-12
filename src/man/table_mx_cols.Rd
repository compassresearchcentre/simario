\name{table_mx_cols}
\alias{table_mx_cols}
\title{Generates a frequency table, with option to group by, for each column of a matrix.
The group-by variable must be time-invariant.}
\usage{
  table_mx_cols(mx, grpby = NULL, grpby.tag = NULL,
    logiset = NULL, useNA = "ifany")
}
\arguments{
  \item{mx}{matrix, or dataframe}

  \item{grpby}{a vector of elements to group by, or NULL or
  unspecified to do no grouping.  Same length as the
  columns of mx.}

  \item{grpby.tag}{a character vector. If specified this
  value with be attached as the meta attribute "grpby.tag"}

  \item{logiset}{logical vector indicating which rows to
  include, or NULL to include all.}

  \item{useNA}{whether to include extra NA levels in the
  table.  one of c("no", "ifany", "always"). Defaults to
  "ifany".}
}
\value{
  list. Each element of the list is a frequency table for a
  column in mx.  If grpby is specified this will be a table
  with columns that are the group by and rows the
  categories.  If grpby = NULL then a table with 1 column
  and rows as categories is returned.
}
\description{
  Generates a frequency table, with option to group by, for
  each column of a matrix. The group-by variable must be
  time-invariant.
}
\examples{
mx <- matrix(c(8,2,2,2,8,2,3,2,3,2,2,4,8,2,3,4,2,2,4,3),nrow=4,ncol=5,dimnames=list(NULL, LETTERS[1:5]))
grpby <- c('M','F','F','M')
table_mx_cols(mx)
table_mx_cols(mx, grpby)
logiset <- c(FALSE, TRUE, FALSE, TRUE)
table_mx_cols(mx, grpby = grpby, logiset = logiset)
}

