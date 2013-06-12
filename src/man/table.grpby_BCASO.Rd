\name{table.grpby_BCASO}
\alias{table.grpby_BCASO}
\title{Frequency table, with option to group results.
Extension of table.grpby() that can handle grpby as a matrix as well as a vector.
The group-by variable may be time-invariant or time-variant.}
\usage{
  table.grpby_BCASO(x, grpby = NULL, wgts = NULL)
}
\arguments{
  \item{x}{vector of values which can be interpreted as
  factors}

  \item{grpby}{A vector or matrix of elements to group by,
  or NULL to do no grouping.  vector must be same length as
  the columns of x or matrix must have the same number of
  rows as z. If the group-by variable is time-invariant
  grpby can be provided as a vector or as a matrix with
  every column the same.  If the group-by variable is
  time-variable then grpby should be a matrix with number
  of columns equal to number of years.}

  \item{useNA}{whether to include extra NA levels in the
  table.  one of c("no", "ifany", "always"). Defaults to
  "ifany".}
}
\value{
  a table. If grpby is specified this will be a table with
  columns that are the group by and rows the categories.
  If grpby = NULL then a table with 1 column and rows as
  categories is returned.
}
\description{
  Frequency table, with option to group results. Extension
  of table.grpby() that can handle grpby as a matrix as
  well as a vector. The group-by variable may be
  time-invariant or time-variant.
}

