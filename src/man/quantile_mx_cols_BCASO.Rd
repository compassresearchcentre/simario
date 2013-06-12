\name{quantile_mx_cols_BCASO}
\alias{quantile_mx_cols_BCASO}
\title{Execute quantile on the columns of a matrix by a grouping variable.
Only used for user-specified subgroups.
Extension of quantile_mx_cols to handle a time-variant grouing variable.
grpby can be a vector or a matrix with columns representing the value of the
grpby variable in different years.
Does not have ability to include weights or logisets.
The group-by variable may be time-invariant or time-variant.
Not sure if can handle no grouping.}
\usage{
  quantile_mx_cols_BCASO(mx, grpby = NULL,
    grpby.tag = NULL, new.names = NULL,
    probs = c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 1),
    logiset = NULL, dict = dict, ...)
}
\arguments{
  \item{mx}{matrix}

  \item{grpby}{a vector or matrix indicating the group to
  whic the unit belongs. Quantiles will be calculated
  separately for each group. If the group-by variable is
  time-invariant grpby can be provided as a vector or as a
  matrix with every column the same.  If the group-by
  variable is time-variable then grpby should be a matrix
  with number of columns equal to number of years.}

  \item{grpby.tag}{string. grouping variable names}

  \item{new.names}{if specified, the names of the result
  will be set to this vector}

  \item{...}{additional arguments to pass to
  \code{\link{quantile}} e.g. na.rm=T}
}
\value{
  quantile of each column returned as a row. Quantiles for
  each group are put side-by-side in a rbind fashion.
}
\description{
  Execute quantile on the columns of a matrix by a grouping
  variable. Only used for user-specified subgroups.
  Extension of quantile_mx_cols to handle a time-variant
  grouing variable. grpby can be a vector or a matrix with
  columns representing the value of the grpby variable in
  different years. Does not have ability to include weights
  or logisets. The group-by variable may be time-invariant
  or time-variant. Not sure if can handle no grouping.
}

