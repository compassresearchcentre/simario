\name{percentages_flattened_mx}
\alias{percentages_flattened_mx}
\title{Calculated percentages within groups of a flattened matrix.}
\usage{
  percentages_flattened_mx(mx.flattened, dict)
}
\arguments{
  \item{mx.flattened}{a flattened matrix, ie: a matrix that
  has rows that contain groups of values. Percentages are
  then calculated within these groups.  The number of
  groups is determined by using the meta "grpby.tag"
  attribute to lookup the group by codings in \code{dict}.
  mx.flattened must also have a meta "varname" attribute.}

  \item{dict}{Dictionary object. Used to determine number
  of groups.}
}
\description{
  Calculated percentages within groups of a flattened
  matrix.
}
\examples{
mx.flattened <- structure(matrix(c(1,2,1,3,1,4,2,2,2,3,2,4,1,2,3,4), nrow=2, byrow = TRUE, dimnames=list(NULL, c("F 1", "F 2", "F 3", "F 4", "M 1", "M 2", "M 3", "M 4"))), meta=c(varname="disability_state", grpby.tag="sex"))
dict <- dict_example
percentages_flattened_mx(mx.flattened, dict)
}
\seealso{
  \code{\link{prop.table.mx.grped.rows}}
}

