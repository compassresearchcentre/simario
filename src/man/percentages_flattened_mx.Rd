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
  attribute to lookup the group by codings in \code{dict}.}

  \item{dict}{Dictionary object. Used to determine number
  of groups.}
}
\description{
  Calculated percentages within groups of a flattened
  matrix.
}
\examples{
\dontrun{
mx.flattened <- structure(matrix(c(1,2,1,3,1,4,2,2,2,3,2,4,1,2,3,4), nrow=2, byrow = TRUE, dimnames=list(NULL, c("Female 1", "Female 2", "Female 3", "Female 4", "Male 1", "Male 2", "Male 3", "Male 4"))), meta=c(varname="disability_state", grpby.tag="sexLvl1"))
dict <- dict_demo
percentages_flattened_mx(mx.flattened, dict)

mx.flattened <- structure(matrix(c(1,2,1,3,1,4,2,2,2,3,2,4,1,2,3,4), nrow=2, byrow = TRUE, dimnames=list(NULL, c("65-69 1", "65-69 2", "65-69  3", "65-69  4", "70-74  1", "70-74 2", "70-74 3", "70-74 4"))), meta=c(varname="disability_state", grpby.tag="age_grp_output"))
percentages_flattened_mx(mx.flattened, dict)
}
mx.flattened <- structure(matrix(c(1,2,1,3,1,4,2,2,2,3,2,4,1,2,3,4), nrow=2, byrow = TRUE, dimnames=list(NULL, c("Female 1", "Female 2", "Female 3", "Female 4", "Male 1", "Male 2", "Male 3", "Male 4"))), meta=c(varname="disability_state", grpby.tag="sex"))
dict <- dict_example
percentages_flattened_mx(mx.flattened, dict)
}
\seealso{
  \code{\link{prop.table.mx.grped.rows}}
}

