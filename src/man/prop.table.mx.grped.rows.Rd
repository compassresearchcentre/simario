\name{prop.table.mx.grped.rows}
\alias{prop.table.mx.grped.rows}
\title{Calculates the proportions within row groupings of a flattened matrix.
Not suitable when each row group is of a different size.}
\usage{
  prop.table.mx.grped.rows(mx.grped.rows, numgrps)
}
\arguments{
  \item{mx.grped.rows}{a matrix with grped rows, ie: within
  each row there are groups of columns that form a set.
  Proportions are then calculated within these groups.}

  \item{numgrps}{the number of groups in each row. Each
  group will be of size \code{ncol(mx.grped.rows) /
  numgrps}.  If numgrps = 1, then there is only 1 group and
  proportions are calculated across the whole row.}
}
\value{
  the original matrix but with its values converted to
  proportions.  Preserves names and any "meta" attribute of
  \code{mx.grped.rows)}.
}
\description{
  Calculates the proportions within row groupings of a
  flattened matrix. Not suitable when each row group is of
  a different size.
}
\examples{
mx.grped.rows <- matrix(c(1,2,1,3,1,4,2,2,2,3,2,4), nrow=2, byrow = TRUE)
numgrps <- 3

mx.grped.rows <- matrix(c(1:4), nrow=1)
numgrps <- 1

prop.table.mx.grped.rows(mx.grped.rows, numgrps)
}

