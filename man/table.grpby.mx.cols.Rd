\name{table.grpby.mx.cols}
\alias{table.grpby.mx.cols}
\title{Executes table.grpby on the cols of a matrix.}
\usage{
  table.grpby.mx.cols(mx, grpby = NULL, grpby.tag = NULL)
}
\arguments{
  \item{mx}{matrix}

  \item{grpby}{elements to group by, or NULL or unspecified
  to do no grouping}

  \item{grpby.tag}{if specified this value with be attached
  as the meta attribute "grpby.tag"}
}
\value{
  list, each element of the list represents a year, each
  element is a 2D matrix where columns are the group by and
  rows the categories. If grpby = NULL then a 1D matrix
  with columns as categories is returned.
}
\description{
  Executes table.grpby on the cols of a matrix.
}
\examples{
mx <- env.base$years1_5$outcomes$z1accomLvl1
grpby <- env.base$years1_5$outcomes$z1gender
r0 <- table.grpby.mx.cols(mx, grpby)

mx <- matrix(c(8,2,2,2,8,2,3,2,3,2,2,4,8,2,3,4,2,2,4,3),nrow=4,ncol=5,dimnames=list(NULL, LETTERS[1:5]))
grpby <- c('M','F','F','M')
r1 <- table.grpby.mx.cols(mx, grpby)

mx <- outcomes[[1]]
grpby=children$r1stchildethn ; grpby.tag="r1stchildethn"
r2 <- table.grpby.mx.cols(mx, grpby, grpby.tag)

mx <- env.base$modules$years1_5$outcomes$z1kidsLvl1
grpby = NULL
r3 <- table.grpby.mx.cols(mx)
}

