\name{as.array.listmx}
\alias{as.array.listmx}
\title{Convert a list of matrices into an array, with each matrix in the z dimension.}
\usage{
  as.array.listmx(listmx)
}
\arguments{
  \item{listmx}{list of matrices. Each must matrix the same
  dimensions.}
}
\value{
  array with dimnames and meta from listmx
}
\description{
  Convert a list of matrices into an array, with each
  matrix in the z dimension.
}
\examples{
listmx <- list(A=structure(matrix(c(1:6), nrow=2, dimnames=(list(1:2, 1:3))), meta=c("grpby.tag"="r1stchildethn")), B=structure( matrix(c(21:26), nrow=2, dimnames=(list(1:2, 1:3))) , meta=c("grpby.tag"="r1stchildethn")))
 as.array.listmx(listmx)
}

