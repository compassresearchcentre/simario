\name{flatten.listmx}
\alias{flatten.listmx}
\title{Takes a list of matrices, flattens them into a single row, aligns them by column name
and then returns a single matrix.}
\usage{
  flatten.listmx(listmx)
}
\arguments{
  \item{listmx}{list of matrices to flatten. Can have
  different dimensions but must have similar names for
  alignment (}
}
\value{
  a single matrix with colnames = the flattened row/col
  names of listmx[[1]]], meta = meta of listmx[[1]]
  rownames = names of listmx
}
\description{
  Takes a list of matrices, flattens them into a single
  row, aligns them by column name and then returns a single
  matrix.
}
\examples{
listmx <- list(A=structure(matrix(c(1:6), nrow=2, dimnames=(list(1:2, 1:3))), meta=c("grpby.tag"="r1stchildethn")), B=structure( matrix(c(21:26), nrow=2, dimnames=(list(1:2, 1:3))) , meta=c("grpby.tag"="r1stchildethn")))
flatten.listmx(listmx)
}
\seealso{
  listmx.flat.aligned)
}

