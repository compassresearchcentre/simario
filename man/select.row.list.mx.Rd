\name{select.row.list.mx}
\alias{select.row.list.mx}
\title{Select only the specified row from each matrix in a list,
and return a list of row vectors.}
\usage{
  select.row.list.mx(mxlist, rownum, na.rm = T)
}
\arguments{
  \item{mxlist}{list of matrices}

  \item{rownum}{row number to select}

  \item{na.rm}{if TRUE, then do not return any selected row
  that contains a NA}
}
\description{
  Select only the specified row from each matrix in a list,
  and return a list of row vectors.
}
\examples{
mxlist <- list(mx1=matrix(1:10, nrow=2), mx2=matrix(c(1,2,NA,4), nrow=2))
 mxlist <- list(mx1=matrix(1:10, nrow=2))
 rownum <- 1
 select.row.list.mx(mxlist, rownum, na.rm = FALSE)
 select.row.list.mx(mxlist, rownum, na.rm = TRUE)
}

