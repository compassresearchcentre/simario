\name{dimnames.prepend.header}
\alias{dimnames.prepend.header}
\title{Push row/col headers (ie: names of the rownames and colnames) into the rownames and colnames.}
\usage{
  dimnames.prepend.header(mx)
}
\arguments{
  \item{mx}{matrix}
}
\description{
  Push row/col headers (ie: names of the rownames and
  colnames) into the rownames and colnames.
}
\examples{
mx <- matrix(1:4, nrow=2, ncol=2, dimnames=list(gender=c("M","F"), SES=c("Low","High")))
 mx <- matrix(1:4, nrow=2, ncol=2, dimnames=list(c("M","F"), SES=c("Low","High")))
 names(dimnames(mx))[[ROW]] <- NA
 dimnames.prepend.header(mx)
}

