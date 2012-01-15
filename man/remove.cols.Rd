\name{remove.cols}
\alias{remove.cols}
\title{Remove cols by name.}
\usage{
  remove.cols(mx, cnames)
}
\arguments{
  \item{mx}{matrix}

  \item{cnames}{vector of colnames}
}
\description{
  Remove cols by name.
}
\examples{
mx <- matrix(1:4, dimnames=list(NULL, c("A","NA (\%)")), nrow = 2)
mx <- matrix(1:4, dimnames=list(NULL, c("A","B")), nrow = 2)
cnames <- "NA (\%)"
remove.cols(mx, cnames)
}

