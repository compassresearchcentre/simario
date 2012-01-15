\name{remove.NA.cols}
\alias{remove.NA.cols}
\title{Identify columns will all NAs, then remove}
\usage{
  remove.NA.cols(mx)
}
\arguments{
  \item{mx}{matrix}
}
\description{
  Identify columns will all NAs, then remove
}
\examples{
mx <- matrix(c(1:4, NA, NA), nrow=2)
remove.NA.cols(mx)
}

