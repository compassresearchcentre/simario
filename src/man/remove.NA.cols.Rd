\name{remove.NA.cols}
\alias{remove.NA.cols}
\title{Remove columns that contain all NAs.}
\usage{
  remove.NA.cols(mx)
}
\arguments{
  \item{mx}{matrix}
}
\description{
  Remove columns that contain all NAs.
}
\examples{
mx <- matrix(c(1:4, NA, NA), nrow=2)
remove.NA.cols(mx)
}

