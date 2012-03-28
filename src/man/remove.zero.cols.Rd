\name{remove.zero.cols}
\alias{remove.zero.cols}
\title{Remove cols that contain all zeros.}
\usage{
  remove.zero.cols(mx)
}
\arguments{
  \item{mx}{matrix}
}
\description{
  Remove cols that contain all zeros.
}
\examples{
mx <- matrix(c(1:4, 0, 0), nrow=2)
remove.zero.cols(mx)
}

