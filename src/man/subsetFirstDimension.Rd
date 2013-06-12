\name{subsetFirstDimension}
\alias{subsetFirstDimension}
\title{Subset the first dimension, returning the same number of dimensions}
\usage{
  subsetFirstDimension(x, logiset)
}
\arguments{
  \item{x}{a vector, matrix or array}

  \item{logical}{vector to subset first dimension by}
}
\description{
  Subset the first dimension, returning the same number of
  dimensions
}
\examples{
x <- matrix(c(1:6), nrow=2)
logiset <- c(TRUE,FALSE)
subsetFirstDimension(x, logiset)
}

