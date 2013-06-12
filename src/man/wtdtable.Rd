\name{wtdtable}
\alias{wtdtable}
\title{Weighted frequency table}
\usage{
  wtdtable(x, wgts = rep(1, length(x)))
}
\arguments{
  \item{x}{vector of values.}

  \item{wgts}{vector of weights. If unspecified a default
  weight of 1 is used for each value of x}
}
\value{
  weighted frequency table
}
\description{
  Weighted frequency table
}
\examples{
\dontrun{
library(Hmisc)
x <- c(8,8,2,1,1,8)
wgts <- c(1,1,2,1,1,1)
wtdtable(x, wgts)
1 2 8
2 2 3
}
}

