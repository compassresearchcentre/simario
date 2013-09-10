\name{summary.grpby}
\alias{summary.grpby}
\title{Summary table, with option to group and weight results.}
\usage{
  summary.grpby(x, grpby = NULL, wgts = NULL)
}
\arguments{
  \item{x}{vector of values which can be interpreted as
  factors}

  \item{grpby}{elements to group by, or NULL to do no
  grouping Same length as the columns of x.}

  \item{wgts}{vector of weights, or NULL to do no weighting
  Same length as the columns of x.}
}
\value{
  a weighted summary table, or rows of weighted summary
  tables if grpby is specified. function also returns the
  weighted number of NA's, but does not include NA's with
  weights of NA.
}
\description{
  Summary table, with option to group and weight results.
}
\examples{
library(Hmisc)
x <- c(8,8,2,1,1,8)
x <- c(8,8,2,1,1,NA)
x <- c(NA,NA,2,1,1,8)
grpby <- c('M','M','F','F','F','F')
wgts<- c(1,2,2,3,1,1)
summary.grpby(x)
summary.grpby(x, grpby)
summary.grpby(x=x, grpby=grpby,wgts=wgts)
}

