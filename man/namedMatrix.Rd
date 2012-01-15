\name{namedMatrix}
\alias{namedMatrix}
\title{Create a matrix of NA with specified col/row names/lengths.}
\usage{
  namedMatrix(rows, cols)
}
\arguments{
  \item{rows}{row names, or a numeric scalar for the number
  of rows}

  \item{cols}{columns names, or a numeric scalar for the
  number of cols}
}
\description{
  Create a matrix of NA with specified col/row
  names/lengths.
}
\examples{
cols <- 2 ; rows <- 5
cols <- c("Male","Female"); rows <- 5
cols <- c("Male","Female"); rows <- paste("Year", 1:5)

namedMatrix(rows, cols)
}

