\name{codingsMatch}
\alias{codingsMatch}
\title{Return the category names for a vector of coded values.
If no category names, returns the list as is.}
\usage{
  codingsMatch(x, varname)
}
\arguments{
  \item{x}{coded values}

  \item{varname}{name of the category of x, or NULL}

  \item{x,}{is there are no codings for varname, else a
  vector of category names corresponding to the values in
  x}
}
\description{
  Return the category names for a vector of coded values.
  If no category names, returns the list as is.
}
\examples{
x <- c(0,1,0,1,0,1) ;  varname <- z1singleLvl1
 x <- character(0) ; varname <- NULL
 codingsMatch(x, varname)
}

