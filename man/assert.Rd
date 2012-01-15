\name{assert}
\alias{assert}
\title{Stops and displays indices of any element that is FALSE.}
\usage{
  assert(xvec)
}
\arguments{
  \item{xvec}{logical vector}
}
\description{
  Stops and displays indices of any element that is FALSE.
}
\examples{
assert(c(T,T))
assert(c(T,F))
}

