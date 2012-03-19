\name{is.alpha.only}
\alias{is.alpha.only}
\title{TRUE for elements that only contain alpha characters, ie: have no numeric component.}
\usage{
  is.alpha.only(x)
}
\arguments{
  \item{x}{character vector}
}
\description{
  TRUE for elements that only contain alpha characters, ie:
  have no numeric component.
}
\examples{
x <- c("1", "1 (\%)", "per cent")
is.alpha.only(x)
}

