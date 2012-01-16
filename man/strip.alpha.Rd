\name{strip.alpha}
\alias{strip.alpha}
\title{Remove any alpha from character vector, leaving only numbers.}
\usage{
  strip.alpha(x)
}
\arguments{
  \item{x}{character vector}
}
\description{
  Remove any alpha from character vector, leaving only
  numbers.
}
\examples{
x <- c("1 (\%)", "-2 (\%)", "2.102 (\%)", "10 (\%)")
strip.alpha(x)
}

