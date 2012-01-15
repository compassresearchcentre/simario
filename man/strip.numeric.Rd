\name{strip.numeric}
\alias{strip.numeric}
\title{Remove any numbers from character vector, leaving only alpha.}
\usage{
  strip.numeric(x)
}
\arguments{
  \item{x}{character vector}
}
\description{
  Remove any numbers from character vector, leaving only
  alpha.
}
\examples{
x <- c("1 (\%)", "-2 (\%)", "2.102 (\%)", "10 (\%)")
strip.numeric(x)
}

