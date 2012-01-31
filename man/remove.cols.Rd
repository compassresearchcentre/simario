\name{remove.cols}
\alias{remove.cols}
\title{Remove cols by name.}
\usage{
  remove.cols(x, cnames)
}
\arguments{
  \item{x}{matrix or dataframe}

  \item{cnames}{vector of colnames}
}
\description{
  Remove cols by name.
}
\examples{
x <- matrix(1:4, dimnames=list(NULL, c("A","NA (\%)")), nrow = 2)
x <- matrix(1:4, dimnames=list(NULL, c("A","B")), nrow = 2)
cnames <- "NA (\%)"
remove.cols(x, cnames)
data(mtcars); x <- cars; cnames <- "dist"
remove.cols(x, cnames)
}

