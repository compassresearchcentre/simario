\name{add_trailing_slash}
\alias{add_trailing_slash}
\title{Adds a trailing slash, if required, to character vector}
\usage{
  add_trailing_slash(x)
}
\arguments{
  \item{x}{character vector}
}
\value{
  x with a trailing slash, if required
}
\description{
  Adds a trailing slash, if required, to character vector
}
\examples{
x <- "d:/workspace"
x <- "d:/workspace/"
add_trailing_slash("d:/workspace")
add_trailing_slash("d:/workspace/")
}

