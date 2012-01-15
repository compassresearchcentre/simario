\name{rmall}
\alias{rmall}
\title{Remove all objects in global environment.}
\usage{
  rmall(exceptions = NULL)
}
\arguments{
  \item{exceptions}{names of vars not to remove}
}
\description{
  Remove all objects in global environment.
}
\examples{
exceptions <- c("ov")
rmall(exceptions)
}

