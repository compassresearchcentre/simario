\name{lapply.inner}
\alias{lapply.inner}
\title{Apply a function to the inner element of a list of lists.}
\usage{
  lapply.inner(lol, .FUN, ..., simplify = FALSE,
    USE.NAMES = FALSE)
}
\arguments{
  \item{lol}{list of lists}

  \item{.FUN}{function that takes an element from lol}

  \item{...}{other arguments to pass to .FUN}

  \item{simplify}{logical or character string; should the
  result be simplified to a vector, matrix or higher
  dimensional array if possible? See \code{\link{sapply}}.}

  \item{USE.NAMES}{logical; if TRUE and if X is character,
  use X as names for the result unless it had names
  already. See \code{\link{sapply}}.}
}
\value{
  a list of lists, the result of applying .FUN to lol
}
\description{
  Apply a function to the inner element of a list of lists.
}
\examples{
\dontrun{
lol <- env.base$years6_13$runs$means
lol <- env.base$years1_5$runs$means
lol <- env.base$years1_5$runs$means
lol <- env.base$years6_13$runs$summaries
lol <- years1_5$runs$summaries
lapply.inner(lol, mean.array.z)
}
}

