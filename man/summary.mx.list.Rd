\name{summary.mx.list}
\alias{summary.mx.list}
\title{Convience method to apply summary.mx to a subset of elements of
list.}
\usage{
  summary.mx.list(xlist, indices)
}
\arguments{
  \item{xlist}{a list of matrices}

  \item{indices}{indices, either numeric or names, of
  elements in x for which to apply summary.mx, or NULL to
  apply to all elements of x}
}
\value{
  list of summaries
}
\description{
  Convience method to apply summary.mx to a subset of
  elements of list.
}
\examples{
xlist <- env.base$years6_13$outcomes
indices <- names(env.base$years6_13$runs$means$all)
summary.mx.list(xlist, indices)
}

