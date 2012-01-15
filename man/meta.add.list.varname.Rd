\name{meta.add.list.varname}
\alias{meta.add.list.varname}
\title{Appends a "varname" meta element to the meta of each element
in a list.}
\usage{
  meta.add.list.varname(xlist, varnames = names(xlist))
}
\arguments{
  \item{xlist}{list of objects}

  \item{varnames}{parallel vector of varnames. Defaults to
  names(xlist).  If NULL, does nothing}
}
\value{
  xlist with the corresponding varname append to the meta
  attribute of each xlist element.
}
\description{
  Appends a "varname" meta element to the meta of each
  element in a list.
}
\examples{
xlist <- list(gpmorb = 1:3, hadmtot = 10:13)
xlist <- list(1:3, 10:13)
varnames <- names(xlist)

meta.add.list.varname(xlist, varnames)
}

