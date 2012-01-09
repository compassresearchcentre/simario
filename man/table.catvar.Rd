\name{table.catvar}
\alias{table.catvar}
\title{Produce a proportioned table for x, using
the specified coding as names and
setting the "meta" attribute to "varname"
of coding.}
\usage{
  table.catvar(x, coding)
}
\arguments{
  \item{x}{vector of values}

  \item{coding}{a coding variable. names(coding) is the
  labels attr(coding, "varname") is a named element in
  xlist}
}
\value{
  a table (proportions) with names specified by coding
}
\description{
  Produce a proportioned table for x, using the specified
  coding as names and setting the "meta" attribute to
  "varname" of coding.
}
\examples{
table.catvar(children$SESBTH, codings$SESBTH)
x <- simframe$z1singleLvl1 ; coding <- codings$z1singleLvl1
table.catvar(simframe$z1singleLvl1, codings$z1singleLvl1)
}

