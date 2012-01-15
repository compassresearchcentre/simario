\name{table.contvar}
\alias{table.contvar}
\title{Display a vector of continuous values in a table using the
breaks supplied.
Attachs a meta attribute with varname}
\usage{
  table.contvar(x, breaks, varname)
}
\arguments{
  \item{x}{vector of continous values}

  \item{breaks}{a numeric vector of two or more cut points
  NB: note that the cut point value is not included in the
  bin (ie: include.lowest = FALSE) Therefore the very first
  cut point must be less than min(x)}

  \item{varname}{added as a tag on the meta attribute}
}
\description{
  Display a vector of continuous values in a table using
  the breaks supplied. Attachs a meta attribute with
  varname
}
\examples{
x <- env.scenario$simframe$bwkg
breaks <- binbreaks$bwkg

table.contvar(env.scenario$simframe$bwkg, binbreaks$bwkg, "bwkg")
}

