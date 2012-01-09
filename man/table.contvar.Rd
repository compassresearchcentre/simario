\name{table.contvar}
\alias{table.contvar}
\title{Display a continuous variable in a table using the
breaks supplied.
Attachs a meta attribute with varname}
\usage{
  table.contvar(x, breaks, varname)
}
\description{
  Display a continuous variable in a table using the breaks
  supplied. Attachs a meta attribute with varname
}
\examples{
x <- env.scenario$simframe$bwkg
breaks <- binbreaks$bwkg

table.contvar(env.scenario$simframe$bwkg, binbreaks$bwkg, "bwkg")
}

