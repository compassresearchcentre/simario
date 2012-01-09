\name{modelVariableNames}
\alias{modelVariableNames}
\title{Return the variables used in the supplied terms.
Removes "LvlX" suffix, and surrounding "I( )"
as well as disaggregates squared variables}
\usage{
  modelVariableNames(model, strip.Lvl = TRUE)
}
\arguments{
  \item{xterms}{terms}

  \item{strip.Lvl}{strip the LvlX from names. Defaults to
  TRUE.}
}
\value{
  chr vector of variable names
}
\description{
  Return the variables used in the supplied terms. Removes
  "LvlX" suffix, and surrounding "I( )" as well as
  disaggregates squared variables
}
\examples{
model <- models$z1accomLvl1
modelVariableNames(model)
modelVariableNames(model, strip.Lvl = FALSE)
}

