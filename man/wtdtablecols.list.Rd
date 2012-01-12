\name{wtdtablecols.list}
\alias{wtdtablecols.list}
\title{Executes wtdtablecols on each varname in xframe and return the results as a list.
Add a "meta" attributed that specifies the weighting variable name (ie: wgtsname).}
\usage{
  wtdtablecols.list(xframe, varnames,
    wgtsname = "weightBase")
}
\arguments{
  \item{xframe}{data frame}

  \item{varnames}{variables to calculate wtdtablecols on}

  \item{wgtsname}{weighting variable in xframe to use for
  weighting}
}
\description{
  Executes wtdtablecols on each varname in xframe and
  return the results as a list. Add a "meta" attributed
  that specifies the weighting variable name (ie:
  wgtsname).
}
\examples{
xframe <- env.base$years1_5$outcomes
varnames <- names(env.base$years1_5$runs$cfreqs)

varnames <- names(existingMatrices)

w <- wtdtablecols.list(xframe, varnames, wgtsname = "weightBase")
}

