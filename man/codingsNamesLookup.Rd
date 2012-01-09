\name{codingsNamesLookup}
\alias{codingsNamesLookup}
\title{Returns the names of the codings for the supplied variables.
NB: Requires codings variable to have been set in global environment.}
\usage{
  codingsNamesLookup(vars)
}
\arguments{
  \item{vars}{vector of variable names}
}
\value{
  list of coding names for the variables supplied
}
\description{
  Returns the names of the codings for the supplied
  variables. NB: Requires codings variable to have been set
  in global environment.
}
\examples{
vars <- c("msmoke", "fsmoke")
vars <- names(runs.mean.freq)

codingsNamesLookup(vars)
}

