\name{predSimBin}
\alias{predSimBin}
\title{Predict and simulate binary value from logistic model}
\usage{
  predSimBin(model.glm, envir = parent.frame(), set = NULL)
}
\arguments{
  \item{model.glm}{model specifying variables to evaluate
  and coefficients to multiple by.}

  \item{envir}{environment in which to evaluate model
  variables.  if unspecified, uses caller's environment}

  \item{set}{logical vector indicating elements or rows to
  simulate, or NULL to simulate using all values in envir}
}
\description{
  Predict and simulate binary value from logistic model
}
\examples{
#model.glm <- models$z1msmokeLvl1
}

