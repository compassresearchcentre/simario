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

  \item{set}{subset logical expression indicating elements
  or rows to keep, or NULL to use all elements returned by
  evaluated model variables}
}
\description{
  Predict and simulate binary value from logistic model
}
\examples{
model.glm <- models$z1msmokeLvl1
}

