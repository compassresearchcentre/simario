\name{predSimNBinom}
\alias{predSimNBinom}
\title{Predict and simulate continuous value from negative binomial
distribution with mean = exp(predicted) and size=1/alpha
where alpha is specified in the model.}
\usage{
  predSimNBinom(model.glm, envir = parent.frame(),
    set = NULL)
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
  Predict and simulate continuous value from negative
  binomial distribution with mean = exp(predicted) and
  size=1/alpha where alpha is specified in the model.
}
\examples{
model.glm <- models$houtptot
newdata <- simvalues
predSimNBinom (model.glm, newdata)
}

