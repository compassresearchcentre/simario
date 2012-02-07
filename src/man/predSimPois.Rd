\name{predSimPois}
\alias{predSimPois}
\title{Predict and simulate continuous value from poisson model}
\usage{
  predSimPois(model.glm, envir = parent.frame(),
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
  Predict and simulate continuous value from poisson model
}
\examples{
\dontrun{
 model.glm <- models$hadmtot
	newdata <- simvalues
}
}

