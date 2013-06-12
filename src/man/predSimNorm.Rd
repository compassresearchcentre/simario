\name{predSimNorm}
\alias{predSimNorm}
\title{Predict and simulate continuous value from normal
distribution of specified standard deviation}
\usage{
  predSimNorm(model.glm, envir = parent.frame(),
    set = NULL)
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
  Predict and simulate continuous value from normal
  distribution of specified standard deviation
}
\examples{
\dontrun{
model.glm <- models$gpprev12
newdata <- simvalues
model.glm <- model2
}
}

