\name{predLogistic}
\alias{predLogistic}
\title{Predict probabilities from the coefficients of a logistic regression}
\usage{
  predLogistic(model.glm, envir = parent.frame(),
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
\value{
  a vector of predicted probabilities
}
\description{
  Predict probabilities from the coefficients of a logistic
  regression
}
\examples{
\dontrun{
model.glm <- models$houtptot
newdata <- simvalues
}
}

