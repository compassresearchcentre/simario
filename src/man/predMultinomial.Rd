\name{predMultinomial}
\alias{predMultinomial}
\title{Predict probabilities from the coefficients of a multinomial regression  (currently only works if output catgegories are 1,2,3 etc with 1 being the reference)}
\usage{
  predMultinomial(model.glm.list, envir = parent.frame(),
    set = NULL)
}
\arguments{
  \item{model.glm.list}{List of logit models specifying
  variables to evaluate and coefficients to multiple by -
  each logit model referring to an output category from the
  multinomial model. IMPORTANT NOTE - only models for the
  non-reference output catgegories must be listed, with the
  logit for the reference group- i.e log(1/1)=0 - being
  taken care of in the function itself. Logit models must
  be listed in order of their output category value - e.g
  the first model refers to output category=2, second model
  refers to output category=3, and so on. (Output
  category=1 has to be the reference category).}

  \item{envir}{environment in which to evaluate model
  variables.  if unspecified, uses caller's environment}

  \item{set}{logical vector indicating elements or rows to
  simulate, or NULL to simulate using all values in envir}
}
\value{
  a matrix of predicted probabilities
}
\description{
  Predict probabilities from the coefficients of a
  multinomial regression (currently only works if output
  catgegories are 1,2,3 etc with 1 being the reference)
}
\examples{
\dontrun{
 model.glm.list<-list(models$GP_multinomial_response_cat2,
			models$GP_multinomial_response_cat3,models$GP_multinomial_response_cat4,
						models$GP_multinomial_response_cat5)
set<-still_alive_and_65plus_NOTRES
}
}

