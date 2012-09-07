\name{selectFixedOutcomeIfSet}
\alias{selectFixedOutcomeIfSet}
\title{Checks whether a fixed vector of values should be used instead of the just simulated vector
(parameter x).
The fixed vector is that in fixed.outcomes and is based on user specified adjustments to a base
run of the simulation.}
\usage{
  selectFixedOutcomeIfSet(simenv, iteration, x, varname)
}
\arguments{
  \item{x}{vector of values to replace with vector in
  fixed.outcomes. Generally x is continuous.}

  \item{varname}{name of variable}
}
\description{
  Checks whether a fixed vector of values should be used
  instead of the just simulated vector (parameter x). The
  fixed vector is that in fixed.outcomes and is based on
  user specified adjustments to a base run of the
  simulation.
}
\examples{
\dontrun{
simenv <- env.scenario
iteration <- 3
x <- 1:1017
varname <- "kids"
x.replaced <- selectFixedOutcomeIfSet(simenv, iteration, x, varname)

attr(env.scenario$fixed.outcomes[["kids"]], "is.fixed.iteration")[3] <- TRUE
 x.replaced2 <- selectFixedOutcomeIfSet(simenv, iteration, x, varname)
table(x.replaced2)
}
}

