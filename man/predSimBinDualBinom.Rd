\name{predSimBinDualBinom}
\alias{predSimBinDualBinom}
\title{Predict and simulate binary value from 2 binomial models.}
\usage{
  predSimBinDualBinom(select, model0, model1,
    envir = parent.frame())
}
\arguments{
  \item{select}{a logical vector, or vector of 0s and 1s,
  which determine when to use model0 and when to use
  model1, i.e: when select == 0, then result is
  predSimBinom using model0 when select == 1, then result
  is 1 - predSimBinom using model1}

  \item{model0}{model0}

  \item{model1}{model1}

  \item{envir}{environment in which to evaluate model
  variables.}
}
\description{
  Predict and simulate binary value from 2 binomial models.
}
\examples{
\dontrun{
	select <- z1single_previousLvl1
	model0 <- models$z1singlePrev0 ; model1 <- models$z1singlePrev1
	predSimBinDualBinom(select, model0, model1)
}
}

