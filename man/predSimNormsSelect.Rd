\name{predSimNormsSelect}
\alias{predSimNormsSelect}
\title{Predict and simulate value from 3 normal models.}
\usage{
  predSimNormsSelect(select, model1, model2, model3,
    envir = parent.frame())
}
\arguments{
  \item{select}{a logical vector, or vector of 0s and 1s,
  which determine when to use model0 and when to use
  model1, i.e: when select == 0, then result is
  predSimBinom using model0 when select == 1, then result
  is 1 - predSimBinom using model1}

  \item{model1}{model 1}

  \item{model2}{model 2}

  \item{model3}{model 3}

  \item{envir}{environment in which to evaluate model
  variables.}
}
\description{
  Predict and simulate value from 3 normal models.
}

