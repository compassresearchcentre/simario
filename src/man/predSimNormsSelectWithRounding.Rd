\name{predSimNormsSelectWithRounding}
\alias{predSimNormsSelectWithRounding}
\title{Predict and simulate value from n normal models with truncation/rounding to ensure simulated
values stay within their category bounds.}
\usage{
  predSimNormsSelectWithRounding(x.cat, models,
    cont.binbreaks, envir = parent.frame())
}
\arguments{
  \item{x.cat}{a categorical vector}

  \item{models}{a list of models with length equal to the
  number of categories in x.cat}

  \item{cont.binbreaks}{the binbreaks of the categorical
  variable}

  \item{envir}{environment in which to evaluate model
  variables.}
}
\value{
  a continuous vector that when binned by cont.bonbreaks
  will be the same as x.cat
}
\description{
  A function based on PredSimNormsSelect with the
  modification that if any simulated values are outside the
  binbreaks for the group, the simulated values are changed
  to be equal to the boundary value.  Use when all the
  catToCont models for a variable are normal.
}
\examples{
\dontrun{
x.cont = simframe.master$fhrswrk
fhrs.binbreaks = attr(env.scenario$cat.adjustments$fhrswrk, "cont.binbreaks")
x.cat <- bin(x.cont, fhrs.binbreaks)
desired_props <- rep(1/7, 7)
desired_props <-c(.05, .1, .15, .2, .25, .2, .05)
adj.x.cat <- modifyProps(x.cat, desired_props, propen=NULL, accuracy=.01)
adj.x.cont <- predSimNormsSelectWithRounding(adj.x.cat, catToContModels$fhrswrk, cont.binbreaks=fhrs.binbreaks, envir=simframe.master)
check <- bin(adj.x.cont, fhrs.binbreaks)
table(check)
table(check)/sum(table(check))
}
}

