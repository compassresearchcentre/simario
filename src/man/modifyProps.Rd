\name{modifyProps}
\alias{modifyProps}
\title{Change the default simulated values to proportions requested by the user.
The values of a vector are changed so that the proportions of each discrete value
are that requested by the user.
The values that are changed are the ones with the highest propensity to do so
The propensity score(s) of an observation to be a higher (or lower) value can
be given as input to the function.
If the propensity score(s) are not provided by the user then random
propensity scores are generated.}
\usage{
  modifyProps(default.vec, props, propens = NULL)
}
\arguments{
  \item{props}{a vector that is the proportions requested
  by the user.  The vector is the length of the number of
  distinct values of the variable being modified.}

  \item{default.vec}{a vector after a run of the
  simulation. The values of this variable will be changed
  in accordance with what the user requests}

  \item{propens}{matrix of the propensity scores for each
  child For binary variables there is one column of
  propensity scores: the propensities to change from a 0 to
  a 1.  For categorical variables with more than two
  categories there are multiple columns of propensity
  scores: E.g. for a three category variables the
  propensities to change from category 1 to category 2 are
  in the first column and the propensities to change from
  category 2 to category 3 are in the second column.}
}
\description{
  Change the default simulated values to proportions
  requested by the user. The values of a vector are changed
  so that the proportions of each discrete value are that
  requested by the user. The values that are changed are
  the ones with the highest propensity to do so The
  propensity score(s) of an observation to be a higher (or
  lower) value can be given as input to the function. If
  the propensity score(s) are not provided by the user then
  random propensity scores are generated.
}
\note{
  Assumptions made by the function: It is assumed that the
  proportions given in props are given in consectuive
  increasing order (e.g. {0,1}, {1, 2, 3} or {2, 5, 9,
  23}).  If the user wants to make it so no observations
  are in a particular category the value 0 must be put in
  the corresponding place in the vector props If the
  propensity scores (propens) are provided by the user then
  it is assumed that default.vec and propens are given in
  the same order and exactly the same children are in each
  vector (i.e. there are no children in one vector that are
  not in the other).  In other words, the propensity score
  for a specific child is in the same row in propens as
  that same child's value of the variable in default.vec.
}
\examples{
\dontrun{
default.vec <- children$SESBTH
props <- c(0.1,0.1,0.8)
propens <- propensities$SESBTH

default.vec <- env.scenario$simframe$z1fsmokeLvl1
props <- c(0.1,0.9)
propens <- propensities$z1fsmokeLvl1[,1]

default.vec <- env.scenario$simframe$catpregsmk2
props <- c(0.1, 0.1, 0.1, 0.5, 0.2)
propens <- NULL

prop.table(table(default.vec))
prop.table(table(modifyProps(default.vec, props, propens)))
}
}
\seealso{
  This function calls \code{\link{change.cat}}
}

