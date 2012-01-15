\name{modifyPropsAsBinLevels}
\alias{modifyPropsAsBinLevels}
\title{Takes a categorical var specified in separate binary level variables
and applies modifyProps and returns the result as a list of modified
binary level variables.}
\usage{
  modifyPropsAsBinLevels(vecs.list, desiredProps,
    propens = NULL)
}
\arguments{
  \item{vecs.list}{vectors to modify, eg:
  simframe[binLevelVarnames]}

  \item{desiredProps}{desired proportions}

  \item{propens}{propensities, if ANY}
}
\value{
  same list of vectors, but with proportions modified
}
\description{
  Takes a categorical var specified in separate binary
  level variables and applies modifyProps and returns the
  result as a list of modified binary level variables.
}
\examples{
level1 <- env.scenario$simframe$SESBTHLvl1
level2 <- env.scenario$simframe$SESBTHLvl2
level3 <- env.scenario$simframe$SESBTHLvl3
vecs.list <- list(level1=level1, level2=level2, level3=level3) ; vecs.list <- list(level1, level2, level3)
desiredProps <- c(0.1,0.1,0.8)
propens <- propens.all$SESBTH
r <- modifyPropsAsBinLevels(vecs.list, desiredProps = desiredProps, propens = propens)

level0 <- simframe$z1chparLvl0; level1 <- simframe$z1chparLvl1
desiredProps <- c(0,1)
vecs.list <- list(level0=level0, level1=level1) ; vecs.list <- list(level0, level1)
vecs.list <- simframe[binLevelVarnames]
r <- modifyPropsAsBinLevels(vecs.list, desiredProps = desiredProps, propens = propens)
}

