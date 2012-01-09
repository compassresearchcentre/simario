\name{setScenarioSimenv}
\alias{setScenarioSimenv}
\title{Appends the supplied simenv to envs (the list of
scenario environments) and sets env.scenario
to point to the simenv.}
\usage{
  setScenarioSimenv(simenv)
}
\arguments{
  \item{simenv}{a simenv. If name is NULL then the name
  will be set to "Scenario X" where X is the next scenario
  number.}
}
\description{
  Called by initMELC()
}
\examples{
setScenarioSimenv(SimenvMELC$new())
}

