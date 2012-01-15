\name{checkOutcomeVars}
\alias{checkOutcomeVars}
\title{Checks that the outcome variables have a source variable in simframe.}
\usage{
  checkOutcomeVars(outcomes, simframe)
}
\arguments{
  \item{outcomes}{list of outcome variables with a varname
  attribute.}

  \item{simframe}{simframe to test for presence of varname}
}
\value{
  nothing if successful, otherwise errors
}
\description{
  Checks that the outcome variables have a source variable
  in simframe.
}
\examples{
checkOutcomeVars(createMELCfoutcomes(children),simframe) #outcomes <- createMELCfoutcomes(children)
}

