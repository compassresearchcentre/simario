\name{createOutcomeMatrices}
\alias{createOutcomeMatrices}
\title{Create a list of empty outcome matrix variables for a specified outcome set.
The variables to create are those from the outcome set specified by "outcomeSetName".}
\usage{
  createOutcomeMatrices(simframe, outcomeSetName,
    iterations)
}
\arguments{
  \item{simframe}{simulation frame}

  \item{outcomeSetName}{name of the outcomeset to create
  variables for}

  \item{iterations}{either a vector of names for each
  iteration, eg: ("Year 5", "Year 6" ...) or a scalar
  specifying the number of iterations.}
}
\value{
  A dataframe of outcome variables. Each output variable is
  a matrix of simvalues by iterations. Each matrix is
  assigned the "varname" attribute that names the source
  variable in the simframe that will be used during
  simulation to fill the matrix. This source variable is
  the same name as the name outcome matrix.
}
\description{
  Create a list of empty outcome matrix variables for a
  specified outcome set. The variables to create are those
  from the outcome set specified by "outcomeSetName".
}
\examples{
simframe <- simframe.start
simframe <- env.base$simframe
outcomeSetName <- "years1_5"
outcomeSetName <- "years6_13"
iterations <- 8
iterations <- c(6:13)
outcomes <- createOutcomeMatrices(simframe, outcomeSetName, iterations)
}

