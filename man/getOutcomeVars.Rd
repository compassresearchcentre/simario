\name{getOutcomeVars}
\alias{getOutcomeVars}
\title{Get a set of outcome variables from the simframe.}
\usage{
  getOutcomeVars(simframe, select_outcome_type = NULL,
    select_outcome_set = NULL, sorted = FALSE)
}
\arguments{
  \item{simframe}{simframe}

  \item{select_outcome_type}{a vector of outcome_types, or
  NULL to select all types.}

  \item{select_outcome_set}{a vector of outcome_set, , or
  NULL to select all sets.}

  \item{sorted}{return outcome variables sorted by outcome
  name?}
}
\value{
  a vector of simframe variables (with names the same as
  the vector value)
}
\description{
  Get a set of outcome variables from the simframe.
}
\examples{
select_outcome_set = "years1_5"
select_outcome_set = c("years1_5", "years6_13")
select_outcome_set = NULL

select_outcome_type = "categorical"
select_outcome_type = "continuous"
select_outcome_type = NULL
sorted = TRUE

getOutcomeVars(simframe, select_outcome_type, select_outcome_set, sorted)
}

