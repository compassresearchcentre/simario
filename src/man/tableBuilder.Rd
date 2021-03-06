% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/TableBuilder.R
\name{tableBuilder}
\alias{tableBuilder}
\title{Returns a dataset for use in a table by the Table Builder GUI.}
\usage{
tableBuilder(envName, statistic, variableName, grpbyName = "", CI = TRUE,
  logisetexpr = NULL, dict = dict_demo, not.in.logiset = FALSE)
}
\arguments{
\item{envName}{the environment to use - Base, Scenario etc.}

\item{statistic}{the summary measure to use in producing the dataset - frequencies, means, quintiles}

\item{variableName}{the variable to use in producing the dataset}

\item{grpbyName}{a subgroup by which to examine the variable}

\item{CI}{logical indicating whether 95% confidence intervals should be generated}

\item{logisetexpr}{a character expression which defines the logiset variable}

\item{dict}{Dictionary object.}

\item{not.in.logiset}{logical.  If TRUE, then the results will be calculated on those not in the logiset rather than those in the logiset.}
}
\value{
a summary table for the entire or subgroup of the variable of interest.
}
\description{
Currently cannot do freqs for final outcomes (can only do freqs for those vars with binbreaks)
Also currently can only use a true categorical variable as a groupby variable - this coould be changed.
}
\details{
If the user defined a logiset/subgroup expression inthe scenario weightings screen and then
goes to tableBuilder() and sets a grpbyName, then the results they get will be on the entire
dataset, not just on their subgroup they defined earlier.  The user can't define a logiset
expression in tableBuilder - the logisetexpr parameter is there so it can be used to show the
user, in the scenario weightings screen, the distributions of the variable of interest for
their subgroup only so they can better choose the proportions for their subgroup scenario.
}


