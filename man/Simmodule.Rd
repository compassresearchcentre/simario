\docType{data}
\name{Simmodule}
\alias{Simmodule}
\title{Simulation module object.}
\format{proto object 
 $ runs            :List of 5 
 $ class           :function (.)   
 $ outcomes        : list() 
 $ clone           :function (.)   
 $ name            : NULL 
 $ result          : list() 
 $ calcFinalResults:function (., simenv)   
 $ simulateRun     :function (., simenv)   
 $ new             :function (., name, freqvars, cfreqvars, meanvars, freqs.args,  
    means.args)   
 $ appendRunStats  :function (.)}
\usage{
  Simmodule
}
\description{
  A simulation module is really the core of a simulation.
  It contains the code and output for a distinct set of
  results generated, eg: health outcomes for years 1 - 10.
}
\details{
  It contains the following key elements:

  outcomes - a list of all outcome matrices for the
  Simmodule.

  each Simmodule has a \code{simulateRun} method which
  transforms the simframe. Typically, transformations will
  move variables for micro-units in the simframe through
  multiple iterations (or time steps). At the end of each
  iteration, for outcome variables (as defined in the
  simframe), the current values for all micro-units are
  stored in an outcome matrix. An outcome matrix contains
  the set of values for each micro-unit during each
  iteration.

  At the end of each run a set of run stats is calculated
  for outcomes. A run stat is essentially a function that
  takes an outcome matrix and produces an aggregate value
  for each iteration. This aggregate value may be a single
  value (eg: mean), a vector (eg: frequencies, quantiles,
  summary), or a matrix (eg: 2 way table).

  Run stats are averaged across multiple runs by
  calcFinalResults to get a final simulation result.
}
\keyword{datasets}

