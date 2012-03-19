\docType{data}
\name{Simmodule}
\alias{Simmodule}
\title{Simulation module object.}
\format{proto object 
 $ class          :function (.)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 216 11 216 33 18 40 216 216 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x00000000113915b0>  
 $ collateRunStats:function (., simenv)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 180 21 211 2 28 9 180 211 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x00000000113915b0>  
 $ outcomes       : list() 
 $ clone          :function (.)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 214 11 214 57 18 64 214 214 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x00000000113915b0>  
 $ name           : NULL 
 $ result         : list() 
 $ simulateRun    :function (., simenv)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 119 17 121 2 24 9 119 121 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x00000000113915b0>  
 $ new            :function (., name, freqvars, cfreqvars, meanvars, freqs.args,  
    means.args)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 77 9 107 2 16 9 77 107 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x00000000113915b0>  
 $ appendRunStats :function (.)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 135 20 165 2 27 9 135 165 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x00000000113915b0>  
 $ runstats       :List of 5}
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
  collateRunStats to get a final simulation result.
}
\keyword{datasets}

