\docType{data}
\name{Simmodule}
\alias{Simmodule}
\title{Simulation module object.}
\format{proto object 
 $ class                      :function (.)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 93 11 93 33 18 40 93 93 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x0766d60c>  
 $ clone                      :function (.)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 91 11 91 57 18 64 91 91 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x0766d60c>  
 $ collate_all_run_results    :function (., all_run_results)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 87 29 89 2 36 9 87 89 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x0766d60c>  
 $ simulateRun                :function (., simenv)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 59 17 61 2 24 9 59 61 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x0766d60c>  
 $ new                        :function (., name)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 39 9 47 2 16 9 39 47 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x0766d60c>  
 $ map_outcomes_to_run_results:function (., outcomes)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 75 33 77 2 40 9 75 77 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x0766d60c>}
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

