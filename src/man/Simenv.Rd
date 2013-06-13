\docType{data}
\name{Simenv}
\alias{Simenv}
\title{Simenv object - a simulation environment.}
\format{proto object 
 $ class                                        :function (.)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 82 11 82 30 18 37 82 82 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x000000001aa5eab0>  
 $ applyCatAdjustmentToSimframeVarMultipleBinary:function (., binLevelVarnames, desiredProps, propens, printAdj = TRUE,  
    logiset = NULL)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 370 51 446 2 58 9 370 446 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x000000001aa5eab0>  
 $ applyContAdjustmentToSimframe                :function (., varname, iteration, desiredProps, catToContModels, cont.binbreaks,  
    propensities)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 449 35 455 2 42 9 449 455 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x000000001aa5eab0>  
 $ numberOfUnits                                :function (.)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 535 19 537 2 26 9 535 537 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x000000001aa5eab0>  
 $ clone                                        :function (.)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 80 11 80 57 18 64 80 80 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x000000001aa5eab0>  
 $ applyAllFixedOutcomesIfSetToSimframe         :function (.)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 185 42 193 2 49 9 185 193 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x000000001aa5eab0>  
 $ simulate                                     :function (., total_runs = 1)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 483 14 533 2 21 9 483 533 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x000000001aa5eab0>  
 $ generatePreSimulationStats                   :function (., simframe)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 464 32 466 2 39 9 464 466 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x000000001aa5eab0>  
 $ applyAllCatAdjustmentsToSimframe             :function (., iteration, propensities = NULL, print_adj = TRUE, cat.adjustments = .$cat.adjustments)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 150 38 183 2 45 9 150 183 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x000000001aa5eab0>  
 $ applyCatAdjustmentToSimframeVarSingle        :function (., varname, desired_props, propens, print_adj = T, logiset = NULL)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 296 43 318 2 50 9 296 318 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x000000001aa5eab0>  
 $ new                                          :function (., name, simframe, dict, cat.adjustments = list(), modules = list())   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 67 9 78 2 16 9 67 78 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x000000001aa5eab0>  
 $ applyCatAdjustmentToSimframe                 :function (., varnames, desired_props, iteration, propensities, print_adj = TRUE)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 243 34 256 2 41 9 243 256 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x000000001aa5eab0>}
\usage{
  Simenv
}
\description{
  A simulation environment contains everything required to
  perform a simulation. Typically 1 Simenv will be created
  and used to run a base simulation, and additional Simenvs
  will be created to test different scenarios.
}
\details{
  A Simenv consists of a: - a simframe (possibly with
  adjustments to test a scenario) - one or more simulation
  modules (Simmodule). A Simmodule contains outcomes, run
  stats, and runs.averaged for a simulation as well as the
  code to generate them.

  Uses the global environment list variable "propensities"
  when performing categorical adjustment

  This class will be subclassed by specific simulation
  problems which will provide their own simframe,
  Simmodules and adjustments.
}
\keyword{datasets}

