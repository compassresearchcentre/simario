\docType{data}
\name{Simenv}
\alias{Simenv}
\title{Simenv object - a simulation environment.}
\format{proto object 
 $ class                                        :function (.)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 82 11 82 30 18 37 82 82 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x000000001430c5a8>  
 $ applyCatAdjustmentToSimframeVarMultipleBinary:function (., binLevelVarnames, desiredProps, propens, printAdj = TRUE,  
    logiset = NULL)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 359 51 428 2 58 9 359 428 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x000000001430c5a8>  
 $ numberOfUnits                                :function (.)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 507 19 509 2 26 9 507 509 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x000000001430c5a8>  
 $ clone                                        :function (.)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 80 11 80 57 18 64 80 80 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x000000001430c5a8>  
 $ applyAllFixedOutcomesIfSetToSimframe         :function (.)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 174 42 182 2 49 9 174 182 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x000000001430c5a8>  
 $ simulate                                     :function (., total_runs = 1)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 455 14 505 2 21 9 455 505 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x000000001430c5a8>  
 $ generatePreSimulationStats                   :function (., simframe)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 436 32 438 2 39 9 436 438 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x000000001430c5a8>  
 $ applyAllCatAdjustmentsToSimframe             :function (., iteration, propensities, print_adj = TRUE, cat.adjustments = .$cat.adjustments)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 149 38 172 2 45 9 149 172 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x000000001430c5a8>  
 $ applyCatAdjustmentToSimframeVarSingle        :function (., varname, desired_props, propens, print_adj = T, logiset = NULL)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 285 43 307 2 50 9 285 307 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x000000001430c5a8>  
 $ new                                          :function (., name, simframe, dict, cat.adjustments = list(), modules = list())   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 67 9 78 2 16 9 67 78 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x000000001430c5a8>  
 $ applyCatAdjustmentToSimframe                 :function (., varnames, desired_props, iteration, propensities, print_adj = TRUE)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 232 34 245 2 41 9 232 245 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x000000001430c5a8>}
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

