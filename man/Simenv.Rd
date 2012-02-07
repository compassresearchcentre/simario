\docType{data}
\name{Simenv}
\alias{Simenv}
\title{Simenv object - a simulation environment.}
\format{proto object 
 $ class                                        :function (.)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 72 11 72 30 18 37 72 72 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x092c766c>  
 $ presim.stats                                 : list() 
 $ cat.adjustments                              : list() 
 $ applyCatAdjustmentToSimframeVarMultipleBinary:function (., binLevelVarnames, desiredProps, propens, printAdj = TRUE)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 211 51 238 2 58 9 211 238 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x092c766c>  
 $ numberOfUnits                                :function (.)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 294 19 296 2 26 9 294 296 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x092c766c>  
 $ clone                                        :function (.)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 70 11 70 57 18 64 70 70 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x092c766c>  
 $ name                                         : NULL 
 $ simframe                                     : NULL 
 $ simulate                                     :function (., total_runs = 1)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 261 14 292 2 21 9 261 292 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x092c766c>  
 $ generatePreSimulationStats                   :function (., simframe)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 246 32 248 2 39 9 246 248 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x092c766c>  
 $ dict                                         : NULL 
 $ applyAllCatAdjustmentsToSimframe             :function (., iteration, propensities, print_adj = TRUE)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 100 38 118 2 45 9 100 118 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x092c766c>  
 $ num_runs_simulated                           : int 0 
 $ applyCatAdjustmentToSimframeVarSingle        :function (., varname, desired_props, propens, print_adj = T)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 172 43 180 2 50 9 172 180 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x092c766c>  
 $ new                                          :function (., name, simframe, dict, cat.adjustments = list(),  
    modules = list())   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 58 9 68 2 16 9 58 68 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x092c766c>  
 $ applyCatAdjustmentToSimframe                 :function (., varnames, desired_props, iteration, propensities,  
    print_adj = TRUE)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 136 34 147 2 41 9 136 147 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x092c766c>  
 $ modules                                      : list()}
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

