\docType{data}
\name{Simenv}
\alias{Simenv}
\title{Simenv object - a simulation environment.}
\format{proto object 
 $ class                                        :function (.)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 71 11 71 30 18 37 71 71 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x000000001132ece0>  
 $ presim.stats                                 : list() 
 $ cat.adjustments                              : list() 
 $ applyCatAdjustmentToSimframeVarMultipleBinary:function (., binLevelVarnames, desiredProps, propens, printAdj = TRUE)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 213 51 240 2 58 9 213 240 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x000000001132ece0>  
 $ numberOfUnits                                :function (.)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 300 19 302 2 26 9 300 302 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x000000001132ece0>  
 $ clone                                        :function (.)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 69 11 69 57 18 64 69 69 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x000000001132ece0>  
 $ name                                         : NULL 
 $ simframe                                     : NULL 
 $ simulate                                     :function (., total_runs = 1)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 265 14 298 2 21 9 265 298 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x000000001132ece0>  
 $ generatePreSimulationStats                   :function (., simframe)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 248 32 250 2 39 9 248 250 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x000000001132ece0>  
 $ dict                                         : NULL 
 $ applyAllCatAdjustmentsToSimframe             :function (., iteration, propensities, print_adj = TRUE)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 99 38 118 2 45 9 99 118 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x000000001132ece0>  
 $ num_runs_simulated                           : int 0 
 $ applyCatAdjustmentToSimframeVarSingle        :function (., varname, desired_props, propens, print_adj = T)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 174 43 182 2 50 9 174 182 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x000000001132ece0>  
 $ new                                          :function (., name, simframe, dict, cat.adjustments = list(),  
    modules = list())   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 57 9 67 2 16 9 57 67 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x000000001132ece0>  
 $ applyCatAdjustmentToSimframe                 :function (., varnames, desired_props, iteration, propensities,  
    print_adj = TRUE)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 138 34 149 2 41 9 138 149 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x000000001132ece0>  
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

