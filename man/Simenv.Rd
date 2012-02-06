\docType{data}
\name{Simenv}
\alias{Simenv}
\title{Simenv object - a simulation environment.}
\format{proto object 
 $ applyCatAdjustmentsToSimframe                 :function (., iteration, propensities, print_adj = TRUE)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 100 35 126 2 42 9 100 126 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x0845f638>  
 $ class                                         :function (.)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 72 11 72 30 18 37 72 72 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x0845f638>  
 $ presim.stats                                  : list() 
 $ cat.adjustments                               : list() 
 $ applyCatAdjustmentsToSimframeVarMultipleBinary:function (., binLevelVarnames, desiredProps, propens, printAdj = TRUE)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 190 52 217 2 59 9 190 217 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x0845f638>  
 $ numberOfUnits                                 :function (.)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 273 19 275 2 26 9 273 275 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x0845f638>  
 $ clone                                         :function (.)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 70 11 70 57 18 64 70 70 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x0845f638>  
 $ name                                          : NULL 
 $ simframe                                      : NULL 
 $ simulate                                      :function (., total_runs = 1)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 240 14 271 2 21 9 240 271 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x0845f638>  
 $ generatePreSimulationStats                    :function (., simframe)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 225 32 227 2 39 9 225 227 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x0845f638>  
 $ dict                                          : NULL 
 $ num_runs_simulated                            : int 0 
 $ applyCatAdjustmentsToSimframeVarSingle        :function (., varname, desired_props, propens, print_adj = T)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 151 44 159 2 51 9 151 159 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x0845f638>  
 $ new                                           :function (., name, simframe, dict, cat.adjustments = list(),  
    modules = list())   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 58 9 68 2 16 9 58 68 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x0845f638>  
 $ modules                                       : list()}
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

