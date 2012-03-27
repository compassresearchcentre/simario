\docType{data}
\name{Simenv}
\alias{Simenv}
\title{Simenv object - a simulation environment.}
\format{proto object 
 $ class                                        :function (.)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 68 11 68 30 18 37 68 68 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x07b24138>  
 $ applyCatAdjustmentToSimframeVarMultipleBinary:function (., binLevelVarnames, desiredProps, propens, printAdj = TRUE)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 210 51 237 2 58 9 210 237 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x07b24138>  
 $ numberOfUnits                                :function (.)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 310 19 312 2 26 9 310 312 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x07b24138>  
 $ clone                                        :function (.)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 66 11 66 57 18 64 66 66 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x07b24138>  
 $ simulate                                     :function (., total_runs = 1)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 262 14 308 2 21 9 262 308 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x07b24138>  
 $ generatePreSimulationStats                   :function (., simframe)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 245 32 247 2 39 9 245 247 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x07b24138>  
 $ applyAllCatAdjustmentsToSimframe             :function (., iteration, propensities, print_adj = TRUE)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 96 38 115 2 45 9 96 115 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x07b24138>  
 $ applyCatAdjustmentToSimframeVarSingle        :function (., varname, desired_props, propens, print_adj = T)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 171 43 179 2 50 9 171 179 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x07b24138>  
 $ new                                          :function (., name, simframe, dict, cat.adjustments = list(),  
    modules = list())   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 54 9 64 2 16 9 54 64 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x07b24138>  
 $ applyCatAdjustmentToSimframe                 :function (., varnames, desired_props, iteration, propensities,  
    print_adj = TRUE)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 135 34 146 2 41 9 135 146 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x07b24138>}
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

