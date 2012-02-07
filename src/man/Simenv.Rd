\docType{data}
\name{Simenv}
\alias{Simenv}
\title{Simenv object - a simulation environment.}
\format{proto object 
 $ class                                        :function (.)   
 $ presim.stats                                 : list() 
 $ cat.adjustments                              : list() 
 $ applyCatAdjustmentToSimframeVarMultipleBinary:function (., binLevelVarnames, desiredProps, propens, printAdj = TRUE)   
 $ numberOfUnits                                :function (.)   
 $ clone                                        :function (.)   
 $ name                                         : NULL 
 $ simframe                                     : NULL 
 $ simulate                                     :function (., total_runs = 1)   
 $ generatePreSimulationStats                   :function (., simframe)   
 $ dict                                         : NULL 
 $ applyAllCatAdjustmentsToSimframe             :function (., iteration, propensities, print_adj = TRUE)   
 $ num_runs_simulated                           : int 0 
 $ applyCatAdjustmentToSimframeVarSingle        :function (., varname, desired_props, propens, print_adj = T)   
 $ new                                          :function (., name, simframe, dict, cat.adjustments = list(),  
    modules = list())   
 $ applyCatAdjustmentToSimframe                 :function (., varnames, desired_props, iteration, propensities,  
    print_adj = TRUE)   
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

