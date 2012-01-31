\docType{data}
\name{Simenv}
\alias{Simenv}
\title{Simenv object - a simulation environment.}
\format{proto object 
 $ applyCatAdjustmentsToSimframe                 :function (., iteration, propens.all, print_adj = TRUE)   
 $ class                                         :function (.)   
 $ presim.stats                                  : list() 
 $ cat.adjustments                               : list() 
 $ applyCatAdjustmentsToSimframeVarMultipleBinary:function (., binLevelVarnames, desiredProps, propens, printAdj = TRUE)   
 $ runs_simulated                                : int 0 
 $ numberOfUnits                                 :function (.)   
 $ clone                                         :function (.)   
 $ name                                          : NULL 
 $ simframe                                      : NULL 
 $ simulate                                      :function (., total_runs = 1)   
 $ generatePreSimulationStats                    :function (., simframe)   
 $ dict                                          : NULL 
 $ applyCatAdjustmentsToSimframeVarSingle        :function (., varname, desired_props, propens, print_adj = T)   
 $ new                                           :function (., name, simframe, dict, cat.adjustments = list(level.vars = list(),  
    nonlevel.vars = list()))   
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
  stats, and results for a simulation as well as the code
  to generate them.

  Uses the global environment list variable propens.all
  when performing categorical adjustment

  This class will be subclassed by specific simulation
  problems which will provide their own simframe,
  Simmodules and adjustments.
}
\keyword{datasets}

