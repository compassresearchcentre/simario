\docType{data}
\name{Simenv}
\alias{Simenv}
\title{Simenv object - a simulation environment.}
\format{proto object 
 $ adjustSimframeCatLevelVars.list   :function (., desiredProps.list, lvls.list, propens.list, printAdj = TRUE)   
 $ numRuns                           : int 0 
 $ class                             :function (.)   
 $ adjustSimframe                    :function (., iteration, propens.all, printAdj = TRUE)   
 $ envName                           : NULL 
 $ baseTablesGenerate                :function (., simframe)   
 $ adjustSimframeCatLevelVars        :function (., binLevelVarnames, desiredProps, propens, printAdj = TRUE,  
    levelvalues = NULL)   
 $ numCases                          : NULL 
 $ clone                             :function (.)   
 $ simframe                          : NULL 
 $ base.tables                       : list() 
 $ simulate                          :function (., numruns = 1)   
 $ adjustSimframeCatNonLevelVars.list:function (., desiredProps.list, propens.list, printAdj = TRUE)   
 $ dict                              : NULL 
 $ new                               :function (., name, simframe, dict, catadjs = list(level.vars = list(),  
    nonlevel.vars = list()))   
 $ modules                           : list() 
 $ catadjs                           :List of 2}
\usage{
  Simenv
}
\description{
  Contains a simframe, one or more simulation modules, and
  adjustments required to test scenarios. This class will
  be subclassed by specific simulation problems which will
  provide their own simframe, Simmodules and adjustments.
}
\details{
  Typically 1 Simenv will be created and used to run a base
  simulation, and additional Simenvs will be created to
  test different scenarios.
}
\keyword{datasets}

