\docType{data}
\name{Simmodule}
\alias{Simmodule}
\title{Simulation module object. Contains a user-specified function that generates outcomes.
Run results are generated from outcomes at the end of the run, and final simulation
results are generated from all runs at the end of the simulation.}
\format{proto object 
 $ runs            :List of 5 
 $ class           :function (.)   
 $ simulateOutcomes:function (., simenv)   
 $ outcomes        : list() 
 $ clone           :function (.)   
 $ name            : NULL 
 $ appendRunResults:function (.)   
 $ result          : list() 
 $ calcFinalResults:function (., simenv)   
 $ new             :function (., name, freqvars, cfreqvars, meanvars, freqs.args,  
    means.args)}
\usage{
  Simmodule
}
\description{
  Simulation module object. Contains a user-specified
  function that generates outcomes. Run results are
  generated from outcomes at the end of the run, and final
  simulation results are generated from all runs at the end
  of the simulation.
}
\keyword{datasets}

