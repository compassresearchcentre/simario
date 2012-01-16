\docType{data}
\name{Simmodule}
\alias{Simmodule}
\title{Simulation module object. Contains a user-specified function that generates outcomes.}
\format{proto object 
 $ runs            :List of 5 
 $ class           :function (.)   
 $ simulateOutcomes:function (., simenv)   
 $ outcomes        : list() 
 $ clone           :function (.)   
 $ name            : NULL 
 $ result          : list() 
 $ calcFinalResults:function (., simenv)   
 $ new             :function (., name, freqvars, cfreqvars, meanvars, freqs.args,  
    means.args)   
 $ appendRunStats  :function (.)}
\usage{
  Simmodule
}
\description{
  Run stats are generated from outcomes at the end of the
  run, and final simulation results are generated from all
  runs at the end of the simulation.
}
\keyword{datasets}

