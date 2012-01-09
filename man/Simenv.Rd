\name{Simenv}
\alias{Simenv}
\docType{data}
\title{
%%   ~~ data name/kind ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data(Simenv)}
\format{
  The format is:
proto object 
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
 $ new                               :function (., name, simframe, catadjs = list(level.vars = list(),  
    nonlevel.vars = list()))   
 $ modules                           : list() 
 $ catadjs                           :List of 2 
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(Simenv)
## maybe str(Simenv) ; plot(Simenv) ...
}
\keyword{datasets}
