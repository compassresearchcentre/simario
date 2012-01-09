\name{dictLookup}
\alias{dictLookup}
\title{Lookup description of variable x in the dictionary
first determines the name of variable x, then does the lookup}
\usage{
  dictLookup(x)
}
\description{
  Lookup description of variable x in the dictionary first
  determines the name of variable x, then does the lookup
}
\examples{
x <- env.base$years1_5$results$freqs$all$z1msmokeLvl1
x <- env.base$years1_5$results$freqs$all.by.ethnicity$z1msmokeLvl1
dictLookup(x)
dictLookup("burt")
dictLookup(c(1,2))
dictLookup(freqSingle)
dictLookup("single")
dictLookup("don't exist")
dictLookup(runs.mean.freq$base$o.chres)
}

