\name{finialise.lolmx}
\alias{finialise.lolmx}
\title{Prepare frequency runstats for display by:
 flattening into a 3D array
 calculate percentages
 remove zero category
 label columns}
\usage{
  finialise.lolmx(lol.mx, dict, asPercentages = T,
    removeZeroCategory = T)
}
\arguments{
  \item{lol.mx}{a list of lists of matrices, eg: List of 2
  $ year1:List of 2 ..$ 1: 'table' int [1:2, 1:3] 1 2 3 4 5
  6 ..$ 2: 'table' int [1:2, 1:3] 21 22 23 24 25 26 $
  year2:List of 2 ..$ 1: 'table' int [1:2, 1:3] 31 32 33 34
  35 36 ..$ 2: 'table' int [1:2, 1:3] 41 42 43 44 45 46

  NB: matrices can have different dimensions and are
  aligned first within a list, and then between the lists.}

  \item{dict}{Dictionary object instance}

  \item{asPercentages}{convert values to percentages of
  their grouping (if any)}

  \item{removeZeroCategory}{remove zero category}
}
\value{
  labelled and percentaged array, with z-dim = runs
}
\description{
  Prepare frequency runstats for display by: flattening
  into a 3D array calculate percentages remove zero
  category label columns
}
\examples{
\dontrun{
varname = "z1singleLvl1" ; varname = "gptotvis"
lol.mx <- env.base$modules[[1]]$runstats$freqs$all[[1]]
lol.mx <- env.base$modules$years1_5$runstats$freqs$all$z1singleLvl1
lol.mx <- env.base$modules$years1_5$runstats$freqs$all.by.ethnicity$z1singleLvl1
lol.mx <- env.base$modules$years1_5$runstats$cfreqs$gptotvis

lol.mx <- env.base$modules$years1_5$runstats$freqs$all$sptype

lol.mx <- env.scenario$modules$years1_5$runstats$freqs$all$z1singleLvl1
lol.mx <- env.scenario$modules$years1_5$runstats$freqs$all.by.ethnicity$z1singleLvl1
lol.mx <- env.scenario$modules$years1_5$runstats$cfreqs$houtptot
lol.mx <- env.scenario$modules$years6_13$runstats$cfreqs[["cond"]]

dict <- dict.MELC
dict <- dict_demo

asPercentages = T; removeZeroCategory = T; CI = F
removeZeroCategory = F
finialise.lolmx(lol.mx)
finialise.lolmx(lol.mx, dict, asPercentages, removeZeroCategory, CI)
}
}
\seealso{
  align.by.name.list.mx
}

