\name{finialise.lolmx}
\alias{finialise.lolmx}
\title{Prepare run results for display by:
 flattening into a 3D array
 calculate percentages
 remove zero category
 label columns
 and calculate mean across all matrices}
\usage{
  finialise.lolmx(lol.mx, asPercentages = T,
    removeZeroCategory = T, CI = F)
}
\arguments{
  \item{lol.mx}{a list of lists of matrices, eg: List of 2
  $ year1:List of 2 ..$ 1: 'table' int [1:2, 1:3] 1 2 3 4 5
  6 ..$ 2: 'table' int [1:2, 1:3] 21 22 23 24 25 26 $
  year2:List of 2 ..$ 1: 'table' int [1:2, 1:3] 31 32 33 34
  35 36 ..$ 2: 'table' int [1:2, 1:3] 41 42 43 44 45 46

  NB: matrices can have different dimensions and are
  aligned first within a list, and then between the lists.}

  \item{asPercentages}{convert values to percentages of
  their grouping (if any)}

  \item{removeZeroCategory}{remove zero category}

  \item{CI}{if TRUE, will produce confidence intervals on
  the means}
}
\value{
  a matrix of means
}
\description{
  Prepare run results for display by: flattening into a 3D
  array calculate percentages remove zero category label
  columns and calculate mean across all matrices
}
\examples{
varname = "z1singleLvl1" ; varname = "gptotvis"
lol.mx <- env.base$modules$years1_5$runs$freqs$all$z1singleLvl1
lol.mx <- env.base$modules$years1_5$runs$freqs$all.by.ethnicity$z1singleLvl1
lol.mx <- env.base$modules$years1_5$runs$cfreqs$gptotvis

lol.mx <- env.base$modules$years1_5$runs$freqs$all$sptype

lol.mx <- env.scenario$modules$years1_5$runs$freqs$all$z1singleLvl1
lol.mx <- env.scenario$modules$years1_5$runs$freqs$all.by.ethnicity$z1singleLvl1
lol.mx <- env.scenario$modules$years1_5$runs$cfreqs$gptotvis

asPercentages = T; removeZeroCategory = T; CI = F
removeZeroCategory = F
finialise.lolmx(lol.mx)
finialise.lolmx(lol.mx, asPercentages, removeZeroCategory, CI)
}
\seealso{
  align.by.name.list.mx
}

