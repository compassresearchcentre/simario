\name{mean.list.var.run.mx}
\alias{mean.list.var.run.mx}
\title{Get the column (category) values (or as a percentage) of the mean of all runs in list.var.run.mx
combining all year matrices into a single matrix.}
\usage{
  mean.list.var.run.mx(list.var.run.mx,
    removeZeroCategory = T, asPercentages = T)
}
\arguments{
  \item{list.var.run.mx}{a list of variable elements,
  containing run elements, containing year matrices.  Year
  matrices are frequency tables, columns are the grouped by
  elements and rows the categories

  eg:

  > str(list.var.run.mx) List of 13 $ msmoke :List of 2 ..$
  :List of 5 .. ..$ 1: 'table' int [1:2, 1:3] 646 284 23 12
  52 58}

  \item{removeZeroCategory}{if TRUE, remove all category
  (column) that is named "0"}

  \item{asPercentages}{if TRUE, return the values as a
  column percentage}
}
\value{
  a list of matrices containing the calculated proportion
  of the mean of all runs
}
\description{
  For each list in list.var.run.mx: *take the mean,
  combining multiple lists of matrices into a single list
  of matrices. *flatten each matrix into a single row,
  calculating proportions and removing 0 category if
  requested *combine the rows (ie. iterations) into a
  single matrix
}
\examples{
. <- env.base$modules$years1_5 ; . <- env.scenario$modules$years1_5
list.var.run.mx <- .$runs$cfreqs ;  removeZeroCategory = F; asPercentages = F
list.var.run.mx <- env.base$modules$years1_5$runs$cfreqs ;  removeZeroCategory = F
list.var.run.mx <- env.base$modules$years1_5$runs$freqs$all
list.var.run.mx <- .$runs$freqs$all.by.ethnicity

removeZeroCategory = TRUE ; removeZeroCategory = F
list.var.run.mx.mean.flat.combined <- mean.list.var.run.mx ( list.var.run.mx )
list.var.run.mx.mean.flat.combined <- mean.list.var.run.mx ( list.var.run.mx , removeZeroCategory)
}

