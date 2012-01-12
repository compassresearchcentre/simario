\name{labelFlattenedArrayCols}
\alias{labelFlattenedArrayCols}
\title{Label columns of a 3D array that has flattened codes for colnames.}
\usage{
  labelFlattenedArrayCols(xa, dict,
    varname = attr(xa, "meta")["varname"],
    grpby.tag = attr(xa, "meta")["grpby.tag"],
    removeZeroCategory = T)
}
\arguments{
  \item{xa}{3D array with flattened codes for colnames}

  \item{varname}{category variable name of this 3D array}

  \item{grpby.tag}{grouping of flattened codes if any, else
  NULL or NA}

  \item{removeZeroCategory}{remove zero category}
}
\value{
  3D array with relabled columns and dropped zero category
  (if requested)
}
\description{
  Label columns of a 3D array that has flattened codes for
  colnames.
}
\examples{
xa <- lol.mx.array

lol.mx <- env.base$modules$years1_5$runs$freqs$all$z1singleLvl1
lol.mx <- env.scenario$modules$years1_5$runs$freqs$all$z1singleLvl1
lol.mx <- env.base$modules$years1_5$runs$freqs$all.by.ethnicity$z1singleLvl1
lol.mx <- env.base$modules$years1_5$runs$cfreqs$gptotvis
xa <- flatten.lolmx(lol.mx)

removeZeroCategory = F
labelFlattenedArrayCols(xa, removeZeroCategory=removeZeroCategory)
}

