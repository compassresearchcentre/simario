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
  \item{xa}{3D array with flattened codes for colnames.  A
  flattened code is in the form "0 1", where the first
  value is a grping code and the second a varname code.  If
  grpby.tag is NULL or NA, then the flattened code will be
  in the form "0", i.e: no grping codes only varname
  codes.}

  \item{dict}{a Dictionary proto object}

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
\dontrun{
xa <- lol.mx.array

lol.mx <- env.base$modules$years1_5$runstats$freqs$all$z1singleLvl1
lol.mx <- env.scenario$modules$years1_5$runstats$freqs$all$z1singleLvl1
lol.mx <- env.base$modules$years1_5$runstats$freqs$all.by.ethnicity$z1singleLvl1
lol.mx <- env.base$modules$years1_5$runstats$cfreqs$gptotvis
xa <- flatten.lolmx(lol.mx)
dict <- dict.MELC
removeZeroCategory = F

xa <- r$all.by.gender$householdsize

labelFlattenedArrayCols(xa, dict=dict, removeZeroCategory=removeZeroCategory)
}
}

