\name{labelColumnCodes}
\alias{labelColumnCodes}
\title{Label columns of a 3D array with the codings of the specified varname.}
\usage{
  labelColumnCodes(x, dict, varname)
}
\arguments{
  \item{x}{vector/array with a column for each category,
  ordered}

  \item{dict}{a Dictionary proto object}

  \item{varname}{categorical variable name. The codings for
  this variable are applied as column names.}
}
\value{
  x with column names that use the codings of varname, and
  a column label that is the decsription of varname.
}
\description{
  Label columns of a 3D array with the codings of the
  specified varname.
}
\examples{
\dontrun{
x <- structure(matrix(1:2, nrow=1, dimnames=list(1, c("0","1"))), meta=c("grpby.tag"="z1gender"))
x <- structure(matrix(1:6, nrow=1, dimnames=list(1, c("0 Mean","0 Lower","0 Upper","1 Mean","1 Lower","1 Upper"))), meta=c("grpby.tag"="z1gender"))

x <- env.base$modules$years1_5$runs.averaged$means$all.by.gender$kids
x <- env.scenario$modules$years1_5$runs.averaged$means$all.by.gender$kids
x <- env.base$modules$years1_5$runs.averaged$means$all$kids
x <- env.base$modules$years1_5$runs.averaged$means$all.by.gender$gptotvis
varname=attr(x, "meta")["grpby.tag"]
dict <- dict.MELC
labelColumnCodes(x, dict, varname)
}
}

