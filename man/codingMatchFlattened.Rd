\name{codingMatchFlattened}
\alias{codingMatchFlattened}
\title{Returns the category names for the vector of flattened codes.}
\usage{
  codingMatchFlattened(x.flat, varname, grpby.tag)
}
\arguments{
  \item{varname}{identifies the varname coding}

  \item{grpby.tag}{identifies the grping coding, or NULL if
  no grouping coding.}

  \item{x.flat}{a vector of flattened codes. A flattened
  code is in the form "0 1", where the first value is a
  grping code and the second a varname code.  If grpby.tag
  is NULL or NA, then the flattened code will be in the
  form "0", i.e: no grping codes only varname codes.}
}
\description{
  Returns the category names for the vector of flattened
  codes.
}
\examples{
x.flat = c("1 0", "1 1", "2 0", "2 1", "3 0", "3 1"); varname = "z1singleLvl1"; grpby.tag = "r1stchildethn"
 x.flat = c("0","1") ; varname = "z1singleLvl1"; grpby.tag = NULL
 x.flat = c("0", "1", "3", "5") ; varname = "gptotvis" ; grpby.tag = NULL
 x.flat = c("1 0", "2 1", "3 3", "1 0", "3 5") ; varname = "gptotvis" ; grpby.tag = "r1stchildethn"
 codingMatchFlattened(x.flat, varname, grpby.tag)
}

