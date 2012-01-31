\name{strip_lvl_suffix}
\alias{strip_lvl_suffix}
\title{Remove trailing "LvlX" (if any) where X is any character}
\usage{
  strip_lvl_suffix(varname)
}
\arguments{
  \item{varname}{character vector to strip}
}
\description{
  Remove trailing "LvlX" (if any) where X is any character
}
\examples{
\dontrun{
varname <- "fooLvl1"
strip_lvl_suffix(varname)
varname <- "bar"
strip_lvl_suffix(varname)
}
}

