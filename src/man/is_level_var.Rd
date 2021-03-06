% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/CategoricalAdjustment.r
\name{is_level_var}
\alias{is_level_var}
\title{Check if each element of a character vector has the trailing "LvlX" (if any)
where X is any character}
\usage{
is_level_var(varname)
}
\arguments{
\item{varname}{character vector to check}
}
\value{
a vector of logical value
}
\description{
Check if each element of a character vector has the trailing "LvlX" (if any)
where X is any character
}
\examples{
\dontrun{
varname <- c("fooLvl1", "bar")
is_level_var(varname)
}
}

