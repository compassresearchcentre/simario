% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/support.r
\name{withinfunc}
\alias{withinfunc}
\title{Same as within, but the expr executed is a function.
Unlike within, the func will be executed within data.
Functions will read variables from data, and if <<- is used
then values will be assigned in data IF the variable already exists in data.}
\usage{
withinfunc(data, func, ...)
}
\arguments{
\item{data}{data to use for constructing an environment.  Can be a list or a data frame.}

\item{func}{function to evaluate in data}

\item{...}{arguments to pass to func}
}
\value{
data modified
}
\description{
Same as within, but the expr executed is a function.
Unlike within, the func will be executed within data.
Functions will read variables from data, and if <<- is used
then values will be assigned in data IF the variable already exists in data.
}
\examples{
myvar <- "globalenv"
data <- list(myvar="dataenv")
func <- function() {
	print(myvar)
	myvar <<- "func.changed"
}

args <- list()
data <- withinfunc(data, func)
}

