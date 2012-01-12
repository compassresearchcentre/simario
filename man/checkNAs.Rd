\name{checkNAs}
\alias{checkNAs}
\title{Check a list for any NAs, producing error if they exist, otherwise
silently exit.}
\usage{
  checkNAs(xlist)
}
\description{
  Check a list for any NAs, producing error if they exist,
  otherwise silently exit.
}
\examples{
foo <- c(1,NA,3)
 names(foo) <- c("a","b","c")
 xlist <- foo
	checkNAs(foo)
	checkNAs(mpropens)
}

