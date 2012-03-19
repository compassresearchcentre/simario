\name{as.matrixFromList}
\alias{as.matrixFromList}
\title{Converts a list of same length vectors to a matrix.
Unlike as.matrix.default, it unlists first and
uses names from the first list element in the result.}
\usage{
  as.matrixFromList(xlist, byrow = TRUE)
}
\arguments{
  \item{xlist}{a list of same length vectors}

  \item{byrow}{if TRUE, each list element becomes a row in
  the matrix else, each list element becomes a column in
  the matrix}
}
\description{
  Converts a list of same length vectors to a matrix.
  Unlike as.matrix.default, it unlists first and uses names
  from the first list element in the result.
}
\examples{
#xlist <- runstats.mean.cfreq$gpmorb

 xlist <- list(a=c("1st"=1,"2nd"=2, "3rd"=3), b=c(11:13))

 as.matrix.default(xlist)
 as.matrix(xlist)
 as.matrixFromList(xlist)
 as.matrixFromList(xlist, byrow = FALSE)
}

