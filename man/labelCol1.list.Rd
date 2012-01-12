\name{labelCol1.list}
\alias{labelCol1.list}
\title{Set name of each object's 1st column to the
object's name in the named list.}
\usage{
  labelCol1.list(xlist, xlabels = names(xlist),
    onlyIfNull = TRUE)
}
\arguments{
  \item{xlist}{list}

  \item{xlabels}{labels, defaults to names(xlist) if
  length(xlist) > length(labels) then the labels are
  repeated}

  \item{onlyIfNull}{if TRUE, then naming will only occur if
  the xlist item has no existing colnames}
}
\description{
  Set name of each object's 1st column to the object's name
  in the named list.
}
\examples{
xlist <- env.base$years1_5$results$means$all
xlist <- results$means$all
x <- xlist$gptotvis
labelCol1.list(xlist, "Mean")
}

