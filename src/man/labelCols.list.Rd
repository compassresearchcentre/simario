\name{labelCols.list}
\alias{labelCols.list}
\title{Set the colnames on each object in a list.}
\usage{
  labelCols.list(xlist, xlabels = names(xlist),
    onlyIfNull = TRUE)

  labelCols.list(xlist, xlabels = names(xlist),
    onlyIfNull = TRUE)
}
\arguments{
  \item{xlist}{list}

  \item{xlabels}{labels, defaults to names(xlist) if
  length(xlist) > length(labels) then the labels are
  repeated}

  \item{onlyIfNull}{if TRUE, then naming will only occur if
  the xlist item has no existing colnames}

  \item{xlist}{list}

  \item{xlabels}{labels, defaults to names(xlist) if
  length(xlist) > length(labels) then the labels are
  repeated}

  \item{onlyIfNull}{if TRUE, then naming will only occur if
  the xlist item has no existing colnames}
}
\description{
  Set the colnames on each object in a list.

  Set the colnames on each object in a list.
}
\examples{
\dontrun{
xlist <- env.base$years1_5$runs.averaged$means$all
xlist <- runs.averaged$means$all
x <- xlist$gptotvis
labelCols.list(xlist, "Mean")
}
\dontrun{
xlist <- env.base$years1_5$runs.averaged$means$all
xlist <- runs.averaged$means$all
x <- xlist$gptotvis
labelCols.list(xlist, "Mean")
}
}

