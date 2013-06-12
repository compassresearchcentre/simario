\name{labelCols.list}
\alias{labelCols.list}
\title{Set the colnames on each object in a list. By default
will only set if no exiting colnames (see onlyIfNull param).}
\usage{
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
}
\description{
  Set the colnames on each object in a list. By default
  will only set if no exiting colnames (see onlyIfNull
  param).
}
\examples{
\dontrun{
xlist <- env.base$years1_5$run_results_collated$means
xlist <- run_results_collated$means
x <- xlist$gptotvis
labelCols.list(xlist, "Mean")
}
}

