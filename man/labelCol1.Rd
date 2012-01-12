\name{labelCol1}
\alias{labelCol1}
\title{Set the first column to label.}
\usage{
  labelCol1(x, label, onlyIfNull = TRUE)
}
\arguments{
  \item{x}{object with columns}

  \item{onlyIfNull}{if TRUE, then naming will only occued
  if x has no existing colnames}
}
\description{
  Set the first column to label.
}
\examples{
xnamedlist <- env.base$years1_5$results$means$all
xnamedlist <- results$means$all
x <- xnamedlist$gptotvis
labelCol1FromList(x, "Mean")
}

