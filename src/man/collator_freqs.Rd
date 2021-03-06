% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/Collate.r
\name{collator_freqs}
\alias{collator_freqs}
\title{Collate frequencies.}
\usage{
collator_freqs(runs, dict, row.dim.label = "Year", col.dim.label = "",
  numbers = FALSE, CI = FALSE)
}
\arguments{
\item{runs}{a list of lists of matrices, one inner list per run.
 Each inner list may have any number of matrices,
 and each matrix may have a different sets of rows or columns.
 The matrices will be flattened into rows.}

\item{dict}{Dictionary object. Used to label columns.}

\item{row.dim.label}{name of the entire row dimension}

\item{col.dim.label}{name of the entire col dimension}

\item{numbers}{If TRUE, it produces frequency table. Otherwise, it will return
 percentage table.}

\item{CI}{if TRUE and length(runs) > 1, lower and upper confidence intervals
 are returned in additional columns}
}
\value{
a matrix of collated result for each iteration
}
\description{
Performs the following:
\itemize{
  \item Takes mean without confidence intervals using \code{\link{collator_mutiple_lists_mx}}
  \item Labels the result using the dictionary
  \item Converts frequencies to percentages
  \item Labels the output
}
}
\examples{
run1_mx1 = matrix(1:2, nrow=1, dimnames=list(1, c("F","M")))
run1_mx2 = matrix(1:4, nrow=2, dimnames=list(1:2, c("F","M")), byrow = TRUE)
run1 = structure(list(run1_mx1, run1_mx2), meta=c(varname="disability_state", grpby.tag="sex"))
run2_mx1 = matrix(11:12, nrow=1, dimnames=list(1, c("F","M")))
run2_mx2 = matrix(11:14, nrow=2, dimnames=list(3:4, c("F","M")), byrow = TRUE)
run2 = structure(list(run2_mx1, run2_mx2), meta=c(varname="disability_state", grpby.tag="sex"))

runs <- list(run1=run1,run2=run2)
dict <- dict_example
collator_freqs(runs, dict)
collator_freqs(runs, dict, numbers=TRUE)
}
\seealso{
\code{\link{collator_mutiple_lists_mx}}
}

