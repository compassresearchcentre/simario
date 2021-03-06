% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/Collate.r
\name{collator_means}
\alias{collator_means}
\title{Collate means over multiple runs. Performs the following:}
\usage{
collator_means(runs, dict, ...)
}
\arguments{
\item{runs}{a list of matrices, one matrix per run.}

\item{dict}{Dictionary object. Used to label columns.}

\item{...}{Additional arguments passed to \code{\link{collator_list_mx}}}
}
\value{
a matrix with the averaged values of runs.
}
\description{
\itemize{
  \item Takes mean with confidence intervals using \code{\link{collator_list_mx}}
  \item Labels the result using the dictionary
}
}
\examples{
run1 = structure(matrix(1:6, nrow=3, dimnames=list(1:3, c("F","M"))), meta=c(varname="earnings", grpby.tag="sex"))
run2 = structure(matrix(11:16, nrow=3, dimnames=list(1:3, c("F","M"))), meta=c(varname="earnings", grpby.tag="sex"))

runs <- list(run1=run1,run2=run2)
dict <- dict_example
collator_means(runs, dict)
}
\seealso{
\code{\link{collator_list_mx}}
}

