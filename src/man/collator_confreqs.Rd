\name{collator_confreqs}
\alias{collator_confreqs}
\title{Collate frequencies for variables without fixed category codings specified
in the dictionary. Instead these variables will have a large variable number of
categories.}
\usage{
  collator_confreqs(runs, dict, row.dim.label = "Year",
    col.dim.label = "")
}
\arguments{
  \item{runs}{a list of lists of matrices, one inner list
  per run.  Each inner list may have any number of
  matrices, and each matrix may have a different sets of
  rows or columns.  The matrices will be flattened into
  rows.}

  \item{dict}{Dictionary object. Used to label columns.}

  \item{row.dim.label}{name of the entire row dimension}

  \item{col.dim.label}{name of the entire col dimension}
}
\description{
  Collate frequencies for variables without fixed category
  codings specified in the dictionary. Instead these
  variables will have a large variable number of
  categories.
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
collator_confreqs(runs, dict)
}
\seealso{
  \code{\link{collator_mutiple_lists_mx}}
}

