\name{collator_histogram}
\alias{collator_histogram}
\title{Collate frequencies for variables without fixed category codings specified
in the dictionary.}
\usage{
  collator_histogram(runs, dict, row.dim.label = "Year",
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
  codings specified in the dictionary.
}
\seealso{
  \code{\link{collator_mutiple_lists_mx}}
}

