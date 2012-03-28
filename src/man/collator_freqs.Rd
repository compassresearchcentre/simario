\name{collator_freqs}
\alias{collator_freqs}
\title{Collate frequencies for variables with fixed category codings specified
in the dictionary.}
\usage{
  collator_freqs(runs, dict, row.dim.label = "Year",
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
  Collate frequencies for variables with fixed category
  codings specified in the dictionary.
}
\examples{
\dontrun{
runs <- all_run_results_zipped$freqs[[1]]
runs <- all_run_results_zipped$freqs_by_sex[[1]]
collator_freqs(runs, dict_example)
}
}
\seealso{
  \code{\link{collator_mutiple_lists_mx}}
}

