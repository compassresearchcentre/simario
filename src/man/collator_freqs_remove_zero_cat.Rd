\name{collator_freqs_remove_zero_cat}
\alias{collator_freqs_remove_zero_cat}
\title{Collate frequencies and removes the zero category. Performs the following:}
\usage{
  collator_freqs_remove_zero_cat(runs, dict,
    row.dim.label = "Year", col.dim.label = "")
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
  \itemize{ \item Takes mean without confidence intervals
  using \code{\link{collator_mutiple_lists_mx}} \item
  Labels the result using the dictionary \item Converts
  frequencies to percentages \item Removes the zero
  category }
}
\examples{
\dontrun{
runs <- all_run_results_zipped$freqs[[1]]
runs <- all_run_results_zipped$freqs_by_sex[[1]]
collator_freqs_remove_zero_cat(runs, dict_example)
}
}
\seealso{
  \code{\link{collator_mutiple_lists_mx}}
}

