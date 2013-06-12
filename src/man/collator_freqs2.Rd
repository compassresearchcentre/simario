\name{collator_freqs2}
\alias{collator_freqs2}
\title{A new version of collator_freqs() that calls collator_mutiple_lists_mx2() (and then
mean_array_pctile_CIs2()) instead of then 'non-2' versions.  This means that
percenatages will be averaged over runs instead of frequencies and the confidence
intervals will be calculated on percentages rather than frequencies.  See documentation
on mean_array_pctile_CIS2() more more details.
In contrast to the original collator_freqs() this function does not take a numbers
argument.  It will only produce percentages and not raw numbers or frequencies.
Another difference between this function and the original collator_freqs() is that
this function takes cat.adjustment and binbreaks arguments.  These are sent to
collator_mutiple_lists_mx2().}
\usage{
  collator_freqs2(runs, dict, row.dim.label = "Year",
    col.dim.label = "", CI = FALSE, cat.adjustments = NULL,
    binbreaks = NULL)
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

  \item{cat.adjustments}{The cat.adjustments list
  containing the cat.adjustments for multiple variables.
  Within the function the specific cat.adjustments for the
  variable of interest are extracted from the list in
  further embedded functions.  Either cat.adjustments or
  binbreaks are needed if frequencies of a continuous
  variable are being requested.}

  \item{dict}{the specific project dictionary}

  \item{binbreaks}{The binbreaks for the specific outcome
  variable.  Either binbreaks or cat.adjustments may be
  provided to the function.}
}
\description{
  A new version of collator_freqs() that calls
  collator_mutiple_lists_mx2() (and then
  mean_array_pctile_CIs2()) instead of then 'non-2'
  versions.  This means that percenatages will be averaged
  over runs instead of frequencies and the confidence
  intervals will be calculated on percentages rather than
  frequencies.  See documentation on
  mean_array_pctile_CIS2() more more details. In contrast
  to the original collator_freqs() this function does not
  take a numbers argument.  It will only produce
  percentages and not raw numbers or frequencies. Another
  difference between this function and the original
  collator_freqs() is that this function takes
  cat.adjustment and binbreaks arguments.  These are sent
  to collator_mutiple_lists_mx2().
}

