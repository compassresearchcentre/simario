\name{collator_mutiple_lists_mx2}
\alias{collator_mutiple_lists_mx2}
\title{A new version of collator_mutiple_lists_mx() that calls
mean_array_pctile_CIs2()) instead of then 'non-2' version.  This means that
percenatages will be averaged over runs instead of frequencies and the confidence
intervals will be calculated on percentages rather than frequencies.  See documentation
on mean_array_pctile_CIS2() more more details.
Another difference between this function and the original collator_mutiple_lists_mx()
is that this function takes cat.adjustments, dict, and binbreaks arguments.}
\usage{
  collator_mutiple_lists_mx2(runs, CI = TRUE,
    cat.adjustments = NULL, dict, binbreaks = NULL)
}
\description{
  A new version of collator_mutiple_lists_mx() that calls
  mean_array_pctile_CIs2()) instead of then 'non-2'
  version.  This means that percenatages will be averaged
  over runs instead of frequencies and the confidence
  intervals will be calculated on percentages rather than
  frequencies.  See documentation on
  mean_array_pctile_CIS2() more more details. Another
  difference between this function and the original
  collator_mutiple_lists_mx() is that this function takes
  cat.adjustments, dict, and binbreaks arguments.
}

