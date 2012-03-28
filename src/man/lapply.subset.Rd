\name{lapply.subset}
\alias{lapply.subset}
\title{Executes .FUN on the specified varnames in X and return the results as a list.}
\usage{
  lapply.subset(X, indices, .FUN, ...)
}
\arguments{
  \item{X}{a vector (atomic or list) or an expression
  object. Other objects (including classed objects) will be
  coerced by base::as.list.}

  \item{indices}{indices, either numeric or names, of
  elements in X for which to apply .FUN, or NULL to apply
  to all elements of X}

  \item{.FUN}{function}

  \item{...}{optional arguments to .FUN.}
}
\value{
  results of .FUN applied to X as a list
}
\description{
  Executes .FUN on the specified varnames in X and return
  the results as a list.
}
\examples{
\dontrun{
X <- env.base$years1_5$outcomes
indices <- names(env.base$years1_5$run_results$confreqs)
.FUN <- wtdtablecols

X <- env.base$years6_13$outcomes
indices <- names(env.base$years6_13$runstats$means$all)
.FUN <- summary_mx_cols


#' lapply.subset(X, indices, .FUN, ...)
results.list <-  run_results$freqs.by.ethnicity; X <- outcomes; indices=names(results.list); .FUN=table_mx_cols
results <- lapply.subset (X, indices, .FUN, grpby.tag="r1stchildethn")


lapply.subset (X, indices, .FUN, logiset=logiset, grpby=grpby, grpby.tag = grpby.tag)

results <- lapply.subset (X, indices, .FUN)
}
}

