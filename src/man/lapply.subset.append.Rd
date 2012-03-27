\name{lapply.subset.append}
\alias{lapply.subset.append}
\title{Execute a function over a subset of a list, and merges the results into
an existing list.}
\usage{
  lapply.subset.append(results.list, X,
    indices = names(results.list), simplify = TRUE, .FUN,
    ...)
}
\arguments{
  \item{results.list}{an existing list, eg:

  List of 2 $ gptotvis: num [1:5, 1, 1] 5.82 4.24 4.09 3.93
  4.11 ... $ hadmtot : num [1:5, 1, 1] 0.2344 0.1014 0.1163
  0.0847 0.0921 ...}

  \item{X}{vector, list or dataframe on which to execute
  .FUN}

  \item{indices}{elements of X to apply .FUN to. If NULL,
  returns results.list.  Defaults to names(results.list).}

  \item{simplify}{if TRUE, then results will be added to
  the Z dimension of the matrices in results.list. If
  FALSE, then results will be added as a extra list element
  to each element of results.list}

  \item{.FUN}{.FUN to execute over elements of X}

  \item{...}{additional arguments to .FUN}
}
\description{
  Execute a function over a subset of a list, and merges
  the results into an existing list.
}
\examples{
\dontrun{
X <- env.base$years6_13$outcomes
results.list <- env.base$years6_13$runstats$summaries
lapply.subset.append (results.list, X, .FUN=summary_mx_cols)

X <- env.base$years1_5$outcomes
lol <- env.base$years1_5$run_results$confreqs

lol <- env.base$years6_13$runstats$summaries
X <- env.base$years6_13$outcomes

X <- outcomes
.FUN=table_mx_cols

lapply.subset.append (lol, X, simplify = FALSE, .FUN=wtdtablecols)


lapply.subset.append(results.list=lol.x, X=X, simplify=simplify, .FUN=.FUN, logiset=lol.a$logiset)

results.list <- run_results$freqs; X <- outcomes; indices=names(results.list); simplify = FALSE; .FUN=table_mx_cols
lapply.subset.append (run_results$freqs, outcomes, simplify = FALSE, .FUN=table_mx_cols)

results.list <- run_results$confreqs; X <- outcomes; indices=names(results.list); simplify = FALSE; .FUN=wtdtablecols
lapply.subset.append (run_results$confreqs, outcomes, simplify = FALSE, .FUN=wtdtablecols, wgts=outcomes[[wgtsname]])

results.list <- runstats$summaries; X <- outcomes; indices=names(results.list); simplify = FALSE; .FUN=quantile_mx_cols
lapply.subset.append (runstats$summaries, outcomes, .FUN=quantile_mx_cols, probs=seq(0.2,1,0.2))

results.list <-  run_results$freqs.by.ethnicity; X <- outcomes; indices=names(results.list); simplify = FALSE; .FUN=table_mx_cols
lapply.subset.append (run_results$freqs.by.ethnicity, outcomes, simplify = FALSE, .FUN=table_mx_cols, grpby=outcomes$r1stchildethn, grpby.tag="r1stchildethn")

results.list <- mxlist; X <- xframe ; indices=names(results.list); simplify = TRUE ; .FUN = wtdmeancols.lbl2
lapply.subset.append (results.list, X, indices, simplify, .FUN, logiset=logiset, grpby=grpby, grpby.tag = grpby.tag)
}
}

