\name{lapply.subset.append.lol.args}
\alias{lapply.subset.append.lol.args}
\title{Apply lapply.subset.append to each element of lol, using the arguments from
the parallel list lol.args and the function .FUN.}
\usage{
  lapply.subset.append.lol.args(X, lol,
    lol.args = attr(lol, "args.list"), simplify = TRUE,
    .FUN)
}
\arguments{
  \item{X}{vector, list or dataframe on which to execute
  .FUN}

  \item{lol}{a list of lists containing matrix elements}

  \item{lol.args}{For each list in lol, there is a set of
  arguments that are supplied to .FUN.  Each matrix in the
  list will use the arguments from its corresponding
  lol.args list.  e.g: .FUN will be called on matrices in
  lol[[1]] with arguments supplied in lol.args[[1]]
  Defaults to a "args.list" attribute on lol.}

  \item{simplify}{if TRUE, then results will be added to
  the Z dimension of the matrix elements in lol. If FALSE,
  then results will be added as a extra list element to
  each matrix elements of lol}

  \item{.FUN}{.FUN to execute over elements of X}
}
\description{
  Apply lapply.subset.append to each element of lol, using
  the arguments from the parallel list lol.args and the
  function .FUN.
}
\examples{
\dontrun{
X <- outcomes_wtotals
lol <- .$runs$means
lol.args <- attr(lol, "args.list")
.FUN <- wtdmeancols

X <- env.base$years1_5$outcomes
lol <- env.base$years1_5$runs$means
lol.args <- attr(lol, "args.list")
.FUN <- wtdmeancols.lbl

X <- env.base$modules[[1]]$outcomes
lol <- list(all=namedList("gptotvis", "hadmtot"), females.by.ethnicity=namedList("gptotvis", "hadmtot"))
lol.args <- list(all=NULL, females.by.ethnicity=list(logiset=childsets$females, grpby=children$r1stchildethn, grpby.tag="r1stchildethn"))
attr(lol, "args.list") <- lol.args
.FUN <- wtdmeancols.lbl
}

outcome1 <- structure(matrix(1:8, ncol=2), varname="outcome1")
outcome2 <- structure(matrix(11:18, ncol=2), varname="outcome2")
X <- list(outcome1=outcome1, outcome2=outcome2)
lol <- list(all=namedList("outcome1", "outcome2"), females.by.ethnicity=namedList("outcome1", "outcome2"))
lol.args <- list(all=NULL, females.by.ethnicity=list(logiset=c(TRUE,TRUE,TRUE,FALSE), grpby=c("E","E","O","O"), grpby.tag="ethn"))
simplify = TRUE
attr(lol, "args.list") <- lol.args
.FUN <- wtdmeancols

lapply.subset.append.lol.args(X, lol, lol.args, simplify=simplify, .FUN)
wtdmeancols(outcome1)
wtdmeancols(outcome1, logiset=lol.args$females.by.ethnicity$logiset, grpby=lol.args$females.by.ethnicity$grpby, grpby.tag="ethn")
}

