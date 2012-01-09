\name{lapply.subset.append.lol.args}
\alias{lapply.subset.append.lol.args}
\title{Apply lapply.subset.append to each element of lol, using the arguments from
the parallel list lol.args and the function .FUN.}
\usage{
  lapply.subset.append.lol.args(X, lol,
    lol.args = attr(lol, "args.list"), simplify = TRUE,
    .FUN)
}
\description{
  Apply lapply.subset.append to each element of lol, using
  the arguments from the parallel list lol.args and the
  function .FUN.
}
\examples{
X <- env.base$years1_5$outcomes
lol <- env.base$years1_5$runs$means
lol.args <- attr(lol, "args.list")
.FUN <- wtdmeancols.lbl

X <- env.base$years1_5$outcomes
lol <- list(all=namedList("gptotvis", "hadmtot"), females.by.ethnicity=namedList("gptotvis", "hadmtot"))
lol.args <- list(all=NULL, females.by.ethnicity=list(logiset=childsets$females, grpby=children$r1stchildethn, grpby.tag="r1stchildethn"))
attr(lol, "args.list") <- lol.args
.FUN <- wtdmeancols.lbl

lapply.subset.append.lol.args(X, lol, lol.args, .FUN)
}

