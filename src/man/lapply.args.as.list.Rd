\name{lapply.args.as.list}
\alias{lapply.args.as.list}
\title{Call lapply with a list of arguments to FUN specified as the parameter FUN.args.}
\usage{
  lapply.args.as.list(X, FUN, FUN.args)
}
\arguments{
  \item{X}{a vector (atomic or list) or an expression
  object. Other objects (including classed objects) will be
  coerced by base::as.list.}

  \item{FUN}{the function to be applied to each element of
  xlist}

  \item{FUN.args}{a named list of arguments to be suppplied
  to FUN when executed on each element of xlist}
}
\description{
  Call lapply with a list of arguments to FUN specified as
  the parameter FUN.args.
}
\examples{
mx <- matrix(c(8,2,2,2,8,2,3,2,3,2,2,4,8,2,3,4,2,2,4,3),nrow=4,ncol=5,dimnames=list(NULL, LETTERS[1:5]))
grpby <- c('M','F','F','M')
logiset <- c(FALSE, TRUE, FALSE, TRUE)
result <- table_mx_cols(mx, grpby = grpby, logiset = logiset)

X <- list(mx=mx)
FUN <- table_mx_cols
FUN.args <- list(grpby = grpby, logiset = logiset)
result2 <- lapply.args.as.list(X, FUN, FUN.args)
all.equal(result2[[1]], result)
}

