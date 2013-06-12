\name{eval.list}
\alias{eval.list}
\title{Evaluate a list/vector of strings as expressions.
Errors if expressions cannot be evaluated.}
\usage{
  eval.list(exprlist, envir = .GlobalEnv,
    enclos = parent.frame(), allowEmptyExpr = FALSE)
}
\arguments{
  \item{exprlist}{list/vector of strings to evaluate}

  \item{envir}{environment to evaluate in, defaults to
  global environment.}

  \item{enclos}{Specifies the enclosure, i.e., where R
  looks for objects not found in envir.  Defaults to the
  caller's environment (parent.frame())}

  \item{allowEmptyExpr}{allow expressions to return no
  value, defaults to FALSE}
}
\description{
  Evaluate a list/vector of strings as expressions. Errors
  if expressions cannot be evaluated.
}
\examples{
\dontrun{
exprlist <- initial_value
envir <- bf
eval.list(exprlist, bf)
}
}

