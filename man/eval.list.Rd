\name{eval.list}
\alias{eval.list}
\title{Evaluate a list/vector of strings as expressions.
Errors if expressions cannot be evaluated.}
\usage{
  eval.list(exprlist, envir = .GlobalEnv,
    allowEmptyExpr = FALSE)
}
\arguments{
  \item{exprlist}{list/vector of strings to evaluate}

  \item{envir}{environment to evaluate in, defaults to
  global environment.}

  \item{allowEmptyExpr}{allow expressions to return no
  value, defaults to FALSE}
}
\description{
  Evaluate a list/vector of strings as expressions. Errors
  if expressions cannot be evaluated.
}
\examples{
exprlist <- initial_value
envir <- bf
eval.list(exprlist, bf)
}

