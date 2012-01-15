\name{predict}
\alias{predict}
\title{Predict. Looks in envir for variables specified by model, then multiples the coefficients
by each variable and summs the results.}
\usage{
  predict(model, envir = parent.frame(), set = NULL)
}
\arguments{
  \item{model}{model with terms and coefficiens}

  \item{envir}{an environment in which model variables are
  evaluated. May also be NULL, a list, a data frame, a
  pairlist or an integer as specified to sys.call.

  If the specified envir is a list or data.frame, then the
  parent frame is used as the enclosing environment in
  which variables that don't exist in envir will be
  evaluated.

  If not specified then the parent frame is used, ie: the
  environment of the function calling this (i.e: the
  dynamic parent rather than the lexical parent in which
  this function is defined).

  The usual order of evaluation is used, i.e: in envir,
  then its parents and along the search path. For example,
  if a model variable is reassigned in the function calling
  this, then that reassigned value will be used before a
  global or attached value.}

  \item{set}{subset logical expression indicating elements
  or rows to keep, or NULL to use all elements returned by
  evaluated model variables}
}
\description{
  Predict. Looks in envir for variables specified by model,
  then multiples the coefficients by each variable and
  summs the results.
}
\examples{
model <- model.glm
 model <- models$gptotvis ; set <- c(T, rep(F, 1074))
}

