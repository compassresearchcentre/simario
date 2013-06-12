\name{adjust.proportions}
\alias{adjust.proportions}
\title{A wrapper for modifyProps.
Subsets by the logiset call the appropriate version of modifyProps
(modifyPropsContinuous or modifyProps) then, if modifyProps was called on a logiset,
combine and reorder the data using combine.and.reorder().
Called in adjustCatVar(), adjustContVar, and applyContAdjustmentToSimframe().}
\usage{
  adjust.proportions(x, desiredProps, propens = NULL,
    logiset = NULL, catToContModels = NULL,
    cont.binbreaks = NULL, envir = parent.frame())
}
\arguments{
  \item{x}{A categorical vector to be adjusted.}

  \item{desiredProps}{Vector of desired proportions}

  \item{propens}{propensity scores used to decide who
  should change categories}

  \item{logiset}{A TRUE/FALSE vector indicating the subset
  of units to apply the scenari (change in proportions) to}

  \item{catToContModels}{A list of models which will to
  used to convert the adjusted categorical variable back to
  continuous.}

  \item{cont.binbreaks}{Binbreaks for the variable being
  adjusted if exist.  Used to ensure the imputed continuous
  values (only for continuous variables) are within the
  bounds of the category.}

  \item{envir}{environment - for the MELC MSM is usually
  the simframe of the scenario environment.}
}
\value{
  an adjusted vector, either categorical or continuous
  depending on whether catToCont models were provided.
}
\description{
  A wrapper for modifyProps. Subsets by the logiset call
  the appropriate version of modifyProps
  (modifyPropsContinuous or modifyProps) then, if
  modifyProps was called on a logiset, combine and reorder
  the data using combine.and.reorder(). Called in
  adjustCatVar(), adjustContVar, and
  applyContAdjustmentToSimframe().
}

