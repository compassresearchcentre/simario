\name{createAdjustmentMatrices}
\alias{createAdjustmentMatrices}
\title{Categorical adjusment}
\usage{
  createAdjustmentMatrices(cat.varnames, dict, rows)
}
\arguments{
  \item{cat.varnames}{names of level vars to create
  adjustment matrices for}

  \item{dict}{Dictionary object. Used to lookup codings for
  cat.varnames}

  \item{rows}{row names, or a numeric scalar for the number
  of rows number of iterations to create}
}
\description{
  Before or during the simulation the user may wish to
  specify the proportion of category values desired for a
  simframe variable Eg: a user may wish the proportion of
  home owners in year 2 to be 0.4, 0.6 Desired proportions
  can be specified in a categorical adjustment matrix in
  which rows = iterations In the above example instead of
  simulating the home ownership variable in year 2, it will
  be set to the desired proportions 0.4, 0.6 A desired
  proportion of NA will leave the variable unchanged If
  propensities are supplied they will be used to select
  which micro-units to adjust, otherwise the selection will
  be random Propensities are specified via the global list
  variable propensities Create empty categorical variable
  adjustment matrices for specified number of iterations.
  Initial matrix values are NA (i.e: no adjustment).
}
\examples{
\dontrun{
dict <- dict.MELC
cat.varnames <- c("z1homeownLvl1", "catpregsmk2")
rows = 5

dict <- dict_demo
cat.varnames  <- c("sex","age_grp", "alive")
rows = 100

createAdjustmentMatrices(cat.varnames, dict, rows)
}
}

