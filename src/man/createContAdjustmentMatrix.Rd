\name{createContAdjustmentMatrix}
\alias{createContAdjustmentMatrix}
\title{Continuous adjusments}
\usage{
  createContAdjustmentMatrix(num.cases, numiterations)
}
\arguments{
  \item{num.cases}{the number of cases (or micro-units) in
  the simulation}

  \item{numiterations}{The number of iterations in the
  simulation}
}
\description{
  Before or during the simulation the user may wish to
  specify changes to continuous variables. Eg: a user may
  wish to decrease the number of cigarettes smoked per day
  by 20 for every child with a mother who smokes 40 or more
  cigarettes a day) The user specifies from the user
  interface desired increments (or decrements) for all
  micro units in particular categories.  These adjustments
  are made to the simulated data from the base simulation
  and the results stored in these matrices.  At each year
  in the simulation these cont.adjustment matrices are
  checked and, if they contain values, they are used
  instead of the simulated values at that iteration. If the
  column for a particular variable at an iteration only
  contains NAs then the simulated values just simulated for
  that iteraction will be used
}
\details{
  Create empty continuous variable adjustment matrices for
  specified number of iterations. Initial matrix values are
  NA (i.e: no adjustment).
}
\examples{
\dontrun{
numiteration=NUM_ITERATIONS
num.cases=20
createContAdjustmentMatrix(num.cases, numiterations)
}
}

