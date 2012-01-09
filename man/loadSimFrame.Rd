\name{loadSimFrame}
\alias{loadSimFrame}
\title{Creates a simulation frame. A simulation frame is the set of all variables
(input, intermediate, and outcome) used or produced by models during
a simulation. During simulation the simulation frame will contain the
current iteration's set of variables being used in the simulation.
This function establishes the values of the simulation frame at the start
prior to simulation using the initial values specified by the simframe
definition.}
\usage{
  loadSimFrame(bf, sfdef)
}
\arguments{
  \item{bf}{base file}

  \item{sfdef}{simframe definition. A dataframe of
  variables that make up the simframe, defined as follows:
  $simvarname - the name of the variable in the simframe
  $previous_var - (if any) the name in which to store the
  variable value in at the end the iteration, for use in
  the next iteration $initial_value - an expression that
  generates the initial value of the variable.  The
  expression is evaluated in the context of the base file,
  and if not found then in the context of the calling
  frame.  If empty then an initial numeric value of NA is
  used.  $outcome_type - if this is an outcome variable,
  it's tpype, one of "categorical" or "continuous"
  $outcome_set - if an outcome variable, the set it belongs
  to}
}
\value{
  the simulation frame. The simulation frame contains the
  variables in sfdef with their inital_value evaluated.
  Each variable contains a set of observations. Each
  observation represents a case in the basefile.

  If an observation is NA, then that case (ie: it's
  observations for all variables) is removed. This however
  does not apply to initial_value expressions that return a
  singular NA. These remain as a vector containing NA for
  as each observation.

  The frame has same rownames as basefile. This is useful
  during investigation or debugging.

  The simulation frame object has the following attributes:

  "previous" - variables that represent values in the
  previous iteration "df.outcome.vars" - a dataframe of
  simvarname, outcome_type, and outcome_set "na.omit" - an
  omit variable that indicates the observations that were
  removed because their were NA. Initial_values that
  returned a singular NA remain.
}
\description{
  Creates a simulation frame. A simulation frame is the set
  of all variables (input, intermediate, and outcome) used
  or produced by models during a simulation. During
  simulation the simulation frame will contain the current
  iteration's set of variables being used in the
  simulation. This function establishes the values of the
  simulation frame at the start prior to simulation using
  the initial values specified by the simframe definition.
}
\examples{
bf <- children
basefiledir <- "D:/workspace.sim/MELC/CHDS/base/"
sfdef <- readXLSSheet1(basefiledir, "simframedef.xlsx")
simframe.start <- loadSimFrame(bf, sfdef)
simframe <- loadSimFrame(bf, sfdef)
}

