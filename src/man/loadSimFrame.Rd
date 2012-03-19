\name{loadSimFrame}
\alias{loadSimFrame}
\title{Creates a simulation frame. A simulation frame is the set of all variables
(input, intermediate, and outcome) used or produced by models during
a simulation. During simulation the simulation frame will contain the
current iteration's set of variables being used in the simulation.
This function establishes the values of the simulation frame at the start
prior to simulation by evaluating the initial value expression specified by
the simframe definition.}
\usage{
  loadSimFrame(simframe_defn, envir = .GlobalEnv)
}
\arguments{
  \item{simframe_defn}{simframe definition. A dataframe of
  variables that define the simframe: $Varname - the name
  of a variable in the simframe $Previous_var - the name of
  a variable in which to store the current value in at the
  beginning of each iteration (i.e: before it's
  transformed).  Optional - for models that require
  previous state.  $Initial_value - an expression that
  generates the initial value of the variable.  The
  expression is evaluated in the context of envir.  If
  empty then an initial numeric value of NA is used.
  $Outcome_type - if specified, indicates this is an
  outcome variable and indicates its type which is one of
  "categorical" or "continuous" $Outcome_module - if
  specified, indicates the Simmmodule this outcome variable
  belongs to}

  \item{envir}{environment to evaluate initial_value in.
  Typically this will be a dataframe loaded from a base
  file. Initial_value can not only reference values in
  envir, but also values in the calling frame.  Defaults to
  global environment.}
}
\value{
  the simulation frame. The simulation frame contains the
  variables in simframe_defn with their inital_value
  evaluated. Each variable contains a set of observations.
  Each observation represents a case in the basefile.

  If an observation is NA, then that case (ie: it's
  observations for all variables) is removed. This however
  does not apply to initial_value expressions that return a
  singular NA. These remain as a vector containing NA for
  as each observation.

  The simulation frame object has the following attributes:

  "previous" - variables that represent values in the
  previous iteration "outcome_vars" - a dataframe of
  Varname, Outcome_type, and Outcome_module "na.omit" - an
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
  simulation frame at the start prior to simulation by
  evaluating the initial value expression specified by the
  simframe definition.
}
\examples{
\dontrun{
envir <- children
envir <- people
basefiledir <- "D:/workspace.sim/MELC/CHDS/base/"
basefiledir <- "D:/workspace.sim/simario/src/demo/resource"
simframe_defn <- readXLSSheet1(basefiledir, "simframedef.xlsx")
simframe.master <- loadSimFrame(simframe_defn, envir)
}
}

