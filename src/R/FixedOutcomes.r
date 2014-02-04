#' Checks whether a fixed vector of values should be used instead of the just simulated vector 
#' (parameter x).
#' The fixed vector is that in fixed.outcomes and is based on user specified adjustments to a base
#' run of the simulation.  
#'
#' @param simenv
#'  a Simenv object
#' 
#' @param iteration
#'  the current iteration
#' 
#' @param x
#'  vector of values to replace with vector in fixed.outcomes. Generally x is continuous.
#' 
#' @param varname
#'  name of variable
#' 
#' @return
#'  a vector
#' 
#' @export
#' @examples
#' \dontrun{
#' simenv <- env.scenario
#' iteration <- 3
#' x <- 1:1017
#' varname <- "kids"
#' x.replaced <- selectFixedOutcomeIfSet(simenv, iteration, x, varname)
#' 
#' attr(env.scenario$fixed.outcomes[["kids"]], "is.fixed.iteration")[3] <- TRUE 
#'  x.replaced2 <- selectFixedOutcomeIfSet(simenv, iteration, x, varname)
#' table(x.replaced2)
#' }
selectFixedOutcomeIfSet <- function(simenv, iteration, x, varname) {
	fixed.outcomes <- simenv$fixed.outcomes
	
	is.fixed.iteration <- attr(fixed.outcomes[[varname]], "is.fixed.iteration")
	
	if (!is.fixed.iteration[iteration]) {
		return(x)
	}
	
	run.num <-  simenv$num_runs_simulated+1
	
	cat("Using fixed values for ", varname, ". Iteration:", iteration, " Run:", run.num, "\n", sep="")
	
	fixed.outcomes[[varname]][, iteration, run.num]
	
}