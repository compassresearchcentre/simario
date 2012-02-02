# TODO: Add comment
# 
# Author: oman002
###############################################################################


SimmoduleDemo <- proto(. = Simmodule, expr = {
			
	#' Create new object.
	#' 
	#' @param simframe
	#' 
	#' @examples 
	#' simframe <- simframe.master
	#' SimmoduleDemo$new(simframe) 
	new <- function(., simframe) {
		catvars <- getOutcomeVars(simframe, "categorical", "main")
		convars <- getOutcomeVars(simframe, "continuous", "main")
		
		mean.spec <- list(
				all = list(),
				males = list(logiset=people_sets$males),
				females = list(logiset=people_sets$females),
				all.by.gender = list(grpby=people$sex, grpby.tag="sex")
		)
		
		freqs.spec <- list(
				all = list(),
				males = list(logiset=people_sets$males),
				females = list(logiset=people_sets$females),
				all.by.gender = list(grpby=people$sex, grpby.tag="sex")
		)
		
		.super$new(., "Main", catvars, convars, convars, freqs.spec, mean.spec)
	}
	
	#' Simulate outcomes.
	#'
	#' @param .
	#'  receiving object. not used.
	#' @param simenv
	#'  a Simenv object containing a simframe and cat.adjustments
	#' 
	#' @return outcomes
	#' 
	#' @examples
	#' 
	#' simenv <- env.base
	#' 
	#' outcomes <- simulateRun(simenv) 
	simulateRun <- function(., simenv) {
		
		#' Adjust categorical values to desired proportions in cat.adjustments (if any).
		#' 
		#' @param x
		#'  categorical values to adjust
		#' @param varname
		#'  varname, used a lookup into cat.adjustments and propensities
		adjustCatVar <- function(x, varname) {
			cat.adjustments <- simenv$cat.adjustments
			
			if (!varname %in% names(cat.adjustments)) stop(gettextf("No cat.adjustments for %s", varname))
			
			desiredProps <- cat.adjustments[[varname]][iteration,]
			
			if (any(is.na(desiredProps))) {
				return(x)
			}
			
			cat("Adjusting", varname, ": ", desiredProps, "\n")
			
			modifyProps(x, desiredProps, propensities[[varname]][,,iteration])
		}
		
		store_current_values_in_outcomes <- function(xcol) {
			outcomes <<- lapply(outcomes, function(x) {
						x[,xcol] <- get(attr(x,"varname"));x 
					}) 
		}
		
		store_current_values_in_previous <- function() {
			invisible(mapply(function(var.prev, var.current) {
								assign(var.prev, get(var.current), inherits=T)
								
							}, previous, names(previous)))
		}
		
		# SIMULATION STARTS HERE
		# simenv <- env.base
						
		attach(simenv$simframe, name="simframe")
		
		NUM_ITERATIONS <- 100
		num_people <- length(sex)
		MAX_AGE <- 99		
		
		outcomes <- createOutcomeMatrices(simenv$simframe, "main", c(1:NUM_ITERATIONS))
		previous <- attr(simenv$simframe, "previous")
		
		for (iteration in 1:NUM_ITERATIONS) {
			#iteration = 1
			cat("Run", simenv$num_runs_simulated, "year", iteration, "\n")

			store_current_values_in_previous()
			
			alive[age > MAX_AGE] <- F
			
			disability_transition_index <- index_sex_age_grp_disability_state(sex, age_grp, disability_state)
				
#			for (i in 1:num_people) {
		
#				as.integer (probs$disability_transition$Sex)
				
#				str(probs$disability_transition)
				
				
#			}
						 
			#sample(1:4, size = num_two_parent_family, replace = T, prob=c(0.0045, 0.0098-0.0045, 0.0272-0.0098, 1-0.0272))
			
			store_current_values_in_outcomes(iteration)
		}
		
		detach("simframe")
		outcomes
		
	}
	
})


#' Create a unique integer index vector given the supplied values.
#' This index can then be used to lookup a row in the disability
#' transition probability dataframe.
#' 
#' @param sex
#'  a factor vector with 2 levels: "F" = 1, "M" = 2 
#' @param age_grp
#'  an integer vector with the values 1,2,3
#' @param disability_state
#'  an integer vector with the values 1,2,3,4
index_sex_age_grp_disability_state <- function(sex, age_grp, disability_state) {
	as.integer(sex) * 100 + age_grp * 10 + disability_state
}
