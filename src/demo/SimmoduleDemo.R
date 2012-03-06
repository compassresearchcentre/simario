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
		catvars <- getOutcomeVars(simframe, "categorical", "demo")
		convars <- getOutcomeVars(simframe, "continuous", "demo")
		
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
		
		.super$new(., "Demo", catvars, convars, convars, freqs.spec, mean.spec)
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
		
		store_current_values_in_outcomes <- function(iteration) {
			outcomes <<- lapply(outcomes, function(x) {
						x[,iteration] <- get(attr(x,"varname"));x 
					}) 
		}
		
		store_current_values_in_previous <- function() {
			previous <- attr(simenv$simframe, "previous")
			invisible(mapply(function(var.prev, var.current) {
								assign(var.prev, get(var.current), inherits=T)
								
							}, previous, names(previous)))
		}

		
		lookup_disability_transition_probs <- function(sex, age_grp, current_disability_state) {
			disability_transition_index <- index_sex_age_grp_disability_state(sex, age_grp, current_disability_state)
			disability_transition_row <- match(disability_transition_index, transition_probabilities$disability_state$index)  
			disability_transition_probs <- transition_probabilities$disability_state$probs[disability_transition_row, ]
			disability_transition_probs
		}

		lookup_death_transition_probs <- function(sex, age) {
			death_transition_probs <- rep(NA, length(sex))
			
			males <- sex == "M"
			females <- sex == "F"
			
			age_males <- age[males]
			age_females <- age[females]
			
			death_transition_probs[males] <- transition_probabilities$death$Male[age_males+1] 
			death_transition_probs[females] <- transition_probabilities$death$Female[age_females+1]
			death_transition_probs
		}
		
		# SIMULATION STARTS HERE
		# simenv <- env.base
						
		attach(simenv$simframe, name="simframe")
		
		NUM_ITERATIONS <- 100
		num_people <- length(sex)
		MAX_AGE <- 99		
		
		outcomes <- createOutcomeMatrices(simenv$simframe, "demo", c(1:NUM_ITERATIONS))
		
		for (iteration in 1:NUM_ITERATIONS) {
			#iteration = 1
			cat("Run", simenv$num_runs_simulated, "year", iteration, "\n")

			store_current_values_in_previous()
			
			alive[age > MAX_AGE] <- F
			
			if (any(alive)) {
			
				disability_transition_probs <- lookup_disability_transition_probs(sex[alive], age_grp[alive], disability_state[alive])
				
				disability_state[alive] <- apply(disability_transition_probs, ROW, function(prob) {
							#prob <- disability_transition_probs[1,]
							sample(1:4, size = 1, replace = T, prob=prob)	
						})
				
				disability_state <- adjustCatVar(disability_state, "disability_state")
				
				earnings[alive] <- earnings[alive] + earnings_scale[disability_state[alive]]
				
				age[alive] <- age[alive] + 1
				
				age_grp[alive] <- bin(age[alive], breaks_age_grp)
				
				death_transition_probs <- lookup_death_transition_probs(sex[alive], age[alive])
	
				now_dead <- runif(length(death_transition_probs)) <= death_transition_probs
				
				alive[alive] <- !now_dead
				
			}
			
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
	as.integer(sex) * 100 + as.integer(age_grp) * 10 + as.integer(disability_state)
}
