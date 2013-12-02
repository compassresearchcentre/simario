# SimmoduleDemo object for the demo.
# 
# Author: Oliver Mannion
###############################################################################


SimmoduleDemo <- proto(. = Simmodule, expr = {
			
	#' Create new object.
	#' 
	#' @param simframe
	#' 
	#' @examples 
	#' . <- SimmoduleDemo
	#' simframe <- simframe.master
	#' SimmoduleDemo$new(simframe) 
	new <- function(., simframe) {
		#note: the . argument is SimmoduleDemo
		.super$new(., name="Demo")
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
	#' outcomes <- simulateRun(simenv=simenv) 
	simulateRun <- function(., simenv) {
		
		#' Adjust categorical values to desired proportions for current iteration
		#' in cat.adjustments (if any).
		#' 
		#' @param x
		#'  vector of categorical values from which a new adjusted vector is returned 
		#' @param varname
		#'  varname, used to lookup in cat.adjustments and propensities
		adjustCatVar <- function(x, varname, propens=NULL) {
			cat.adjustments <- simenv$cat.adjustments
			
			#varname.no.lvl <- strip_lvl_suffix(varname[1])
			
			if (!varname %in% names(cat.adjustments)) stop(gettextf("No cat.adjustments for %s", varname))
			
			desiredProps <- cat.adjustments[[varname]][iteration,]
			
			if (any(is.na(desiredProps))) {
				return(x)
			}
			
			#attach logisetexpr attribute to desiredProps
			desiredProps <- structure(desiredProps, logisetexpr=attr(cat.adjustments[[varname]], "logisetexpr"))
			
			logiset <- evaluateLogisetExprAttribute(desiredProps, parent.frame())
			
					
			cat("Adjusting", varname, ": ", desiredProps, "\n")
			
			adjust.proportions(x, desiredProps, propens, logiset) 
			
			#modifyProps(x, desiredProps, propensities[[varname]][,,iteration])    ###Doesn't work for subgroup
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
			#disability_transition_index is vector of 1000 or however many people
			#transition_probabilities$disability_state$index is a vector of posisble combinations of age,gender, disability status
			#match does a look up of the vector of people indices, in the vector of possible indices
			# ie 'disability_transition_row' gives the appropraite row numbers (for the combo of sex, age_grp, current_disability_state) to refer to in the probablity matrix (transition_probabilities$disability_state$probs)
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
			cat("Run", simenv$num_runs_simulated+1, "year", iteration, "\n")

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
	
	#' Map outcomes to run results for a single run. Run results are typically 
	#' descriptive statistics (eg: freqs, means, etc) generated for variables 
	#' in outcomes. 
	#' 
	#' @param simframe
	#'  the simulation environment's simframe. Useful for accessing adjusted variables.
	#' 
	#' @param outcomes
	#'  the list of outcome matrices generated by simulateRun()
	#' 
	#' @return 
	#'  a list of run results. Each element E1 of run results is a list and is the set of results for a given 
	#'  operation. In turn, each element of E1 may be a list, matrix or vector and is the result for
	#'  the operation applied to an outcome variable (ie: an element in outcomes). eg:
	#'  result
	#'  result$freqs$disability_state (list)  
	#'  result$freqs$num_children (list)
	#'  result$means$earnings (matrix)
	#'  result$quantiles$earnings (matrix)
	#' 
	#' @examples
	#'  . <- env.base$modules$demo
	#'  outcomes <- .$outcomes									
	map_outcomes_to_run_results <- function(., simframe, outcomes, cat.adjustments, run) {
																	#have added cat.adjustments and run here 		
		cat(gettextf("Generating run results for %s\n", .$name))	#to get to work with newer simario version of map_outcomes_to_run_results
		catvars <- getOutcomeVars(simframe.master, "categorical", "demo")	#yet to make these parameters do anything in this function
		convars <- getOutcomeVars(simframe.master, "continuous", "demo")		
		
		# add additional "all years" row totals to continuous vars
		outcomes_wtotals <- lapply(outcomes[convars], function(x) {
						#x <- outcomes[[convars[1]]] 
					structure(cbind(x, "All Years"=rowSums(x, na.rm=TRUE)), varname=attr(x,"varname"))
				})
		
		run_results <- list()
		
		run_results$confreqs <- lapply(outcomes_wtotals[convars], table_mx_cols)
		run_results$freqs <- lapply(outcomes[catvars], table_mx_cols)
		run_results$freqs_males <- lapply(outcomes[catvars], table_mx_cols, logiset=people_sets$males)
		run_results$freqs_females <- lapply(outcomes[catvars], table_mx_cols, logiset=people_sets$females)
		run_results$freqs_by_sex <- lapply(outcomes[catvars], table_mx_cols, grpby=people$sex, grpby.tag="sex")
		run_results$means <- lapply(outcomes_wtotals[convars], mean_mx_cols)
		run_results$means_males <- lapply(outcomes_wtotals[convars], mean_mx_cols, logiset=people_sets$males)
		run_results$means_females <- lapply(outcomes_wtotals[convars], mean_mx_cols, logiset=people_sets$females)
		run_results$means_by_sex <- lapply(outcomes_wtotals[convars], mean_mx_cols, grpby=people$sex, grpby.tag="sex")
		run_results$summaries <- lapply(outcomes_wtotals[convars], summary_mx_cols)
		run_results$quantiles <- lapply(outcomes_wtotals[convars], quantile_mx_cols, new.names=c("Min", "20th", "40th", "60th","80th","Max"), probs=seq(0,1,0.2), na.rm = TRUE)
		
		run_results 
	}
	
	#' Collates (ie: reduces) all run results to averaged values and labels collated results.
	#'
	#' @param all_run_results
	#'  the list of all run results generated by map_outcomes_to_run_results() 
	#' 
	#' @examples 
	#' . <- env.base$modules$demo
	#' all_run_results <- .$run_results
	collate_all_run_results <- function(., all_run_results, cat.adjustments, simframe) {
		cat(gettextf("Collating all run results for %s\n", .$name))
		
		all_run_results_zipped <- lzip(all_run_results)
		all_run_results_zipped <- lapply(all_run_results_zipped, lzip)
	
		collated_results <- list()
		
		collated_results$confreqs <- lapply(all_run_results_zipped$confreqs, collator_freqs, dict = dict_demo)
		#collated_results$histogram <- lapply(all_run_results_zipped$confreqs, collator_histogram, dict = dict_demo)
		collated_results$freqs <- lapply(all_run_results_zipped$freqs, collator_freqs_remove_zero_cat, dict = dict_demo)
		collated_results$freqs_males <- lapply(all_run_results_zipped$freqs_males, collator_freqs_remove_zero_cat, dict = dict_demo)
		collated_results$freqs_females <- lapply(all_run_results_zipped$freqs_females, collator_freqs_remove_zero_cat, dict = dict_demo)
		collated_results$freqs_by_sex <- lapply(all_run_results_zipped$freqs_by_sex, collator_freqs_remove_zero_cat, dict = dict_demo)
		collated_results$means <- lapply(all_run_results_zipped$means, collator_means, dict = dict_demo)
		collated_results$means_males <- lapply(all_run_results_zipped$means_males, collator_means, dict = dict_demo)
		collated_results$means_females <- lapply(all_run_results_zipped$means_females, collator_means, dict = dict_demo)
		collated_results$means_by_sex <- lapply(all_run_results_zipped$means_by_sex, collator_means, dict = dict_demo)
		collated_results$summaries <- lapply(all_run_results_zipped$summaries, collator_list_mx)
		collated_results$quantiles <- lapply(all_run_results_zipped$quantiles, collator_list_mx)
	
		collated_results
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
