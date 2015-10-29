# SimmoduleDemo object for the demo.
# 
# Author: Oliver Mannion
###############################################################################


SimmoduleDemo <- proto(. = Simmodule, expr = {
			
	#' Create new object.
	#' 
	#' @param .
	#'  receiving object
	#' @param simframe
	#'  simframe. Stored in the environment
	#' 
	#' @return 
	#'  a new simmoduleDemo object
	#' 
	#' @export
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
	#' @return 
	#'  outcomes
	#' 
	#' @export
	#' @examples
	#' simenv <- env.base
	#' outcomes <- simulateRun(simenv=simenv) 
	simulateRun <- function(., simenv) {
		
		#' Adjust categorical values to desired proportions for current iteration
		#' in cat.adjustments (if any).
		#'
		#' Allows subgroup adjustment if a subgroup expression attribute is attached. 
		#'  
		#' @param x
		#'  vector of categorical values from which a new adjusted vector is returned 
		#' @param varname
		#'  varname, used to lookup in cat.adjustments and propensities
		#' @param propens
		#' 
		#' @return 
		#' an adjusted vector of categorical variable
		#' 
		#' @export
		#' @examples
		#' adjustCatVar(disability_state, "disability_state")
		# adjustCatVar <- function(x, varname, propens=NULL) {
			# cat.adjustments <- simenv$cat.adjustments
			
			#if (!varname %in% names(cat.adjustments)) stop(gettextf("No cat.adjustments for %s", varname))
			
			#desiredProps <- cat.adjustments[[varname]][iteration,]
			
			#if (any(is.na(desiredProps))) {
				#return(x)
			#}
			
			#attach logisetexpr attribute to desiredProps
			#desiredProps <- structure(desiredProps, logisetexpr=attr(cat.adjustments[[varname]], "logisetexpr"))
			
			#logiset <- evaluateLogisetExprAttribute(desiredProps, parent.frame())
			
					
			#cat("Adjusting", varname, ": ", desiredProps, "\n")
			
			#adjust.proportions(x, desiredProps, propens, logiset) 
			
			#modifyProps(x, desiredProps, propensities[[varname]][,,iteration])    ###Doesn't work for subgroup
		#}
		
	
		#' Adjust categorical or continuous to desired proportions for current iteration
		#' in cat.adjustments (if any).
		#'
		#' Allows subgroup adjustment if a subgroup expression attribute is attached. 
		#'  
		#' @param x
		#'  vector of categorical or continuous values from which a new adjusted vector is returned 
		#' @param varname
		#'  varname, used to lookup in cat.adjustments and propensities
		#' @param propens
		#'  propensity scores used to decide who should change categories
		#' 
		#' @return 
		#' an adjusted vector of categorical variable
		#' 
		#' @export
		#' @examples
		#' adjustVar(disability_state, "disability_state")
		adjustVar <- function(x, varname, propens=NULL) {
			cat.adjustments <- simenv$cat.adjustments
			
			if (!varname %in% names(cat.adjustments)) stop(gettextf("No cat.adjustments for %s", varname))
			
			desiredProps <- cat.adjustments[[varname]][iteration,]
			
			if (any(is.na(desiredProps))) {
				return(x)
			}
			
			#attach logiset attribute to desiredProps
			#desiredProps <- structure(desiredProps, logisetexpr=attr(cat.adjustments[[varname]], "logiset"))
			contvars <- getOutcomeVars(simenv$simframe, "continuous")
			if(varname%in%contvars){
				desiredProps <- structure(desiredProps, varname=varname, logisetexpr=attr(cat.adjustments[[varname]], "logiset"), levels=names(binbreaks[[varname]])[-1])
			}else{
				desiredProps <- structure(desiredProps, varname=varname, logisetexpr=attr(cat.adjustments[[varname]], "logiset"), levels=simenv$dict$codings[[varname]])
			}
			
			
			logiset <- evaluateLogisetExprAttribute(desiredProps, parent.frame())
			
			#think this function is only called in year 1 so don't need to have this here
			##if ((is.null(propens))&(!is.null(propensities[[varname]]))) {
			##propens <- propensities[[varname]][,,iteration]
			##}
			
			cat("Adjusting", varname, ": ", desiredProps, "\n")
			
			catToContModels <- attr(cat.adjustments[[varname]], "catToContModel")
			cont.binbreaks <- attr(cat.adjustments[[varname]], "cont.binbreaks")
			
			adj.x <- adjust.proportions(x, desiredProps, propens, logiset, catToContModels, cont.binbreaks, envir=parent.frame())
			return(adj.x)
		}
		
		
		#' Set up outcomes with values from current vars.
		#'  
		#' @param iteration
		#'  the current iteration
		#' 
		#' @return 
		#'  NULL
		#' 
		#' @export
		#' @examples
		#' 	store_current_values_in_outcomes(1)
		store_current_values_in_outcomes <- function(iteration) {
			outcomes <<- lapply(outcomes, function(x) {
						x[,iteration] <- get(attr(x,"varname"));x 
					}) 
		}
		
		
		#' Set up previous vars with values from current vars.
		#'  
		#' @return 
		#'  NULL
		#' 
		#' @export
		#' @examples
		#' 	store_current_values_in_previous()
		store_current_values_in_previous <- function() {
			previous <- attr(simenv$simframe, "previous")
			invisible(mapply(function(var.prev, var.current) {
								assign(var.prev, get(var.current), inherits=T)
								
							}, previous, names(previous)))
		}

		
		# Return the disability transition probabilities from transition_probabilities.
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
		

		# Return the death transition probabilities from transition_probabilities.
		lookup_death_transition_probs <- function(sex, age) {
			death_transition_probs <- rep(NA, length(sex))
			
			males <- sex == 1
			females <- sex == 2
			
			age_males <- age[males]
			age_females <- age[females]
		
			death_transition_probs[males] <- transition_probabilities$death$Male[age_males+1] 
			death_transition_probs[females] <- transition_probabilities$death$Female[age_females+1]
			death_transition_probs
		}
		
		
		# Return the qualification transition probabilities from transition_probabilities.
		lookup_qualification_transition_probs <- function(age, current_qualification) {
			qualification_transition_index <- index_age_qualification(age, current_qualification)
			#qualification_transition_index is vector of 1000 or however many people
			#transition_probabilities$qualification$index is a vector of posisble combinations of age, current qualification
			#match does a look up of the vector of people indices, in the vector of possible indices
			#ie 'qualification_transition_row' gives the appropraite row numbers (for the combo of age, current_qualification) to refer to in the probablity matrix (transition_probabilities$qualification$probs)
			qualification_transition_row <- match(qualification_transition_index, transition_probabilities$qualification$index)  
			qualification_transition_probs <- transition_probabilities$qualification$probs[qualification_transition_row, ]
			qualification_transition_probs
		}
		
		
		# Simulate qualification
		simulate_qualification <- function(){
			
			if(iteration>=17 & iteration<=56){
				
				qualification_transition_probs <- lookup_qualification_transition_probs(age[alive], qualification[alive])
				
				qualification[alive] <<- apply(qualification_transition_probs, ROW, function(prob) {
							sample(1:4, size = 1, replace = T, prob=prob)	
						})
			}
						
			#generate propensities for scenario testing
			numPeople <- length(simframe.master[[1]])
			qualificationmodels <- propensityModels[["qualification"]]
			qualificationPropensities <- predictOrdinal(qualificationmodels, numPeople, envir=env.base$simframe, stochastic=TRUE)
			qualification <<- adjustVar(qualification, "qualification", propens=qualificationPropensities[,-ncol(qualificationPropensities)])
	
		}
		
		
		# Simulate IQ
		simulate_IQ <- function(){
			IQchange <- 0
			 if (iteration >= 7 & iteration <= 15) {
				IQchange <- predSimNorm(models$IQModel7_15)
			} else if (iteration >= 16 & iteration <= 27) {
				IQchange <- predSimNorm(models$IQModel16_27)
			} else if (iteration >= 28) {
				IQchange <- predSimNorm(models$IQModel28onwards)
			}
			IQ <<- IQ_previous + round(IQchange)
			isNegative <- IQ < 0
			IQ[!alive] <<- IQ_previous[!alive]
			IQ[isNegative] <<- IQ_previous[isNegative]
			IQ <<- adjustVar(IQ,"IQ")
			IQ
		}
		
		
		# Simulate earnings
		simulate_earnings <- function(){
			if(iteration <= 15){
				earnings <<- 0
			} else if (iteration > 15){
				earnings <<- outcomes$earnings[,iteration-1]
				earnings[alive] <<- predSimNBinom(models$earningsModel)
			}
			earnings
		}
		
		
		# SIMULATION STARTS HERE
		# simenv <- env.base
						
		attach(simenv$simframe, name="simframe")
		
		NUM_ITERATIONS <<- 100
		num_people <- length(sex)
		MAX_AGE <- 99		
		
		outcomes <- createOutcomeMatrices(simenv$simframe, "demo", c(1:NUM_ITERATIONS))
		
		for (iteration in 1:NUM_ITERATIONS) {
			#iteration = 17
			cat("Run", simenv$num_runs_simulated+1, "year", iteration, "\n")

			store_current_values_in_previous()
			
			alive[age >= MAX_AGE] <- F
			
			if (any(alive)) {
				age[alive] <- age[alive] + 1
			
				disability_transition_probs <- lookup_disability_transition_probs(sex[alive], age_grp[alive], disability_state[alive])
				
				disability_state[alive] <- apply(disability_transition_probs, ROW, function(prob) {
							#prob <- disability_transition_probs[1,]
							sample(1:4, size = 1, replace = T, prob=prob)	
						})
				
				disability_state <- adjustVar(disability_state, "disability_state")
				
				simulate_qualification()
				
				age_grp[alive] <- bin(age[alive], breaks_age_grp)	
				
				simulate_IQ()
				
				simulate_earnings()
				
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
	#' @param .
	#'  receiving object.
	#' 
	#' @param simframe
	#'  the simulation environment's simframe. Useful for accessing adjusted variables.
	#' 
	#' @param outcomes
	#'  the list of outcome matrices generated by simulateRun()
	#' 
	#' @param cat.adjustments
	#'  the simulation environment's cat.adjustments.
	#' 
	#' @param run
	#'  the current run
	#' 
	#' @return 
	#'  a list of run results. Each element E1 of run results is a list and is the set of results for a given 
	#'  operation. In turn, each element of E1 may be a list, matrix or vector and is the result for
	#'  the operation applied to an outcome variable (ie: an element in outcomes). eg:
	#'  result
	#'  result$freqs$disability_state (list)  
	#'  result$means$earnings (matrix)
	#'  result$quantiles$earnings (matrix)
	#' 
	#' @export
	#' @examples
	#'  . <- env.base$modules$demo
	#'  simframe<-env.base$simframe
	#'  outcomes <- .$outcomes
	#'  cat.adjustments <- env.base$cat.adjustments	
	#'  run<-1
	#'  map_outcomes_to_run_results(simframe, outcomes, cat.adjustments, run)
									
	map_outcomes_to_run_results <- function(., simframe, outcomes, cat.adjustments, run) {
																	#have added cat.adjustments and run here 		
		cat(gettextf("Generating run results for %s\n", .$name))	#to get to work with newer simario version of map_outcomes_to_run_results
		catvars <- getOutcomeVars(simframe.master, "categorical", "demo")	#yet to make these parameters do anything in this function
		convars <- getOutcomeVars(simframe.master, "continuous", "demo")		
		
		# add additional "all years" row totals to continuous vars
		# outcomes_wtotals <- lapply(outcomes[convars], function(x) {
						#x <- outcomes[[convars[1]]] 
		#			structure(cbind(x, "All Years"=rowSums(x, na.rm=TRUE)), varname=attr(x,"varname"))
		#		})
	
	
	#bin/group continuous variables that are displayed as categorical for scenario testing purposes
	#(IQ)	
	
	binned.list <- binned.list.base <- list()
	for (i in 1:length(convars)) {
		tab <- outcomes[c(convars[i])]
		binned.tab <- matrix(bin(tab[[1]], binbreaks[[convars[i]]]), ncol=100)
		attr(binned.tab, "meta") <- c(varname=convars[[i]])
		binned.list[[i]] <- binned.tab
		names(binned.list)[i] <- convars[i]
		if (!is.null(attr(cat.adjustments[[1]], "logisetexpr"))) {
			#the env.base$...run1$outcomes object only exits if we are in the scenario environments
			#we only need the base.outcomes.current.run if a subgroup scenario was run 
			#(otherwise, if no subgroup scenario run, they just look at the tables from env.base)
			base.outcomes.expr <- paste("env.base$modules$demo$run_results$run", run, "$outcomes", sep="")
			base.outcomes.current.run <- eval(parse(text=base.outcomes.expr))
			
			if (is.null(base.outcomes.current.run)) {
				stop(gettextf("%s does not exist. Cannot create binned.list.base", base.outcomes.expr))
			}
			
			tab.base <- base.outcomes.current.run[c(convars[i])]
			binned.tab.base <- matrix(bin(tab.base[[1]], binbreaks[[convars[i]]]), ncol=100)
			attr(binned.tab.base, "meta") <- c(varname=convars[[i]])
			binned.list.base[[i]] <- binned.tab.base
			names(binned.list.base)[i] <- convars[i]
		}
	}
		#browser()
		#user specified subgroup variable
		#prepend "outcomes$", "simframe$", or "env.base$...outcomes$" to variables so they
		#can be found
		if (!is.null(attr(cat.adjustments[[1]], "logisetexpr"))) {
			subgroup.expr <- attr(cat.adjustments[[1]], "logisetexpr")
			prepended.exprs <- prepend.paths(subgroup.expr) 
			sg.expr <- unlist(prepended.exprs["sg.expr"])
			names(sg.expr) <- ""
			sg.expr.base <- unlist(prepended.exprs["sg.expr.base"])
			names(sg.expr.base) <- ""
			eval(parse(text=sg.expr))
			eval(parse(text=sg.expr.base))
		}
		
		run_results <- list()
		
		if (!is.null(attr(cat.adjustments[[1]], "logisetexpr"))) {
			run_results$freqs_by_subgroup <- lapply(outcomes[catvars], table_mx_cols_MELC, grpby=sg.var, grpby.tag=sg.expr, dict=dict_demo)
			run_results$freqs_by_subgroup_base_data <- lapply(base.outcomes.current.run[catvars], table_mx_cols_MELC, grpby=sg.var.base, grpby.tag=sg.expr.base, dict=dict_demo)
			run_results$means_by_subgroup <- lapply(outcomes[convars], mean_mx_cols_BCASO, grpby=sg.var, grpby.tag=sg.expr, dict=dict_demo)
			run_results$means_by_subgroup_base_data <- lapply(base.outcomes.current.run[convars], mean_mx_cols_BCASO, grpby=sg.var.base, grpby.tag=sg.expr.base, dict=dict_demo)
			run_results$freqs_continuousGrouped_by_subgroup <- lapply(binned.list, table_mx_cols_MELC, grpby=sg.var, grpby.tag=sg.expr, dict=dict_demo)
			run_results$freqs_continuousGrouped_by_subgroup_base_data <- lapply(binned.list.base, table_mx_cols_MELC, grpby=sg.var.base, grpby.tag=sg.expr.base, dict=dict_demo)
		}
		
		#run_results$confreqs <- lapply(outcomes[convars], table_mx_cols_MELC, dict=dict_demo)
	    run_results$freqs <- lapply(outcomes[catvars], table_mx_cols_MELC, dict=dict_demo)
		##run_results$freqs_males <- lapply(outcomes[catvars], table_mx_cols_MELC, logiset=people_sets$males, dict=dict_demo)
		##run_results$freqs_females <- lapply(outcomes[catvars], table_mx_cols_MELC, logiset=people_sets$females, dict=dict_demo)
		##run_results$freqs_by_sex <- lapply(outcomes[catvars], table_mx_cols_MELC, grpby=people$sex, grpby.tag="sex", dict=dict_demo)
		run_results$freqs_continuousGrouped <- lapply(binned.list, table_mx_cols_MELC, dict=dict_demo)

		run_results$means <- lapply(outcomes[convars], mean_mx_cols_BCASO, dict=dict_demo)
		##run_results$means_males <- lapply(outcomes[convars], mean_mx_cols_BCASO, logiset=people_sets$males, dict=dict_demo)
		##run_results$means_females <- lapply(outcomes[convars], mean_mx_cols_BCASO, logiset=people_sets$females, dict=dict_demo)
		##run_results$means_by_sex <- lapply(outcomes[convars], mean_mx_cols_BCASO, grpby=people$sex, grpby.tag="sex", dict=dict_demo)
		run_results$summaries <- lapply(outcomes[convars], summary_mx_cols)
		run_results$quantiles <- lapply(outcomes[convars], quantile_mx_cols, new.names=c("Min", "20th", "40th", "60th","80th","Max"), probs=seq(0,1,0.2), na.rm = TRUE)
		
		run_results$outcomes <- outcomes 
		
		run_results 
	}


	#' Collates (ie: reduces) all run results to averaged values and labels collated results.
	#'
	#' @param .
	#'  receiving object.
	#' 
	#' @param all_run_results
	#'  the list of all run results generated by map_outcomes_to_run_results() 
	#' 
	#' @param cat.adjustments
	#'  the simulation environment's cat.adjustments.
	#' 
	#' @param simframe
	#'  the simulation environment's simframe.
	#' 
	#' @export
	#' @examples 
	#' . <- env.base$modules$demo
	#' all_run_results <- .$run_results
	#' cat.adjustments <- env.base$cat.adjustments	
	#' simframe<-env.base$simframe
	#' collate_all_run_results(all_run_results, cat.adjustments, simframe)
	collate_all_run_results <- function(., all_run_results, cat.adjustments, simframe) {
		cat(gettextf("Collating all run results for %s\n", .$name))
		
		outcomes <- .$outcomes
		
		all_run_results_zipped <- lzip(all_run_results)
		all_run_results_zipped <- lapply(all_run_results_zipped, lzip)
	
		collated_results <- list()
		
		if (!is.null(all_run_results_zipped$freqs_by_subgroup)) {
			collated_results$freqs_by_subgroup <- lapply(all_run_results_zipped$freqs_by_subgroup, collator_freqs_remove_zero_cat2, dict=dict_demo, CI=TRUE)
			collated_results$means_by_subgroup <- lapply(all_run_results_zipped$means_by_subgroup, collator_means, dict=dict_demo, NA.as.zero=F, CI=TRUE)
			collated_results$freqs_continuousGrouped_by_subgroup <- lapply(all_run_results_zipped$freqs_continuousGrouped_by_subgroup, collator_freqs2, dict=dict_demo, CI=FALSE, cat.adjustments=cat.adjustments)
			collated_results$freqs_by_subgroup_base_data <- lapply(all_run_results_zipped$freqs_by_subgroup_base_data, collator_freqs_remove_zero_cat2, dict=dict_demo, CI=TRUE)
			collated_results$means_by_subgroup_base_data <- lapply(all_run_results_zipped$means_by_subgroup_base_data, collator_means, dict=dict_demo, NA.as.zero=F, CI=TRUE)
			collated_results$freqs_continuousGrouped_by_subgroup_base_data <- lapply(all_run_results_zipped$freqs_continuousGrouped_by_subgroup_base_data, collator_freqs2, dict=dict_demo, CI=FALSE, cat.adjustments=cat.adjustments) #cat.adjustments only used to get binbreaks
		}
		
		#collated_results$confreqs <- lapply(all_run_results_zipped$confreqs, collator_freqs, dict = dict_demo)
		#collated_results$histogram <- lapply(all_run_results_zipped$confreqs, collator_histogram, dict = dict_demo)
		collated_results$freqs <- lapply(all_run_results_zipped$freqs, collator_freqs_remove_zero_cat, dict = dict_demo)
		collated_results$freqs_continuousGrouped <- lapply(all_run_results_zipped$freqs_continuousGrouped, collator_freqs2, dict=dict_demo, CI=TRUE, cat.adjustments=cat.adjustments)
		##collated_results$freqs_males <- lapply(all_run_results_zipped$freqs_males, collator_freqs_remove_zero_cat, dict = dict_demo)
		##collated_results$freqs_females <- lapply(all_run_results_zipped$freqs_females, collator_freqs_remove_zero_cat, dict = dict_demo)
		##collated_results$freqs_by_sex <- lapply(all_run_results_zipped$freqs_by_sex, collator_freqs_remove_zero_cat, dict = dict_demo)
		collated_results$means <- lapply(all_run_results_zipped$means, collator_means, dict = dict_demo)
		##collated_results$means_males <- lapply(all_run_results_zipped$means_males, collator_means, dict = dict_demo)
		##collated_results$means_females <- lapply(all_run_results_zipped$means_females, collator_means, dict = dict_demo)
		##collated_results$means_by_sex <- lapply(all_run_results_zipped$means_by_sex, collator_means, dict = dict_demo)
		collated_results$summaries <- lapply(all_run_results_zipped$summaries, collator_list_mx)
		collated_results$quantiles <- lapply(all_run_results_zipped$quantiles, collator_list_mx)
		
		collated_results
	}
	
})

#' 
#' 
#' 
#' 
#' @param expr
#' 
#' @return 
#' 
#' 
#' @export 
#' @example 
#' 
prepend.paths <- function(expr) {
	#expr <- "r1stchildethnLvl3==1 & mhrswrk<21"
	
	catvars <- getOutcomeVars(env.base$simframe, "categorical")
	contvars <- getOutcomeVars(env.base$simframe, "continuous")
	time.variant.vars <- c(catvars, contvars)
	#catvars and contvars are time-variant
	presimvars <- names(env.base$presim.stats)
	#presimvars are time-invariant
	
	#browser()
	
	for (i in 1:length(time.variant.vars)) {
		pos1 <- str_locate(expr, time.variant.vars[i])
		if (sum(is.na(pos1))==0) {
			replace.expr1 <- paste("outcomes$", time.variant.vars[i], sep="")
			expr <- stri_replace_all_fixed(expr, time.variant.vars[i], replace.expr1)	
		}
	}
	for (i in 1:length(presimvars)) {
		pos2 <- str_locate(expr, presimvars[i])
		if (sum(is.na(pos2))==0) {
			replace.expr2 <- paste("simframe$", presimvars[i], sep="")
			##expr <- str_replace(expr, presimvars[i], replace.expr2)
			expr <- stri_replace_all_fixed(expr, presimvars[i], replace.expr2)
		}
	}
	
	
	sg.expr <- paste("sg.var <- ", expr, sep="")
	sg.expr.base <- stri_replace_all_fixed(sg.expr, "outcomes", "base.outcomes.current.run")
	sg.expr.base <- str_replace(sg.expr.base, "sg.var", "sg.var.base")
	sg.expr.base <- stri_replace_all_fixed(sg.expr.base, "simframe", "env.base$simframe")
	result <- list(sg.expr=sg.expr, sg.expr.base=sg.expr.base)
	return(result)
}

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
#' 
#' @return 
#'  a vector of unique integer index
#' 
#' @export 
#' @example 
#' sex <- c(1,1,2,2)
#' age_grp <- c(1,2,3,1)
#' disability_state <- c(1,2,3,4)
#' index_sex_age_grp_disability_state(sex, age_grp, disability_state)
index_sex_age_grp_disability_state <- function(sex, age_grp, disability_state) {
	as.integer(sex) * 100 + as.integer(age_grp) * 10 + as.integer(disability_state)
}


#' Create a unique integer index vector given the supplied values.
#' This index can then be used to lookup a row in the qualification
#' transition probability dataframe.
#' 
#' @param age
#'  an integer vector with the values 17 to 56.
#' @param qualification
#'  an integer vector with the values 1,2,3,4
#' 
#' @return 
#'  a vector of unique integer index
#' 
#' @export 
#' @example 
#' age <- 17:56
#' qualification<-rep(c(1:4),10)
#' index_age_qualification(age, qualification)
index_age_qualification <- function(age, qualification) {
	as.integer(age) * 10 + as.integer(qualification)
}