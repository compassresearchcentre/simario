# Simulation module object. Contains a user-specified function that generates outcomes.
# Run results are generated from outcomes at the end of the run, and final simulation
# results are generated from all runs at the end of the simulation.
# 
# Author: oman002
###############################################################################

library(proto)

Simmodule <- proto(
. = .GlobalEnv,  # parent environment is .GlobalEnv, rather than the package namespace
expr = {
	name <- NULL 
	outcomes <- list()
			
	#' Run results:
	#' frequencies, continuous frequencies, means, summaries, quantiles
	runs <- list(freqs = list(), 
				cfreqs = list(), 
				means = list(), 
				summaries = list(),
				quantiles = list()
			)
					
	#' Final end-of-simulation results
	result <- list()
			
	#' Creates a new object. 
	#' 
	#' @param .
	#'  Simmodule receiving object.
	#' @param name
	#'  name of this object
	#' @param freqvars
	#' 			frequency variable names, or NULL
	#' @param cfreqvars
	#' 			continuous frequency variable names, or NULL
	#' @param meanvars
	#' 			mean variable names, or NULL
	#' @param freqs.args
	#' 			frequency variable args, or NULL
	#' @param means.args
	#' 			mean variable args, or NULL
	#' @return
	#'  	list(freq = freq, cfreq = cfreq, mean.sets = mean.sets, mean.grouped = mean.grouped)
	#' 		each element of the list is a list of empty catvar or convar elements, either grouped
	#' 		into sets (mean.grouped and mean.sets) or listed straight.
	#' 
	#' @examples 
	#'
	#' freqvars <- c("msmoke", "fsmoke", "single", "kids", "householdsize", "welfare", "mhrswrk", "fhrswrk", "accom", "homeown", "bedrooms",	"chpar", "chres")
	#' meanvars <- c("gptotvis", "hadmtot", "houtptot", "gpresp", "gpmorb", "gpprev")
	#'  
	#' freqvars <- getOutcomeVars(simframe.start, select_outcome_type="categorical", select_outcome_set="years1_5")
	#' meanvars <- getOutcomeVars(simframe.start, select_outcome_type="continuous", select_outcome_set="years1_5")
	#' 
	#' freqs.args <- list( by.ethnicity = list(grpbycoding=codings$r1stchildethn) )
	#' means.args <- list(	all = list(), males = list(logiset=childsets$males),	females = list(logiset=childsets$females),pacific = list(logiset=childsets$pacific),	maori = list(logiset=childsets$maori))
	#' 
	#' cfreqvars <- meanvars
	#' outputs <- Simmodule$new(freqvars, meanvars, meanvars, freqs.args, means.args)
	new <- function(., name, freqvars, cfreqvars, meanvars, freqs.args, means.args) {
		# Frequency tables for categorical variables
		freqslist <- namedList(freqvars)
		
		# Frequency tables for continuous variables
		cfreqs <- namedList(cfreqvars)
		
		# Mean tables for continuous variables
		meanslist <- namedList(meanvars)
		
		freqs <- lapply(freqs.args, function(x) freqslist)
		attr(freqs, "args.list") <- freqs.args
		
		means <- lapply(means.args, function(x) meanslist)
		attr(means, "args.list") <- means.args

		runs <- list(freqs = freqs, 
				cfreqs = cfreqs, 
				means = means, 
				summaries = meanslist,
				quantiles = meanslist
		)

		# return new object
		proto(.,
				name=name,
				outcomes=list(),
				runs=runs,
				results=list()
		)
	}

	#' Simulate outomes and store them in the outcomes list.
	#' 
	#' Sub-classes should extend this function.
	#' 
	#' @param .
	#'  Simmodule receiving object
	#' @param simenv
	#'  simulation environment object
	#' 
	#' NB: simenv$simframe will be re-used in subsequent runs so should NOT be modified
	simulateOutcomes <- function (., simenv) {
		
	}
	
	#' Calculate and append run results, based on the values in outcomes.
	#' Called at the end of each run.
	#' 
	#' @param .
	#'  Simmodule receiving object. .$runs is modified.
	#'   	
	#' @examples
	#' 
	#' . <- env.base$modules$years1_5
	#' . <- env.base$modules$years6_13
	#' 
	#' appendRunResults (.)
	appendRunResults <- function (.) {
		
		cat(gettextf("Generating run results for %s\n", .$name))
		
		.$runs$freqs <- lapply.subset.append.lol.args(.$outcomes, .$runs$freqs, simplify = FALSE, .FUN=table.grpby.mx.cols)
		
		.$runs$summaries <- lapply.subset.append (.$runs$summaries, .$outcomes, .FUN=summary.mx) 
		
		# add additional "all years" row totals to continuous vars
		# used in means, quantiles, and cfreqs
		convars <- unique(c(names(.$runs$means[[1]]), names(.$runs$quantiles)))
		
		outcomes_wtotals <- lapply.subset(.$outcomes, convars, function(x) {
					#x <- .$outcomes$gpmorb
					structure(cbind(x, "All Years"=rowSums(x, na.rm=TRUE)), varname=attr(x,"varname"))
				})
		
		.$runs$means <- lapply.subset.append.lol.args(outcomes_wtotals, .$runs$means, .FUN= wtdmeancols)

		.$runs$cfreqs <- lapply.subset.append (.$runs$cfreqs, outcomes_wtotals, simplify = FALSE, .FUN=table.grpby.mx.cols)
		
		#.$runs$quantiles <- lapply.subset.append (.$runs$quantiles, outcomes_wtotals, .FUN=quantile.mx, 
		#		probs=seq(0, 1, 0.02), na.rm = TRUE)

		.$runs$quantiles <- lapply.subset.append (.$runs$quantiles, outcomes_wtotals, .FUN=quantile.mx, 
				new.names=c("Min", "20th", "40th", "60th","80th","Max"), probs=seq(0,1,0.2), na.rm = TRUE)
		
		
		return()
	}
	
	#' Calculate and store mean of values across all runs (i.e: final results).
	#' Called at the end of the simulation.
	#' 
	#' @param .
	#'  Simmodule receiving object. .$runs is modified.
	#' @examples
	#' 
	#' . <- env.base$modules$years1_5
	#' . <- env.base$modules$years6_13
	#' . <- env.scenario$modules$years1_5
	#' simenv <- env.base
	#' simenv <- env.scenario
	#' .$calcFinalResults(simenv) 
	calcFinalResults <- function(., simenv) {
		cat(gettextf("Generating final results for %s\n", .$name))
		
		.$results <- list()
		
		#.$results$freqs <- lapply(.$runs$freqs, mean.list.var.run.mx)
		.$results$freqs <- lapply.inner(.$runs$freqs, finialise.lolmx, dict = simenv$dict)
		
		#.$results$cfreqs <- mean.list.var.run.mx(.$runs$cfreqs, removeZeroCategory = FALSE, asPercentages = FALSE)
		.$results$cfreqs <- lapply(.$runs$cfreqs, finialise.lolmx, dict = simenv$dict, removeZeroCategory = FALSE)
		.$results$histo <- lapply(.$runs$cfreqs, finialise.lolmx, dict = simenv$dict, asPercentages = F, removeZeroCategory = FALSE, CI = T)
		
		.$results$means <- lapply.inner(.$runs$means, mean.array.z)
		.$results$summaries <- lapply(.$runs$summaries, mean.array.z)
		.$results$quantiles <- lapply(.$runs$quantiles, mean.array.z)
		
		.$results$means  <- lapply.inner(.$results$means, function(x) labelColumnCodes(x, simenv$dict, attr(x, "meta")["grpby.tag"]) )
		
		# set non-existant colnames to "Mean"
		.$results$means <- lapply(.$results$means, function(x) labelCols.list(x, "Mean"))
		
		
		return()
	}
	
	
	clone <- function(.) as.proto(.$as.list(all.names=TRUE))
	
	class <- function(.) "Simmodule"
})
