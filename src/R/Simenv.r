library(proto)

#' Simenv object - a simulation environment.
#'
#' A simulation environment contains everything required to perform a simulation. Typically 1 Simenv will be created 
#' and used to run a base simulation, and additional Simenvs will be created to test different scenarios.
#'
#' A Simenv consists of a:
#' - a simframe (possibly with adjustments to test a scenario)  
#' - one or more simulation modules (Simmodule). A Simmodule contains outcomes, run stats, and runs.averaged for a simulation
#'   as well as the code to generate them.
#' 
#' Uses the global environment list variable "propensities" when performing categorical adjustment  
#' 
#' This class will be subclassed by specific simulation problems which will provide their own simframe,
#' Simmodules and adjustments.  
#'  
#' @export 
Simenv <- proto(
expr = {  

	#' Create new simenv object
	#' 
	#' @param name
	#'  simulation name
	#' 
	#' @param simframe
	#'  simframe
	#' 
	#' @param dict
	#'  a Dictionary object
	#' 
	#' @param cat.adjustments
	#' Categorical variable adjustment matrices.
	#' 
	#' Each element is an adjustment matrix:
	#' 
	#'            Non-smoker (%) Smoker (%)
	#'     Year 1             NA         NA
	#'     Year 2             NA         NA
	#' attr(,"varnames")
	#' [1] "z1msmokeLvl0" "z1msmokeLvl1"
	#' 
	#' The values in the first row are used to make adjustments before the simulation begins.
	#' Values in subsequent rows can be used during the simulation to set the required proportion
	#' during the specified iteration (eg: iteration 2 if a value is specified in Year 2).
	#' The variables in the simframe to adjust are specified by the varnames attribute.
	#' 
	#' @param cont.adjustments
	#' A list of time-variant continuous variable adjustment matrices.
	#' 
	#' Each element is an adjustment matrix with number of rows equal to the number of micro units
	#' amd number of columns equal to the number of iterations plus 1 (for the presimulation adjustments).
	#' 
	#' The user specifies from the user interface desired increments (or decrements) for all micro units
	#' in particular categories (e.g. decrease the number of cigarettes smoked per day by 20 for every 
	#' child with a mother who smokes 40 or more cigarettes a day), these adjustments are made to the 
	#' simulated data from the base simulation and results stored in these matrices.  At each year in the 
	#' simulation these cont.adjustment matrices are checked and, if they contain values, they are used
	#' instead of the simulated values at that year. 
	#' 
	#' @param modules
	#'  the list of Simmodules for this Simenv
	#' 
	#' @examples
	#' env <- Simenv$new(name = "Base", simframe=simframe.master, dict=dict_demo)
	new <- function (., name, simframe, dict, cat.adjustments=list(), modules=list()) {
		proto(.,
				name=name,
				num_runs_simulated <- 0L,
				simframe=simframe,
				presim.stats=list(),
				cat.adjustments=cat.adjustments,
				modules=modules,
				dict=dict
		)
	}
	
	clone <- function(.) as.proto(.$as.list(all.names=TRUE))
	
	class <- function(.) "Simenv"
	
	#' Apply categorical adjustments to simframe.
	#' 
	#' @param .
	#'  simenv receiving object. .$simframe is modified.  
	#' 
	#' @param propensities
	#' 		named list of propensities for the cat.adjustments
	#' @param printAdj
	#' 		if TRUE will print new proportions of modified simframe vars
	#'
	#' @return 
	#'  NULL. simframe in receiving object is modified directly.
	#'   
	#' @examples
	#'  . <- env.scenario
	#' iteration = 1 ; print_adj = TRUE
	#' 
	#' 	.$cat.adjustments$z1accomLvl1[1,] <- c(0.5,0.5)
	#'  .$cat.adjustments$SESBTH[1,] <- c(0.1,0.1,0.8)
	#'  .$cat.adjustments$catpregsmk2[1,] <- c(0.01,0.02,0.03,0.04,0.90)
	#' 
	#'  print(prop.table(table(.$simframe$z1accomLvl1)),digits=3)
	#'  print(prop.table(table(binary.levels.combine(.$simframe$SESBTHLvl1 , .$simframe$SESBTHLvl2, .$simframe$SESBTHLvl3))),digits=3)
	#'  print(prop.table(table(.$simframe$catpregsmk2)),digits=3)	
	#' 
	#' .$applyAllCatAdjustmentsToSimframe(iteration, propensities, print_adj)
	applyAllCatAdjustmentsToSimframe <- function(., iteration, propensities, print_adj = TRUE,...) {

		invisible(lapply(.$cat.adjustments, function (catadj) {
			#catadj <- .$cat.adjustments[[1]]
			#catadj <- .$cat.adjustments$SESBTH
			#catadj <- .$cat.adjustments$catpregsmk2
			cat_adj_vector <- catadj[iteration, ]
			
			if (!any(is.na(cat_adj_vector))) {
				
				varnames <- attr(catadj, "varnames")
				if (is.null(varnames)) {
					stop(gettextf("Missing varnames attribute"))
				}
				.$applyCatAdjustmentToSimframe(varnames, cat_adj_vector, iteration, propensities, print_adj, ...)
			}
			
		}))

	}

	#' Apply categorical adjustments to simframe.
	#' 
	#' @param .
	#'  simenv receiving object. .$simframe is modified.  
	#' @param varnames
	#'  varname(s) of variable(s) to adjust, eg: "catpregsmk2" or c("z1msmokeLvl0","z1msmokeLvl1")
	#' @param desired_props
	#'  a vector of desired proportions, eg: c(0.1, 0.1, 0.8)
	#' @param propensities
	#' 		named list of propensities for the cat.adjustments
	#' @param printAdj
	#' 		if TRUE will print new proportions of modified simframe vars
	#'
	#' @return 
	#'  NULL. simframe in receiving object is modified directly.
	#' 
	#' @examples
	#'  desired_props <- cat_adj_vector  
	applyCatAdjustmentToSimframe <- function(., varnames, desired_props, iteration, propensities, print_adj = TRUE, ...) {
		is_single_variable_to_adjust <- length(varnames) == 1
		
		if (is_single_variable_to_adjust) {
			propens <- propensities[[varnames]][,,iteration]
			.$applyCatAdjustmentToSimframeVarSingle(varnames, desired_props, propens, print_adj, ...)
		} else {
			propens <- propensities[[strip_lvl_suffix(varnames[1])]][,,iteration]
			.$applyCatAdjustmentToSimframeVarMultipleBinary(varnames, desired_props, propens, print_adj, ...)	
		}
		
	}

	#' Adjust the proportions of a single simframe variable.
	#' 
	#' @param .
	#'  simenv receiving object. .$simframe is modified.
	#' @param varname
	#'  simframe variable to adjust  
	#' @param desired_props
	#'  a vector of desired proportions, eg: c(0.1, 0.1, 0.8)
	#' @param propens
	#'  propensities for this variable, if any
	#' @param printAdj
	#'  if TRUE, display adjusted proportions after adjustment
	#' 
	#' @return 
	#'  NULL. simframe in receiving object is modified directly.
	#' 
	#' @examples
	#' . <- env.scenario
	#' varname <- "catpregsmk2" ; varname <- varnames
	#' desired_props <- c(0.01,0.02,0.03,0.04,0.90)
	#' propens <- NULL
	#' print_adj = T
	#' applyCatAdjustmentToSimframeVarSingle(., varname, desired_props, propens, print_adj)
	applyCatAdjustmentToSimframeVarSingle <- function(., varname, desired_props, propens, print_adj = T, ...) {
		if (print_adj) cat(varname,"\n")
		
		.$simframe[varname] <- modifyProps(.$simframe[[varname]], desired_props, propens, ...)
		
		if (print_adj) {
			print(prop.table(table(.$simframe[varname])), digits=3)
		}
	}
	
	#' Adjust the proportions of a simframe variable that exists in multiple binary level vectors,
	#' eg: SESBTHLvl1, SESBTHLvl2, SESBTHLvl3.
	#' 
	#' @param .
	#'  simenv receiving object. .$simframe is modified.  
	#' @param binLevelVarnames
	#'  vector of binary level varnames, eg: c("z1accomLvl0","z1accomLvl1")
	#' @param desiredProps
	#'  desired proportions
	#' @param propens
	#'  propensities, if ANY
	#' @param printAdj
	#'  if TRUE, display adjusted proportions after adjustment
	#'
	#' @return 
	#'  NULL. simframe in receiving object is modified directly.
	#'  
	#' @examples
	#' . <- env.scenario
	#' binLevelVarnames <- c("SESBTHLvl1","SESBTHLvl2", "SESBTHLvl3") 
	#' desiredProps=c(0.1,0.1,0.8)
	#' propens=propensities$SESBTH[,,1]
	#' 
	#' binLevelVarnames <- c("z1single0Lvl0","z1single0Lvl1")
	#' binLevelVarnames <- c("z1accomLvl0","z1accomLvl1") ; propens <- propensities$z1accom[,1]
	#' desiredProps <- c(0,1) ; desiredProps <- c(0.5,0.5)  
	#' propens <- NULL 
	#' 
	#' .$applyCatAdjustmentToSimframeVarMultipleBinary(binLevelVarnames, desiredProps, propens, TRUE)
	applyCatAdjustmentToSimframeVarMultipleBinary <- function (., binLevelVarnames, desiredProps, propens, printAdj = TRUE, ...) {
		#NB: simframe may not always contain Lvl0 var. So we construct one if this is 2 level var.
		is2Level <- length(binLevelVarnames) == 2
		varnames <- intersect(binLevelVarnames, names(.$simframe))
		missingLevel <- setdiff(binLevelVarnames, names(.$simframe))
		
		vecs.list <- .$simframe[varnames]
		
		if(is2Level && length(missingLevel)) {
			# add in generated missing level
			vecs.list[missingLevel] <- as.integer(!.$simframe[varnames])
			
			# order correctly
			vecs.list <- vecs.list[binLevelVarnames] 
		}
		
		result <- modifyPropsAsBinLevels(
				vecs.list, 
				desiredProps=desiredProps, 
				propens=propens, ...)
		
		.$simframe[varnames] <- result[varnames] 
		
		if (printAdj) {
			print(apply(.$simframe[varnames], COL, sum) / apply(.$simframe[varnames], COL, length), digits=3)
			cat("\n")
		}
	}
	
	#' Generate pre simulation stats after adjustment but before simulation begins.
	#' 
	#' Typically these will be descriptive statistics of input variables that don’t change eg: gender, ethnicity
	#' 
	#' Sub-classes override this function.
	#' 
	generatePreSimulationStats <- function(., simframe) {
		
	}
	
	#' Perform a simulation of X runs.
	#' 
	#' NB: if it exists, uses propensities in global environment when doing adjustments for year 1
	#' 
	#' @param .
	#'  Simenv receiving object
	#' @param total_runs
	#'  total number of runs to simulate
	#' @return 
	#'  NULL
	#' @examples 
	#'  . <- env.base
	#'  env.base$simulate()
	simulate <- function(., total_runs=1,...) {
		start_time <- proc.time()
		
		cat(gettextf("Simulating %s\n", .$name))
		
		if (!exists("propensities")) propensities <- NULL
		
		.$applyAllCatAdjustmentsToSimframe(1, propensities,...)
		
		.$presim.stats <- .$generatePreSimulationStats(.$simframe)
		
		if (exists(".DEBUG")) {
			cat("DEBUG: Stopping to allow manual execution\n")
			return()
		}
		
		for (i in 1:total_runs) {
			#i = 1
			cat("Run",i,"of",total_runs,"\n")

			#execute simulateRun on all modules (may only be one module)
			invisible(lapply(.$modules, function(module) #module <- .$modules[[1]] 
								module$outcomes <- module$simulateRun(simenv=.)  ))
			
			#execute map_outcomes_to_run_results on all modules and store run results
			invisible(lapply(.$modules, function(module) {
								#module <- .$modules[[1]]
								run_results <- module$map_outcomes_to_run_results(.$simframe, module$outcomes)
								module$run_results <- c(module$run_results, list(run_results))
								names(module$run_results)[i] <- paste("run", i, sep="")
							}))
			
			.$num_runs_simulated <- .$num_runs_simulated + 1
			
		}
		
		invisible(lapply(.$modules, function(module) {
							module$run_results_collated <- module$collate_all_run_results(module$run_results)
						}))

		# call garbage collector to release memory used during calculation (sometimes this is a lot)
		gc()
		
		end_time <- proc.time()
		
		return(end_time - start_time)
		
	}
	
	numberOfUnits <- function(.) {
		dim(.$simframe)[1]
	}
})
