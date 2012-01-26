library(proto)

#' Simenv object - a simulation environment.
#'
#' A simulation environment contains everything required to perform a simulation. Typically 1 Simenv will be created 
#' and used to run a base simulation, and additional Simenvs will be created to test different scenarios.
#'
#' A Simenv consists of a:
#' - a simframe (possibly with adjustments to test a scenario)  
#' - one or more simulation modules (Simmodule). A Simmodule contains outcomes, run stats, and results for a simulation
#'   as well as the code to generate them.
#' 
#' This class will be subclassed by specific simulation problems which will provide their own simframe,
#' Simmodules and adjustments.  
#'  
#' 
Simenv <- proto(
. = .GlobalEnv,  # parent environment is .GlobalEnv, rather than the package namespace 
expr = {  
	numCases <- NULL
	numRuns <- 0L
	envName <- NULL
	simframe <- NULL
	dict <- NULL
	base.tables <- list()

	#' Categorical variable adjustment matrices.
	#' 
	#' Each element is an adjustment matrix:
	#' 
    #'            Non-smoker (%) Smoker (%)
	#'     Year 1             NA         NA
	#'     Year 2             NA         NA
	#' attr(,"varnames")
	#' [1] "z1homeownLvl0" "z1homeownLvl1"
	#' 
	#' The values in the first row are used to make adjustments before the simulation begins.
	#' Values in subsequent rows can be used during the simulation to set the required proportion
	#' during the specified iteration (eg: iteration 2 if a value is specified in Year 2).
	#' The variables in the simframe to adjust are specified by the varnames attribute.
	catadjs <- list()
			
	modules <- list()

	#' Create new simenv object
	#' 
	#' @param name
	#'  simulation name
	#' @param simframe
	#'  simframe
	#' @param catadjs
	#'  categorical adjustments
	#' 
	#' @examples
	#' env <- Simenv$new(name = "Base", simframe=simframe.start)
	new <- function (., name, simframe, dict, catadjs=list(level.vars=list(), nonlevel.vars=list())) {
		proto(.,
				simframe=simframe,
				dict=dict,
				numCases=dim(simframe)[1],
				numRuns <- 0L,
				envName=name,
				base.tables=list(),
				catadjs=catadjs,
				modules=list()
		)
	}
	
	clone <- function(.) as.proto(.$as.list(all.names=TRUE))
	
	class <- function(.) "Simenv"
	
	#' Apply categorical adjustments to simframe.
	#' 
	#' @param .
	#'  simenv receiving object. .$simframe is modified.  
	#' 
	#' @param propens.all
	#' 		named list of propensities for the catadjs
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
	#' 	.$catadjs$z1accomLvl1[1,] <- c(0.5,0.5)
	#'  .$catadjs$SESBTH[1,] <- c(0.1,0.1,0.8)
	#'  .$catadjs$catpregsmk2[1,] <- c(0.01,0.02,0.03,0.04,0.90)
	#' 
	#'  print(prop.table(table(.$simframe$z1accomLvl1)),digits=3)
	#'  print(prop.table(table(binary.levels.combine(.$simframe$SESBTHLvl1 , .$simframe$SESBTHLvl2, .$simframe$SESBTHLvl3))),digits=3)
	#'  print(prop.table(table(.$simframe$catpregsmk2)),digits=3)	
	#' 
	#' .$applyCatAdjustmentsToSimframe(iteration, propens.all, print_adj)
	applyCatAdjustmentsToSimframe <- function(., iteration, propens.all, print_adj = TRUE) {

		invisible(lapply(.$catadjs, function (catadj) {
			#catadj <- .$catadjs$SESBTH
			#catadj <- .$catadjs$catpregsmk2
			cat_adj_vector <- catadj[iteration, ]
			
			if (!any(is.na(cat_adj_vector))) {
				
				varnames <- attr(catadj, "varnames")
				is_single_variable_to_adjust <- length(varnames) == 1
				
				if (is_single_variable_to_adjust) {
					propens <- propens.all[[varnames]][,,iteration]
					.$applyCatAdjustmentsToSimframeVarSingle(varnames, cat_adj_vector, propens, print_adj)
				} else {
					propens <- propens.all[[strip_lvl_suffix(varnames[1])]][,,iteration]
					.$applyCatAdjustmentsToSimframeVarMultipleBinary(varnames, cat_adj_vector, propens, print_adj)	
				}
			}
			
		}))

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
	#' varname <- "catpregsmk2"
	#' desired_props <- c(0.01,0.02,0.03,0.04,0.90)
	#' propens <- NULL
	#' print_adj = T
	#' applyCatAdjustmentsToSimframeVarSingle(., varname, desired_props, propens, print_adj)
	applyCatAdjustmentsToSimframeVarSingle <- function(., varname, desired_props, propens, print_adj = T) {
		if (print_adj) cat(varname,"\n")
		
		.$simframe[varname] <- modifyProps(.$simframe[[varname]], desired_props, propens)
		
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
	#' propens=propens.all$SESBTH[,,1]
	#' 
	#' binLevelVarnames <- c("z1single0Lvl0","z1single0Lvl1")
	#' binLevelVarnames <- c("z1accomLvl0","z1accomLvl1") ; propens <- propens.all$z1accom[,1]
	#' desiredProps <- c(0,1) ; desiredProps <- c(0.5,0.5)  
	#' propens <- NULL 
	#' 
	#' .$applyCatAdjustmentsToSimframeVarMultipleBinary(binLevelVarnames, desiredProps, propens, TRUE)
	applyCatAdjustmentsToSimframeVarMultipleBinary <- function (., binLevelVarnames, desiredProps, propens, printAdj = TRUE) {
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
				propens=propens)
		
		.$simframe[varnames] <- result[varnames] 
		
		if (printAdj) {
			print(apply(.$simframe[varnames], COL, sum) / apply(.$simframe[varnames], COL, length), digits=3)
			cat("\n")
		}
	}
	
	#' Generate base tables after adjust but before simulation begins.
	#' 
	#' Sub-classes override this function.
	#' 
	baseTablesGenerate <- function(., simframe) {
		
	}
	
	#' Perform a simulation of X runs
	#' 
	#' @param .
	#'  Simenv receiving object
	#' @param numrums
	#'  number of simulation runs
	#' @return 
	#'  NULL
	#' @examples 
	#'  . <- env.base
	#'  env.base$simulate()
	simulate <- function(., numruns=1) {
		pt.start <- proc.time()
		
		cat(gettextf("Simulating %s\n", .$envName))
		
		.$applyCatAdjustmentsToSimframe(1, propens.all)
		
		.$base.tables <- .$baseTablesGenerate(.$simframe)
		
		if (exists(".DEBUG")) {
			cat("DEBUG: Stopping to allow manual execution\n")
			return()
		}
		
		for (i in 1:numruns) {
			cat("Run",i,"of",numruns,"\n")

			.$numRuns <- .$numRuns + 1

			# simulate module outcomes
			
			invisible(lapply(.$modules, function(simmodule) #simmodule <- .$modules[[1]] 
								simmodule$outcomes <- simmodule$simulateRun(simenv=.)  ))
			
			# append module run results
			invisible(lapply(.$modules, function(simmodule) simmodule$appendRunStats()))
		}
		
		# calc final results
		invisible(lapply(.$modules, function(simmodule) simmodule$calcFinalResults(simenv=.)))

		pt.end <- proc.time()
		
		return(pt.end - pt.start)
		
	}
	
})
