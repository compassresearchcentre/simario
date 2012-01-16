library(proto)

#' Simenv object - a simulation environment.
#'
#' Contains a simframe, one or more simulation modules, and adjustments required to test scenarios.
#' This class will be subclassed by specific simulation problems which will provide their own simframe,
#' Simmodules and adjustments.  
#'  
#' Typically 1 Simenv will be created and used to run a base simulation, and additional Simenvs will be
#' created to test different scenarios.
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

	#' Categorical variable adjustments.
	#' 
	#' A list containing:
	#' 
	#' $level.vars -
	#'   a named list of matrices giving desired proportions for a level var, 
	#'   where columns = each category, rows = each iteration.
	#'   A level var represents a set of binary variables that make up a single categorical variable,
	#'   eg: SESBTHLvl1, SESBTHLvl2, SESBTHLvl3   
	#' $nonlevel.vars -
	#'   a named list of matrices giving desired proportions for a categorical var, 
	#'   where columns = each category, rows = each iteration.
	catadjs <- list(level.vars=list(), 
					nonlevel.vars=list())
			
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

	#' Apply adjustments to simframe.
	#' 
	#' @param .
	#'  simenv recieving object. .$simframe is modified.  
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
	#'  simframe <- simframe.start
	#'  catadjs$level.vars$SESBTH[1,] <- c(0.1,0.1,0.8)
	#'  catadjs$level.vars$r1stfeduc[1,] <- c(0.3,0.3,0.4)
	#'  catadjs$level.vars$z1chres[1,] <- c(0.4,0.6)
	#' 	catadjs$level.vars$z1accom[1,] <- c(0.5,0.5)
	#'  catadjs$nonlevel.vars$catpregsmk2[1,] <- c(0.01,0.02,0.03,0.04,0.90)
	#' 
	#'  print(prop.table(table(combine3Levels(simframe$SESBTHLvl1 , simframe$SESBTHLvl2, simframe$SESBTHLvl3))),digits=3)
	#'  print(prop.table(table(simframe$z1chresLvl1)),digits=3)	
	#'  print(prop.table(table(simframe$z1accomLvl1)),digits=3)
	#' 
	#' iteration = 1 ; printAdj = TRUE
	#' adjustSimframe(iteration, propens.all, printAdj)
	adjustSimframe <- function(., iteration, propens.all, printAdj = TRUE) {
		
		# 1. process adjustments for non level vars
		desiredProps.list <- select.row.list.mx(.$catadjs$nonlevel.vars, iteration, na.rm = T) 
		propens.list <- lapply(propens.all[names(desiredProps.list)], function(x) x[,,iteration])
		.$adjustSimframeCatNonLevelVars.list(desiredProps.list, propens.list)
		
		# 2. process adjustments for level vars
		desiredProps.list <- select.row.list.mx(.$catadjs$level.vars, iteration, na.rm = T)
		level.var.names <- names(desiredProps.list)
		lvls.list <- .$catadjs$level.vars.lvls[level.var.names]
		propens.list <- lapply(propens.all[level.var.names], function(x) x[,,iteration])
		.$adjustSimframeCatLevelVars.list(desiredProps.list, lvls.list, propens.list)
		
	}
	
	#' Adjust the proportions of a named list of simframe variables.
	#' 
	#' @param .
	#'  simenv recieving object. .$simframe is modified.  
	#' @param desiredProps
	#'  a NAMED list of desired proportion vectors. The name of each vector is the name of
	#'  a variable in simframe to modify, eg: list(catpregsmk2=c(0.01,0.02,0.03,0.04,0.90))
	#' @param propens
	#'  a parallel list of propensities to use, eg: list(catpregsmk2=NULL)
	#' @param printAdj
	#'  if TRUE, display adjusted proportions after adjustment
	#' 
	#' @return 
	#'  NULL. simframe in receiving object is modified directly.
	#' 
	#' @examples
	#' 
	#' simframe <- simframe.start
	#' desiredProps.list <- list()
	#' desiredProps.list <- list(catpregsmk2=c(0.01,0.02,0.03,0.04,0.90))
	#' propens.list <- list(catpregsmk2=NULL)
	#' printAdj = TRUE 
	#' adjustSimframeCatNonLevelVars.list(desiredProps.list, propens.list, printAdj)
	adjustSimframeCatNonLevelVars.list <- function(., desiredProps.list, propens.list, printAdj = TRUE) {
		
		nl.var.names <- names(desiredProps.list)
		if (!is.null(nl.var.names)) {
			invisible(mapply(function(varname, desiredProps, propens)
							{
								#varname <- names(desiredProps.list)[[1]]
								#desiredProps <- desiredProps.list[[1]]
								#propens <- propens.list[[1]]
								
								if (printAdj) cat(varname,"\n")
								
								.$simframe[varname] <- modifyProps(.$simframe[[varname]], desiredProps, propens)
								
								if (printAdj) {
									print(prop.table(table(.$simframe[varname])),digits=3)
								}
								
							},
							nl.var.names,
							desiredProps.list,
							propens.list, 
							SIMPLIFY=FALSE))
		} 
	}
	
	#' Adjust the proportions of a named list of simframe variables that exists in binary level vectors.
	#' 
	#' @param .
	#'  simenv recieving object. .$simframe is modified.  
	#' @param desiredProps
	#'  a named list of desired proportion vectors. The name of each vector is the base name of
	#'  the binary level vectors to modify, eg: list(SESBTH=c(0.1,0.1,0.8),z1single0=c(0,1))
	#' @param lvls
	#'  a parallel list of lvls to modify, eg:  list(SESBTH=c(1,2,3), z1single0=c(0,1))
	#' @param propens
	#'  a parallel list of propensities to use, eg: list(SESBTH=propens.all$SESBTH, z1single0Lvl1=NULL)
	#' @param printAdj
	#'  if TRUE, display adjusted proportions after adjustment
	#' 
	#' @return 
	#'  NULL. simframe in receiving object is modified directly.
	#' 
	#' @examples
	#' 
	#' simframe <- simframe.start
	#' desiredProps.list <- list(SESBTH=c(0.1,0.1,0.8),z1single0=c(0,1), z1accom=c(0.5,0.5))
	#' lvls.list <- list(SESBTH=c(1,2,3), z1single0=c(0,1), z1accom=c(0,1))
	#' propens.list <- list(SESBTH=propens.all$SESBTH[,,1], z1single0=NULL, z1accom=propens.all$z1accom[,,1])
	#' printAdj = TRUE 
	#' adjustSimframeCatLevelVars.list(desiredProps.list, lvls.list, propens.list, printAdj)
	adjustSimframeCatLevelVars.list <- function(., desiredProps.list, lvls.list, propens.list, printAdj = TRUE) {
		
		invisible(mapply(function(varname, desiredProps, levelvalues, propens) {
							# index = 4; varname <- names(desiredProps.list)[[index]] ; desiredProps <- desiredProps.list[[index]] 
							# levelvalues <- lvls.list[[index]] ; propens <- propens.list[[index]]
							levelvarnames <- paste(varname, "Lvl", levelvalues, sep="")
							.$adjustSimframeCatLevelVars(
									binLevelVarnames=levelvarnames, desiredProps=desiredProps, 
									propens=propens, printAdj=printAdj, levelvalues=levelvalues)
							
						}, names(desiredProps.list), desiredProps.list, lvls.list, propens.list, SIMPLIFY=F))
	}	
	
	#' Adjust the proportions of a simframe variable that exists in binary level vectors,
	#' (eg: SESBTHLvl1, SESBTHLvl2, SESBTHLvl3), and print the adjusted result.  
	#' 
	#' @param .
	#'  simenv recieving object. .$simframe is modified.  
	#' @param binLevelVarnames
	#'  vector of binary level varnames
	#' @param desiredProps
	#'  desired proportions
	#' @param propens
	#'  propensities, if ANY
	#' @param printAdj
	#'  if TRUE, display adjusted proportions after adjustment
	#' @param levelvalues
	#'  if supplied, used during as labels during printing of adjusted proportions
	#'
	#' @return 
	#'  NULL. simframe in receiving object is modified directly.
	#'  
	#' @examples
	#' simframe <- simframe.start
	#' binLevelVarnames <- c("SESBTHLvl1","SESBTHLvl2", "SESBTHLvl3") 
	#' desiredProps=catadjs$level.vars$SESBTH ; desiredProps=c(0.1,0.1,0.8)
	#' propens=propens.all$SESBTH
	#' 
	#' binLevelVarnames <- c("z1single0Lvl0","z1single0Lvl1")
	#' binLevelVarnames <- c("z1accomLvl0","z1accomLvl1") ; propens <- propens.all$z1accom[,1]
	#' desiredProps <- c(0,1) ; desiredProps <- c(0.5,0.5)  
	#' propens <- NULL 
	#' 
	#' adjustSimframeCatLevelVars(binLevelVarnames, desiredProps, propens, TRUE)
	adjustSimframeCatLevelVars <- function (., binLevelVarnames, desiredProps, propens, printAdj = TRUE, levelvalues = NULL) {
		if	(any(!is.na(desiredProps))) {
			
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
			
			if (printAdj) cat(varnames,":")
			
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
		
		.$adjustSimframe(1, propens.all)
		
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
								simmodule$outcomes <- simmodule$simulateOutcomes(simenv=.)  ))
			
			# append module run results
			invisible(lapply(.$modules, function(simmodule) simmodule$appendRunResults()))
		}
		
		# calc final results
		invisible(lapply(.$modules, function(simmodule) simmodule$calcFinalResults(simenv=.)))

		pt.end <- proc.time()
		
		return(pt.end - pt.start)
		
	}
	
})
