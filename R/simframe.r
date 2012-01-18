# Functions related to the simframe.
#
# A simframe is a dataframe of all variables used by any modules during the simulation.
# Each variables continains observations (or values) for each micro-unit.
# These variables are used during the simulation as input, or intermediate values or outcomes.
#
# During simulation the simframe will be transformed to contain the current iteration's 
# set of values. At the beginning of each iteration, the current state of the variable can be saved
# in a separate "previous" variable in the simframe. This is useful for models that 
# require state information from previous iterations (i.e: time periods).
# 
# Author: Oliver Mannion
###############################################################################


#' Checks that the variables used in all of the models
#' exist in simvalues, if not generate stop
#' @param models
#'  a list of glm objects
#' @param simframe
#'  the simframe
#' 
#' @examples
#' #simframe <- simframe.start
#' #checkModelVars(models, simframe)
checkModelVars <- function (models, simframe) {
	# try a predict on each model
	predtest <- lapply(models, function (model) try({
							#model <- models$gptotvis
							#predict(model, simframe)
							model.terms <- attr(delete.response(terms(model)), "variables")
							eval(model.terms, simframe)
						}, silent = TRUE))
	
	# gather errors
	tryerrs <- tryerrorMsgs(predtest) 
	
	if (length(tryerrs) > 0) {
		# concate errors together with model name
		msg <- paste("Model ",names(tryerrs),":",sub(".*:", "", tryerrs ), sep="",collapse="")
		stop(msg)
	}
}

#' Checks that the outcome variables have a source variable in simframe.
#'  
#' @param outcomes list of outcome variables with a varname attribute.
#' @param simframe simframe to test for presence of varname
#' @return nothing if successful, otherwise errors
#' @examples
#' #checkOutcomeVars(createMELCfoutcomes(children),simframe) #outcomes <- createMELCfoutcomes(children) 
checkOutcomeVars <- function(outcomes, simframe) {
	#check varname specified on outcome var exists in simframe
	srcvar <- unlist(lapply(outcomes, attr, "varname"))
	nonexistent_srcvar <- !(srcvar %in% names(simframe))
	if (any(nonexistent_srcvar)) {
		stop(gettextf("src variable(s) %s: not in simframe", paste(srcvar[nonexistent_srcvar],collapse=", ")))
	}
}

#' Create a list of empty outcome matrix variables for a specified outcome set.
#' The variables to create are those from the outcome set specified by "outcome_module_name". 
#' 
#' @param simframe simulation frame
#' @param outcome_module_name name of the outcomeset to create variables for
#' @param iterations either a vector of names for each iteration,
#'  eg: ("Year 5", "Year 6" ...) or a scalar specifying the number 
#'  of iterations.
#' 
#' @return 
#' 
#' A dataframe of outcome variables. Each output variable is a matrix 
#' of simvalues by iterations. Each matrix is assigned the "varname" 
#' attribute that names the source variable in the simframe that will be used 
#' during simulation to fill the matrix. This source variable is the
#' same name as the name outcome matrix.
#' 
#' @examples
#' \dontrun{
#' simframe <- simframe.start
#' simframe <- env.base$simframe
#' outcome_module_name <- "years1_5"
#' outcome_module_name <- "years6_13"
#' iterations <- 8
#' iterations <- c(6:13)
#' outcomes <- createOutcomeMatrices(simframe, outcome_module_name, iterations)
#' }
createOutcomeMatrices <- function (simframe, outcome_module_name, iterations) {
	setVars <- getOutcomeVars(simframe, outcome_module_name=outcome_module_name, sorted=TRUE)
	
	outcomes <- lapply(setVars, function (var) 
				createOutputMatrix(var, length(simframe[[var]]), 
						iterations) )
	
	#convert list to dataframe 
	as_data_frame_list_as_is(outcomes, row.names(simframe))
}

#' Create a matrix of NA with specified col/row names/lengths.
#' 
#' @param cols columns names, or a numeric scalar for the number of cols
#' @param rows row names, or a numeric scalar for the number of rows
#' @param simvarname 
#' 		simframe source var. stored in the "simvar" attribute.
#'      of the matrix. This is the name of the variable in the
#' 		simframe that will be used to fill this matrix during simulation
#' @return
#' a matrix with the "varname" attribute set to simvarname
#' 
#' @examples 
#' \dontrun{
#' rows <- length(children$z1msmoke1)
#' cols <- 5
#' simvarname <- "z1msmokeLvl1"
#' createOutputMatrix(simvarname, nrows, ncols)
#' }
createOutputMatrix <- function (simvarname, rows, cols) {
	structure(namedMatrix(rows, cols), varname = simvarname)
}


#' Get a set of outcome variables from the simframe.
#' 
#' @param simframe
#'  simframe
#' @param outcome_type_select
#'  a vector of outcome_types, or NULL to select all types.
#' @param outcome_module_name
#'  a vector of outcome_module, , or NULL to select all sets.
#' @param sorted
#'  return outcome variables sorted by outcome name?
#' 
#' @return
#'  a vector of simframe variables (with names the same as the vector value)
#' 
#' @examples 
#' \dontrun{
#' outcome_module_name = "years1_5"
#' outcome_module_name = c("years1_5", "years6_13")
#' outcome_module_name = NULL
#' 
#' outcome_type_select = "categorical"
#' outcome_type_select = "continuous"
#' outcome_type_select = NULL
#' sorted = TRUE
#' 
#' getOutcomeVars(simframe, outcome_type_select, outcome_module_name, sorted)
#' }
getOutcomeVars <- function(simframe, outcome_type_select=NULL, outcome_module_name=NULL, sorted=FALSE) {
	
	df.outcome.vars <- attr(simframe, "df.outcome.vars")
	outcomeSet <- df.outcome.vars[with(df.outcome.vars, 
					(is.null(outcome_module_name) | outcome_module %in% outcome_module_name)
							& (is.null(outcome_type_select) | outcome_type %in% outcome_type_select )),]
	
	setVars <- outcomeSet$simvarname
	names(setVars) <- outcomeSet$simvarname
	if (sorted)	setVars <- setVars[sort(names(setVars))]
	setVars
}

#' Creates a simulation frame. A simulation frame is the set of all variables
#' (input, intermediate, and outcome) used or produced by models during 
#' a simulation. During simulation the simulation frame will contain the
#' current iteration's set of variables being used in the simulation.
#' This function establishes the values of the simulation frame at the start 
#' prior to simulation using the initial values specified by the simframe
#' definition.
#' 
#' @param envir  
#'  environment to evaluate initial_value in, defaults to global environment. 
#' @param sfdef
#'   simframe definition. A dataframe of variables that make up the simframe, 
#'   defined as follows:
#'       $simvarname - the name of the variable in the simframe
#'       $previous_var - (if any) the name in which to store the variable value in at the beginning
#'                       of each iteration (i.e: before its transformed), for models that require 
#'  					 previous state
#'       $initial_value - an expression that generates the initial value of the variable.
#'                        The expression is evaluated in the context of the base file, and
#'                        if not found then in the context of the calling frame.
#'					      If empty then an initial numeric value of NA is used.
#' 		 $outcome_type - if this is an outcome variable, it's tpype, one of "categorical" or "continuous"
#' 		 $outcome_module - if an outcome variable, the module it belongs to
#'
#' @return
#'  the simulation frame. The simulation frame contains the variables in sfdef with
#'  their inital_value evaluated. Each variable contains a set of observations. Each
#'  observation represents a case in the basefile.
#'  
#'  If an observation is NA, then that case (ie: it's observations for all 
#'  variables) is removed. This however does not apply to initial_value expressions 
#'  that return a singular NA. These remain as a vector containing NA for 
#'  as each observation.
#'   
#'  The frame has same rownames as basefile. This is useful during investigation 
#'  or debugging.
#' 
#' 	The simulation frame object has the following attributes:
#' 
#' 	"previous" - variables that represent values in the previous iteration
#'  "df.outcome.vars" - a dataframe of simvarname, outcome_type, and outcome_module
#'	"na.omit" - an omit variable that indicates the observations that were removed
#' 				because their were NA. Initial_values that returned a singular NA
#' 				remain.
#' 
#' @examples 
#' \dontrun{
#' envir <- children
#' basefiledir <- "D:/workspace.sim/MELC/CHDS/base/"
#' sfdef <- readXLSSheet1(basefiledir, "simframedef.xlsx")
#' simframe.start <- loadSimFrame(envir, sfdef)
#' simframe <- loadSimFrame(envir, sfdef)
#' }
loadSimFrame <- function (envir = .GlobalEnv, sfdef) {
	
	#remove empty variables, generally these are blank lines
	#at the end of the file
	sfdef <- sfdef[!sfdef$simvarname=="", ]
	
	#check for duplicated simvarnames
	dups <- which(duplicated(sfdef$simvarname))
	
	if(length(dups) > 1) {
		stop(paste("Simframe varname duplicate:", sfdef$simvarname[dups], "\n"))
	}
	
	#load initial_value for evaluation
	initial_value_all <- sfdef$initial_value
	names(initial_value_all) <- sfdef$simvarname
	
	#replace empty intial_value with numeric NA
	initial_value_all[(initial_value_all == "")] <- as.numeric(NA)
	
	#evaluate "initial_value" column. Any objects in the exprs must exist 
	#in envir or if not then in the global environment
	sfvalues  <- eval.list(initial_value_all, envir)
	
	# convert non NA values to data.frame
	# this repeats any inital values that are singular
	sfvalues.df <- data.frame(sfvalues[!is.na(sfvalues)])
	
	#remove obs. that have NAs in one of their values
	sfvalues.df <- na.omit(sfvalues.df)
	nas <- attr(sfvalues.df, "na.action")
	
	# add singular NA values back to data.frame
	sfvalues.df  <- cbind(sfvalues.df, 
			as.list(sfvalues[is.na(sfvalues)]), stringsAsFactors=FALSE)
	
	# setup sfprevious
	# sfprevious = the names of the variables that represent values in the previous iteration
	# names(sfprevious) = the source value for the previous variable
	sfprevious <- as.character(sfdef$previous_var)
	names(sfprevious) <- sfdef$simvarname
	sfprevious <- stripEmpty(sfprevious)
	
	#check previous values exist in simvarname
	nonexistent_previous <- !(sfprevious %in% sfdef$simvarname)
	if (any(nonexistent_previous)) {
		stop(gettextf("previous variable(s) %s: not in simframe", 
						paste(sfprevious[nonexistent_previous],collapse=", ")))
	}
	
	#data frame of outcome var mappings, types and set
	df.outcome.vars <- with(sfdef, sfdef[outcome_module != "", c("simvarname", "outcome_type", "outcome_module")])
	
	#return
	structure(sfvalues.df, previous=sfprevious,
			df.outcome.vars = df.outcome.vars,
			na.actions=nas)
}

