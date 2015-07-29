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
#' 
#' @param models
#'  a list of glm objects
#' @param simframe
#'  the simframe
#' 
#' @return 
#'  nothing if successful, otherwise errors
#' 
#' @export
#' @examples
#' #simframe <- simframe.master
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
#' @param outcomes 
#' list of outcome variables with a varname attribute.
#' 
#' @param simframe 
#' simframe to test for presence of varname
#' 
#' @return 
#' nothing if successful, otherwise errors
#' 
#' @export
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
#' @param simframe 
#' simulation frame
#' 
#' @param outcome_module_name 
#' name of the outcomeset to create variables for
#' 
#' @param iterations 
#' either a vector of names for each iteration,
#'  eg: ("Year 5", "Year 6" ...) or a scalar specifying the number 
#'  of iterations.
#' 
#' @return 
#' A dataframe of outcome variables. Each output variable is a matrix 
#' of simvalues by iterations. Each matrix is assigned the "varname" 
#' attribute that names the source variable in the simframe that will be used 
#' during simulation to fill the matrix. This source variable is the
#' same name as the name outcome matrix.
#' 
#' @export
#' @examples
#' \dontrun{
#' simframe <- simframe.master
#' simframe <- env.base$simframe
#' outcome_module_name <- "years1_5"
#' outcome_module_name <- "years6_13"
#' iterations <- 8
#' iterations <- c(6:13)
#' outcomes <- createOutcomeMatrices(simframe, outcome_module_name, iterations)
#' }
createOutcomeMatrices <- function (simframe, outcome_module_name, iterations) {
	setVars <- getOutcomeVars(simframe, outcome_module_name=outcome_module_name, sorted=TRUE)
	
	if (length(setVars) == 0) {
		stop(gettextf("No outcome variables in simframe where Outcome_module=%s", outcome_module_name))
	}
	
	outcomes <- lapply(setVars, function (var) 
				createOutputMatrix(var, length(simframe[[var]]), 
						iterations) )
	
	#convert list to dataframe 
	as_data_frame_list_as_is(outcomes, row.names(simframe))
}



#' Create a matrix of NA with specified col/row names/lengths.
#' 
#' @param cols 
#' columns names, or a numeric scalar for the number of cols
#' 
#' @param rows 
#' row names, or a numeric scalar for the number of rows
#' 
#' @param varname 
#' 	simframe source var. stored in the "varname" attribute.
#'  of the matrix. This is the name of the variable in the
#'  simframe that will be used to fill this matrix during simulation
#' 
#' @return
#' a matrix with the "varname" attribute set to varname
#' 
#' @export
#' @examples 
#' \dontrun{
#' rows <- length(children$z1msmoke1)
#' cols <- 5
#' varname <- "z1msmokeLvl1"
#' createOutputMatrix(varname, nrows, ncols)
#' }
createOutputMatrix <- function (varname, rows, cols) {
	structure(namedMatrix(rows, cols), varname = varname)
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
#' @export
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
	
	outcome_vars <- attr(simframe, "outcome_vars")
	outcomeSet <- outcome_vars[with(outcome_vars, 
					(is.null(outcome_module_name) | Outcome_module %in% outcome_module_name)
							& (is.null(outcome_type_select) | Outcome_type %in% outcome_type_select )),]
	#just saving the variable names of the rows of "outcome_vars" that we want/ have selected above
	setVars <- outcomeSet$Varname
	names(setVars) <- outcomeSet$Varname
	if (sorted)	setVars <- setVars[sort(names(setVars))]
	setVars
}


#' Creates a simulation frame. A simulation frame is the set of all variables
#' (input, intermediate, and outcome) used or produced by models during 
#' a simulation. During simulation the simulation frame will contain the
#' current iteration's set of variables being used in the simulation.
#' This function establishes the values of the simulation frame at the start 
#' prior to simulation by evaluating the initial value expression specified by 
#' the simframe definition.
#' 
#' @param simframe_defn
#'   simframe definition. A dataframe of variables that define the simframe: 
#'       $Varname - the name of a variable in the simframe
#'       $Previous_var - the name of a variable in which to store the current value in at the beginning
#'                       of each iteration (i.e: before it's transformed). 
#' 						 Optional - for models that require previous state.
#'       $Initial_value - an expression that generates the initial value of the variable.
#'                        The expression is evaluated in the context of envir.
#'					      If empty then an initial numeric value of NA is used.
#' 		 $Outcome_type - if specified, indicates this is an outcome variable and indicates 
#' 						its type which is one of "categorical" or "continuous"
#' 		 $Outcome_module - if specified, indicates the Simmmodule this outcome variable belongs to
#'
#' @param envir  
#'  environment to evaluate initial_value in. Typically this will be a
#'  dataframe loaded from a base file. Initial_value can not only
#'  reference values in envir, but also values in enclos.
#'  Defaults to global environment.
#' 
#' @param enclos
#'  Specifies the enclosure, i.e., where R looks for objects not found in envir.
#'  Defaults to the caller's environment (parent.frame())
#'  
#' @param na_omit
#' 
#' If TRUE and an observation is NA, then that case (ie: it's observations for all 
#'  variables) is removed. This however does not apply to initial_value expressions 
#'  that return a singular NA. These remain as a vector containing NA for 
#'  as each observation.
#' 
#' 
#' @return
#'  the simulation frame. The simulation frame contains the variables in simframe_defn with
#'  their inital_value evaluated. Each variable contains a set of observations. Each
#'  observation represents a case in the basefile.
#'  
#'   
#' 	The simulation frame object has the following attributes:
#' 
#' 	"previous" - variables that represent values in the previous iteration
#'  "outcome_vars" - a dataframe of Varname, Outcome_type, and Outcome_module
#'	"na.actions" - an omit variable that indicates the observations that were removed
#' 				because their were NA. Initial_values that returned a singular NA
#' 				remain.
#' 
#' @export
#' @examples 
#' \dontrun{
#'  data_dir <- paste(getwd(), "/data/", sep=""); base_dir <- file.path(data_dir, "base")
#'  people <- read_csv(base_dir, "Base_file_(people).csv")
#'  envir <- people
#'  simframe_defn <- read_csv(base_dir, "simframedef.csv")
#'  simframe.master <- loadSimFrame(simframe_defn, envir)
#' }
loadSimFrame <- function (simframe_defn, envir = .GlobalEnv, enclos = parent.frame(), 
		na_omit=FALSE) {
	
	#remove empty rows
	#(generally these are blank lines at the end of the file)
	simframe_defn <- simframe_defn[simframe_defn$Varname!="", ]
	
	#check for duplicated varnames
	duplicates <- which(duplicated(simframe_defn$Varname))
	if(length(duplicates) > 1) {
		stop(paste("Simframe varname duplicate:", simframe_defn$Varname[duplicates], "\n"))
	}
	
	initial_value_exprs <- structure(simframe_defn$Initial_value, 
			names = simframe_defn$Varname)
	
	empty_exprs <- (initial_value_exprs == "")
	initial_value_exprs[empty_exprs] <- NA_real_
	
	initial_values  <- eval.list(initial_value_exprs, envir, enclos)
	initial_value_is_NA <- is.na(initial_values)
	
	# convert non NA values to data.frame
	# this repeats any inital values that are singular
	simframe <- data.frame(initial_values[!initial_value_is_NA])
	
	nas <- NULL
	if (na_omit) {
		#remove obs. that have NAs in one of their values
		simframe <- na.omit(simframe)
		nas <- attr(simframe, "na.action")	
	}
	
	if (dim(simframe)[1] == 0) {
		stop ("Simframe is empty. Please check for NAs in simframe variables.")
	}
	
	# add singular NA values back to data.frame
	if (any(initial_value_is_NA)) {
		simframe  <- cbind(simframe, 
				as.list(initial_values[initial_value_is_NA]), stringsAsFactors=FALSE)
	}
	
	# previous = the names of the variables that represent values in the previous iteration
	# names(previous) = the source value for the previous variable
	previous <- as.character(simframe_defn$Previous_var)
	names(previous) <- simframe_defn$Varname
	previous <- stripEmpty(previous)
	
	#check previous values exist in varname
	nonexistent_previous <- !(previous %in% simframe_defn$Varname)
	if (any(nonexistent_previous)) {
		stop(gettextf("previous variable(s) %s: not in simframe", 
						paste(previous[nonexistent_previous],collapse=", ")))
	}
	
	#data frame of outcome var mappings, types and set
	outcome_vars <- with(simframe_defn, simframe_defn[Outcome_module != "", 
					c("Varname", "Outcome_type", "Outcome_module")])
	
	#return
	structure(simframe, previous=previous,
			outcome_vars = outcome_vars,
			na.actions=nas)
}

