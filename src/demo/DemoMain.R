# A simple demonstration model showing the features of simario.
# This demo simulates the disability state, IQ and qualification of a population of males and females. 
# A disability state is either no disability, or mild, moderate or severe disability. 
# A qualification is either None, Secondary School, Below Degree or Degree.
# All of the variables influences the earning capacity, which is the key output of the model.

clearWorkspace <- function() {
	rmall <- function (exceptions = NULL) {
		vars <- ls(".GlobalEnv", all.names=TRUE)
		if (!is.null(exceptions)) {
			vars <- vars[!vars %in% exceptions]
		}
		rm(pos = ".GlobalEnv", list = vars)
	}
	
	rmall(exceptions=c(".DEBUG",".USELIB"))
	
	if ("package:simario" %in% search()) detach("package:simario", unload = T)
	
	while("simframe" %in% search()) detach("simframe")
}

clearWorkspace()

#' Create logical arrays of certain people subsets
#' 
#' @param people
#'  a list of values from base file
#' 
#' @param codings
#'  a list of category names for categorical variables.
#' 
#' @return 
#'  a list of logical vectors for females and males
#' 
#' @export
#' @examples
#' codings <- dict_demo$codings
#' createSets(people, codings) 
## createSets <- function(people, codings) {
	
	##sets <- list()
	##sets$females <- people$sex == codings$sex["Female"] 
	##sets$males <- people$sex == codings$sex["Male"]
	##attr(sets$females, "desc") <- "females only"
	##attr(sets$males, "desc") <- "males only"
	
##	sets
##}






# dir <- data_dir
# loadEarningsScale <- function(dir) {
	# earnings_scale_dataframe <- read_csv(dir, "Annual_earnings_scale_by_disability_status.csv")
	# earnings_scale <- structure(earnings_scale_dataframe$Earnings, .Names=earnings_scale_dataframe$Disability.state)
	# earnings_scale
# }



#copied this new version of the loadSimario function from MELC 
# original is at (H:\COMPASS\MelC\SimarioPaper\OldLoadSimarioFunctionFromDemo) 
loadSimario <- function() {
	.is_dev_environment <- length(find.package("devtools", quiet = T)) > 0
	if (.is_dev_environment & !exists(".USELIB")) {
		cat("loadSimario: loading pre-installed development version using load_all\n")
		
		library(stringr)
		library(devtools)
		library(testthat)	
		library(Hmisc)
		if(installed.packages()["devtools","Version"] >= 0.8) {
			load_all("../../../simario/src", reset = T) 
			
		} else {											
			load_all("simario", reset = T)
		} 
		
		#workaround for devtools issue https://github.com/hadley/devtools/issues/38
		Simenv$.super <- .GlobalEnv
		Simmodule$.super <- .GlobalEnv
	} else {
		cat("loadSimario: loading installed library\n")
		library(simario)
		cat("simario v", sessionInfo()$otherPkgs$simario$Version, "loaded\n")
	}
}



#' List of breaks codings for adjustable continuous variables.  
#' 
#' binbreak names cannot have spaces - because in label_flattened_mx_grping.and.CIs() looks
#' 	at if there are any spaces to determine whether there is subgrouping or not.  An underscore
#' 	can be used instead of a space
#' 
#' @param people
#'  a list of values from base file
#' 
#' @return
#'  a list of binbreaks
#' 
#' @examples
#' createBinBreaks(people)

createBinBreaks <- function(people) {
	binbreaks <- list()
	
	#NB: the very first cut point must be less than min(x)
	#subsequent cut points are the closed right bound,
	#and the final cut point should be max(x)
	# eg: breaks of c(0, 34, 35, 36, 37, 44)
	# will cut into the following bins
	#  (0,34] (34,35] (35,36] (36,37] (37,44] 
	
	binbreaks$earnings <- c(-1, 5000, 10000, 20000, 30000, 50000, 999999999999)
	names(binbreaks$earnings) <- c(NA, "$5000_or_less", "$5001-$10000", "$10001-$20000", "$20001-$30000", "$30001-$50000","$50001_or_more")
	binbreaks$IQ <- c(-1, 49, 69, 85, 129, 999)
	names(binbreaks$IQ) <- c(NA, "0-49", "50-69", "70-85", "86-129", "130+")
	
	
	binbreaks
}


#' Load each model from an xls file, and construct a GLM object from it
#' 
#' @param modelfiledir
#'  directory holding models
#' 
#' @return 
#'  a list of models.
#' 
#' @export
#' @examples
#' modelfiledir <- "C:/Workspace/simario/src/demo/data/models/"
#' models <- loadDemoModels(modelfiledir)
loadDemoModels <- function(modelfiledir) {
	models <- list()
	
	models$IQModel7_15 <- loadGLMCSV(modelfiledir, "IQModel7_15.csv")
	models$IQModel16_27 <- loadGLMCSV(modelfiledir, "IQModel16_27.csv")
	models$IQModel28onwards <- loadGLMCSV(modelfiledir, "IQModel28onwards.csv")
	models$earningsModel <- loadGLMCSV(modelfiledir, "earningsModel.csv")
	
	cat("Loaded models\n")
	models
}



#' Load the transition probabilities.
#' 
#' @param dir
#'  directory holding transition probabilities
#' 
#' @return
#'  a list of transition probabilities
#' 
#' @export
#' @examples
#' dir <- "C:/Workspace/simario/src/demo/data/transition_probabilities/"
#' transition_probabilities <- loadTransitionProbabilities(dir)
loadTransitionProbabilities <- function(dir) {
	transition_probabilities <- list()
	
	transition_probabilities$disability_state <- read_csv(dir, "Disability_state_transition_probabilities.csv", stringsAsFactors=T)
	transition_probabilities$disability_state$index <- with(transition_probabilities$disability_state, 
			index_sex_age_grp_disability_state(Sex, Agegrp, Current.disability.state))
	transition_probabilities$disability_state$probs <- 
			as.matrix(transition_probabilities$disability_state[c("No.Disability", "Mild.Disability", "Moderate.Disability", "Severe.Disability")])
	
	transition_probabilities$death <- read_csv(dir, "Probabilities_of_male_and_female_death_by_age_and_sex.csv")
	
	transition_probabilities$qualification <- read_csv(dir, "QualTransitionProbs.csv", stringsAsFactors=T)
	transition_probabilities$qualification$index <- with(transition_probabilities$qualification, 
			index_age_qualification(age, Current.qualification))
	transition_probabilities$qualification$probs <- 
			as.matrix(transition_probabilities$qualification[c("None", "Secondary.School", "Below.Degree", "Degree")])
	
	transition_probabilities
}



#' Loads the propensity models (models used for deciding who to change in a scenario).
#' The function uses loadGLMCSV to load excel files and returns the object PropensityModels which
#' is a list with each element being a list of propensity models (glm objects) for each variable.
#' 
#' @param modelfiledir
#'  directory holding propensity models 
#' 
#' @return 
#'  a list of propensity models.
#' 
#' @export
#' @examples
#' modelfiledir <- "C:/Workspace/simario/src/demo/data/models_Propensities/"
#' PropensityModels <- loadPropensityModels(modelfiledir) 
loadPropensityModels <- function(modelfiledir) {
	PropensityModels <- list()
	
	PropensityModels$qualification <- list()
	
	PropensityModels$qualification <- list(loadGLMCSV(modelfiledir, "qualProb1.csv"),
			loadGLMCSV(modelfiledir, "qualProb2.csv"), loadGLMCSV(modelfiledir, "qualProb3.csv"))
	
	cat("Loaded Propensity models\n")
	PropensityModels
}



#' Calculate propensities for use in scenario testing from propensity models
#' Only used in generating the propensity arrays that are used for any presim or year 1 scenarios
#' Gets the propensity models from the PropensityModels list generated by the 
#' loadPropensityModels() function.
#' 
#' @param propensityfiledir
#'  directory holding propensities
#' 
#' @param stochastic
#'  If TRUE adds random variation around the probabilities to be in each 
#'  category.  If  TRUE it will cause the probabilities to no longer add to 1 and so should only 
#'  be used when the probabilities are being used as propensities in scenario testing.
#'  Passed into function predictOrdinal()
#' 
#' @return 
#'  a list of propensities.
#' 
#' @export
#' @examples
#' propensityfiledir <- "C:/Workspace/simario/src/demo/data/models_Propensities/"
#' propens.list <- loadDemoPropensities(propensityfiledir, stochastic=TRUE)
loadDemoPropensities <- function(propensityfiledir, stochastic=FALSE) {
	
	num.people <- length(simframe.master[[1]])
	
	#generate msmoke propensities for scenario testing
	qualificationmodels <- propensityModels[["qualification"]]
	
	#set-up empty arrays for each variable, rows=children, columns=number of models for the variable,
	qualificationPropensities <- array(dim=c(num.people, length(qualificationmodels), 1))
	
	#calculate propensities/probabilities from the propensity models
	#then fill the above arrays with the propensities
	qualificationPropensitiesMatrix <- predictOrdinal(qualificationmodels, num.people, envir=simframe.master, stochastic=stochastic)
	qualificationPropensities[,,1] <- qualificationPropensitiesMatrix[,-ncol(qualificationPropensitiesMatrix)]
	
	#combine all the individual arrays into a list
	propens.list <- list()
	
	propens.list$qualification <- qualificationPropensities

	propens.list
}



#' Load models for mapping from categorical to continuous (in sceanrio testing of continuous 
#' variables) 
#' Load each model from an xls file, and construct a GLM object from it
#' 
#' @param modelfiledir
#'  directory holding models 
#' 
#' @return 
#'  a list of models.
#' 
#' @export
#' @examples
#' modelfiledir <- "C:/Workspace/simario/src/demo/data/models_CatToCont/"
#' catToContModels <- loadCatToContModels(modelfiledir)
loadCatToContModels <- function(modelfiledir) {
	catToContModels <- list()
	
	catToContModels$IQ <- list()
	
	catToContModels$IQ <- list(loadGLMCSV(modelfiledir, "IQCatToCont0_49.csv"),
			loadGLMCSV(modelfiledir, "IQCatToCont50_69.csv"), loadGLMCSV(modelfiledir, "IQCatToCont70_85.csv"),
			loadGLMCSV(modelfiledir, "IQCatToCont86_129.csv"), loadGLMCSV(modelfiledir, "IQCatToCont130Plus.csv"))
	
	cat("Loaded CatToCont models\n")
	catToContModels
}



#' Initialise models, basefile, simframe.
#' 
#' @param data_dir
#'  directory holding data
#' 
#' @param modelfiledir
#'  directory holding models
#' 
#' @param propensityfiledir
#'  directory holding propensity models
#' 
#' @param catToCont.modelfiledir
#'  directory holding models for mapping from categorical to continuous
#' 
#' @return 
#'  NULL. It creates objects in workspace directly.
#' 
#' @export
#' @examples
#' data_dir <- paste(getwd(), "/data/", sep="")
#' modelfiledir <- "C:/Workspace/simario/src/demo/data/models/"
#' propensityfiledir <- "C:/Workspace/simario/src/demo/data/models_Propensities/"
#' catToCont.modelfiledir <- "C:/Workspace/simario/src/demo/data/models_CatToCont/"
#' initDemo(data_dir, modelfiledir, propensityfiledir, catToCont.modelfiledir)

initDemo <- function(data_dir=paste(getwd(), "/data/", sep=""), modelfiledir, propensityfiledir, catToCont.modelfiledir) {
	base_dir <- file.path(data_dir, "base")
	
	descriptions_dataframe <- read_file(base_dir, "Data_dictionary.csv")
	codings_dataframe <- descriptions_dataframe
	dict_demo <<- Dictionary$new(descriptions_dataframe, codings_dataframe)
	
	#load initial basefile
	people <<- read_csv(base_dir, "Base_file_(people).csv")
	
	#create simframer
	sfdef <- read_csv(base_dir, "simframedef.csv")
	simframe.master <<- loadSimFrame(sfdef, people)
	##people_sets <<- createSets(people, dict_demo$codings)
	
	transition_probabilities_dir <- file.path(data_dir, "transition_probabilities")
	transition_probabilities <<- loadTransitionProbabilities(transition_probabilities_dir)
	
	#earnings_scale <<- loadEarningsScale(data_dir)
	
	breaks_age_grp <<- c(-1, 59, 79, 99)
	names(breaks_age_grp) <<- c(NA, names(dict_demo$codings$age_grp)) 
	
	#load models
	models <<- loadDemoModels(modelfiledir)
	checkModelVars(models, simframe.master)
	
	#load catToCont models
	catToContModels <<- loadCatToContModels(catToCont.modelfiledir)
	lapply(catToContModels, checkModelVars, simframe=simframe.master)
	
	#Load propensity models
	propensityModels <<- loadPropensityModels(propensityfiledir)
	lapply(propensityModels, checkModelVars, simframe=simframe.master)
	
	###load propensities (calculates them from the propensity models)
	propensities <<- loadDemoPropensities(propensityfiledir, stochastic=TRUE)
	
	
	#load aux
	binbreaks <<- createBinBreaks(people)
	
	cat ("Demo initialised\n")
	
}



#setwd(file.path(Sys.getenv("R_USER"), "simario/src/demo/"))

loadSimario()

#load functions in other files
source("SimenvDemo.R")
source("SimmoduleDemo.R")
source("DemoScenarios.R")
source("Table Builder.R")

#define directories
dirs <- list()
dirs$root <- paste(getwd(),"/",sep="")
dirs$base <- paste(dirs$root ,"base/",sep="")
dirs$models <- paste(dirs$root,"data/models/",sep="")
dirs$catToContModels <- paste(dirs$root, "data/models_CatToCont/", sep="")
dirs$PropensityModels <- paste(dirs$root, "data/models_Propensities/", sep="")

initDemo(modelfiledir=dirs$models, propensityfiledir=dirs$PropensityModels, catToCont.modelfiledir=dirs$catToContModels)

#if no base simulation yet, then simulate 
if (!exists("env.base")) {
	
	# set same seed to begin with
	set.seed(1) 
	
	env.base <- SimenvDemo$new("Base", simframe.master)
	
	print(env.base$simulate(total_runs=2))
}



