# A simple demonstration model showing the features of simario.
# This demo simulates the disability state of a population of males and females. 
# A disability state is either no disability, or mild, moderate or severe disability. 
# The disability state of each individual influences their earning capacity, which is the key output of the model.

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
#' @examples
#' codings <- dict_demo$codings
#' createSets(people, codings) 
createSets <- function(people, codings) {
	
	sets <- list()
	sets$females <- people$sex == codings$sex["Female"] 
	sets$males <- people$sex == codings$sex["Male"]
	attr(sets$females, "desc") <- "females only"
	attr(sets$males, "desc") <- "males only"
	
	sets
}

#' Initialise models, basefile, simframe.
#' 
#' @examples
#' data_dir <- paste(getwd(), "/data/", sep="")
#' initDemo(data_dir)
initDemo <- function(data_dir=paste(getwd(), "/data/", sep="")) {
	base_dir <- file.path(data_dir, "base")
	
	descriptions_dataframe <- read_file(base_dir, "Data_dictionary.csv")
	codings_dataframe <- descriptions_dataframe
	dict_demo <<- Dictionary$new(descriptions_dataframe, codings_dataframe)
	
	#load initial basefile
	people <<- read_csv(base_dir, "Base_file_(people).csv")
	
	#create simframe
	sfdef <- read_csv(base_dir, "simframedef.csv")
	simframe.master <<- loadSimFrame(sfdef, people)
	
	people_sets <<- createSets(people, dict_demo$codings)
	
	transition_probabilities_dir <- file.path(data_dir, "transition_probabilities")
	transition_probabilities <<- loadTransitionProbabilities(transition_probabilities_dir)
	
	earnings_scale <<- loadEarningsScale(data_dir)
	
	breaks_age_grp <<- 	c(-1, 59, 79, 99)
	names(breaks_age_grp) <<- c(NA, names(dict_demo$codings$age_grp)) 
	
	propensities <<- NULL		# referenced by adjustCatVar
	
	cat ("Demo initialised\n")
	
}

#' dir <- data_dir
loadEarningsScale <- function(dir) {
	earnings_scale_dataframe <- read_csv(dir, "Annual_earnings_scale_by_disability_status.csv")
	earnings_scale <- structure(earnings_scale_dataframe$Earnings, .Names=earnings_scale_dataframe$Disability.state)
	earnings_scale
}

loadTransitionProbabilities <- function(dir) {
	transition_probabilities <- list()
	
	transition_probabilities$disability_state <- read_csv(dir, "Disability_state_transition_probabilities.csv", stringsAsFactors=T)
	transition_probabilities$disability_state$index <- with(transition_probabilities$disability_state, 
			index_sex_age_grp_disability_state(Sex, Agegrp, Current.disability.state))
	transition_probabilities$disability_state$probs <- 
			as.matrix(transition_probabilities$disability_state[c("No.Disability", "Mild.Disability", "Moderate.Disability", "Severe.Disability")])
	
	transition_probabilities$death <- read_csv(dir, "Probabilities_of_male_and_female_death_by_age_and_sex.csv")
	
	transition_probabilities
}

loadSimario <- function() {
	not_already_installed <- function(package_names) {
		setdiff(package_names, row.names(installed.packages()))	
	}
	
	.is_dev_environment <- length(find.package("devtools", quiet = T)) > 0 & file.exists(path.expand("~/.Rpackages"))
	if (.is_dev_environment & !exists(".USELIB")) {
		#loads simario from the workspace folder (the R folder - simar/src/R))
		cat("loadSimario: loading pre-installed development version using load_all\n")
		
		if (length(not_already_installed("simario"))) {
			stop("dict_example requires simario library. Please make sure simario has been installed into the R library path. This can be done by running install_simario_library.bat")	
		}
		
		library(devtools)
		load_all("simario", reset = T)
		
		#workaround for devtools issue https://github.com/hadley/devtools/issues/38
		Simenv$.super <- .GlobalEnv
		Simmodule$.super <- .GlobalEnv
	} else {
		#loads simario from a preexisting (on computer) R library folder - like C:/Program Files/R/R-2.14.1/library/simario
		cat("loadSimario: loading installed library\n")
		library(simario)
		cat("simario v", sessionInfo()$otherPkgs$simario$Version, "loaded\n")
	}
}

#setwd(file.path(Sys.getenv("R_USER"), "simario/src/demo/"))
loadSimario()
source("SimenvDemo.R")
source("SimmoduleDemo.R")
source("DemoScenarios.R")

initDemo()

#if no base simulation yet, then simulate 
if (!exists("env.base")) {
	
	# set same seed to begin with
	set.seed(1) 
	
	env.base <- SimenvDemo$new("Base", simframe.master)
	
	print(env.base$simulate(total_runs=2))
}
