# A simple demonstration model showing the features of simar.
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
	
	if ("package:simar" %in% search()) detach("package:simar", unload = T)
	
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
#' data_dir <- getwd()
#' data_dir <- paste(getwd(), "/data/", sep="")
#' initDemo(data_dir)
initDemo <- function(data_dir=paste(getwd(), "/data/", sep="")) {
	base_dir <- file.path(data_dir, "base")
	
	descriptions_dataframe <- read_file(base_dir, "Data dictionary.csv")
	codings_dataframe <- descriptions_dataframe
	dict_demo <<- Dictionary$new(descriptions_dataframe, codings_dataframe)
	
	#load initial basefile
	people <<- read_csv(base_dir, "Base file (people).csv")
	
	#create simframe
	sfdef <- read_csv(base_dir, "simframedef.csv")
	simframe.master <<- loadSimFrame(sfdef, people)
	
	people_sets <<- createSets(people, dict_demo$codings)
	
	transition_probabilities_dir <- file.path(data_dir, "transition_probabilities")
	transition_probabilities <<- loadTransitionProbabilities(transition_probabilities_dir)
	
	earnings_scale <<- loadEarningsScale(data_dir)
	
	breaks_age_grp <<- 	c(-1, 59, 79, 99)
	names(breaks_age_grp) <<- c(NA, names(dict_demo$codings$age_grp)) 
	
	cat ("Demo initialised\n")
	
}

#' dir <- data_dir
loadEarningsScale <- function(dir) {
	earnings_scale_dataframe <- read_csv(dir, "Annual earnings scale by disability status.csv")
	earnings_scale <- structure(earnings_scale_dataframe$Earnings, .Names=earnings_scale_dataframe$Disability.state)
	earnings_scale
}

loadTransitionProbabilities <- function(dir) {
	transition_probabilities <- list()
	
	transition_probabilities$disability_state <- read_csv(dir, "Disability state transition probabilities.csv", stringsAsFactors=T)
	transition_probabilities$disability_state$index <- with(transition_probabilities$disability_state, 
			index_sex_age_grp_disability_state(Sex, Agegrp, Current.disability.state))
	transition_probabilities$disability_state$probs <- 
			as.matrix(transition_probabilities$disability_state[c("No.Disability", "Mild.Disability", "Moderate.Disability", "Severe.Disability")])
	
	transition_probabilities$death <- read_csv(dir, "Probabilities of male and female death by age and sex.csv")
	
	transition_probabilities
}

loadSimar <- function() {
	.devtools_installed <- length(find.package("devtools", quiet = T)) > 0
	if (.devtools_installed & !exists(".USELIB")) {
		cat("loadSimar: loading pre-installed development version using load_all\n")
		
		library(devtools)
		load_all("simar", reset = T)
		
		#workaround for devtools issue https://github.com/hadley/devtools/issues/38
		Simenv$.super <- .GlobalEnv
		Simmodule$.super <- .GlobalEnv
	} else {
		cat("loadSimar: loading installed library")
		library(simar)
	}
}

#setwd(file.path(Sys.getenv("R_USER"), "simar/src/demo/"))
loadSimar()
source("SimenvDemo.R")
source("SimmoduleDemo.R")

initDemo()

#if no base simulation yet, then simulate 
if (!exists("env.base")) {
	
	# set same seed to begin with
	set.seed(1) 
	
	env.base <- SimenvDemo$new("Base", simframe.master)
	
	print(env.base$simulate(total_runs=2))
}
