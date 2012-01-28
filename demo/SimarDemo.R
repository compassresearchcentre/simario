# A simple demonstration model showing the features of simar.
# This demo simulates the disability state of a population of males and females. 
# A disability state is either no disability, or mild, moderate or severe disability. 
# The disability state of each individual influences their earning capacity, which is the key output of the model.


#if devtools installed we are in a development environment so load simar using load_all
if (length(find.package("devtools", quiet = T)) > 0) {
	library(devtools)
	load_all("simar", reset = T)
} else {
	library(simar)
}

rmall(exceptions=".DEBUG")
setwd("D:/workspace.sim/simar/demo/")
source("SimenvDemo.R")
source("SimmoduleMain.R")

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
#' Perform base simulation.
#' 
#' @examples
#' data_dir <- getwd()
#' data_dir <- "D:/workspace.sim/simar/demo/"
#' initDemo(data_dir)
initDemo <- function(data_dir=getwd()) {
	DICT_FILENAME <- "Data dictionary.xlsx"
	dict_demo <<- Dictionary$new_from_XLS(data_dir, DICT_FILENAME, DICT_FILENAME)
	
	#load initial basefile
	people <<- readXLSSheet1(data_dir, "Base file (people).xlsx")
	
	#create simframe
	sfdef <- readXLSSheet1(data_dir, "simframedef.xlsx")
	simframe.master <<- loadSimFrame(people, sfdef)
	
	people_sets <<- createSets(people, dict_demo$codings)
	
	probs <<- list()
	probs$disability_transition <<- read_csv(data_dir, "Disability state transition probabilities.csv", stringsAsFactors=T)
	probs$disability_transition$index <<- with(probs$disability_transition, 
			index_sex_age_grp_disability_state(Sex, Agegrp, Current.disability.state))
	
	probs$death <<- read_csv(data_dir, "Disability state transition probabilities.csv")
	
	cat ("Demo initialised")
	
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
index_sex_age_grp_disability_state <- function(sex, age_grp, disability_state) {
	as.integer(sex) * 100 + age_grp * 10 + disability_state
}

initDemo()

#if no base simulation yet, then simulate 
if (!exists("env.base")) {
	
	# set same seed to begin with
	set.seed(1) 
	
	env.base <- SimenvDemo$new("Base", simframe.master)
	
	print(env.base$simulate(total_runs=2))
}


