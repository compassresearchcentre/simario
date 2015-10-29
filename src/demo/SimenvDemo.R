# SimenvDemo object for the demo simulation.
# 
# Author: Oliver Mannion
###############################################################################

SimenvDemo <- proto(. = Simenv, expr = {
			
	#' Creates a new Demo simulation environment object.
	#' Call after initialisation.
	#' 
	#' @param .
	#'  receiving object.
	#' @param name
	#'  environment name
	#' @param simframe
	#'  simframe. Stored in the environment
	#' @param dict
	#'  the specific project dictionary
	#' 
	#' @return 
	#'  NULL
	#' 
	#' @export
	#' @examples
	#' . <- SimenvDemo
	#' env.scenario <- SimenvDemo$new(name = "My Scenario")
	new <- function(., name=NULL, simframe=simframe.master, dict=dict_demo) {
		
		NUM_ITERATIONS <- 100
		cat.adjustments <- createEmptyCatAdjustments(simframe, dict, 
				numiterations=NUM_ITERATIONS)
		
		modules <- list(demo = SimmoduleDemo$new(simframe))
		
		.super$new(.,
				name = name, 
				simframe = simframe,
				dict = dict,
				cat.adjustments = cat.adjustments,
				modules = modules)
	}
	
	
	#' Create base tables that include the statistics of input variables that don't change.
	#' In demo, it only include sex.
	#' 
	#' @param .
	#'  receiving object.
	#' @param simframe
	#'  simframe. Stored in the environment
	#' 
	#' @return 
	#'  a list of tables for variables that don't change
	#' 
	#' @export
	#' @examples
	#' generatePreSimulationStats()
	generatePreSimulationStats <- function (., simframe) {
		codings <- .$dict$codings
		
		tbls <- list()
		
		# cat vars at birth
		#NB: we transpose to turn the tables into matrices so they
		#are displayed properly
		tbls$sex <- t(table.catvar(simframe$sex, codings$sex))
		tbls
	}
	
	
	#' Create empty categorical variable adjustment matrices. 
	#' If it is a continuous variable, then convert it to categorical variable
	#' according to binbreaks.
	#' 
	#' @param simframe
	#'  simframe. Stored in the environment
	#' @param dict
	#'  the specific project dictionary
	#' @param numiterations
	#'  number of iterations, which is the number of years that simulated.
	#'  In demo, the number of iterations is 100.
	#' 
	#' @return
	#'  A list of matrices for categorical variable adjustments.
	#' 
	#' @export
	#' @examples
	#' simframe <- simframe.master
	#' dict <- dict_demo
	#' cat.adjustments <- createEmptyCatAdjustments(simframe, dict)
	createEmptyCatAdjustments <- function(simframe, dict, numiterations = NUM_ITERATIONS) {
		
		catvars <- getOutcomeVars(simframe, "categorical")
		
		cat.adjustments <- createAdjustmentMatrices(catvars, dict, numiterations)
		
		#create continuous variable cat.adjustments
		#cat.adjustments$IQ <- createAdjustmentMatrix("IQ", binbreaks$IQ[-1], numiterations, is_a_level_var=F, cont.binbreaks=binbreaks$IQ, catToContModels=catToContModels$IQ)
		cat.adjustments$IQ <- createAdjustmentMatrix("IQ", rows=numiterations, is_a_level_var=F, 
				cont.binbreaks=binbreaks$IQ, catToContModels=catToContModels$IQ)
		#is_a_level_var is not strictly needed - I tried it without it and the default works to correctly identify whether this is a categorical or continuous variable
		
		cat.adjustments$earnings <- createAdjustmentMatrix("earnings", binbreaks$earnings[-1], 
				numiterations, is_a_level_var=F, cont.binbreaks=binbreaks$earnings)
		
		cat.adjustments 
	}
	
})


