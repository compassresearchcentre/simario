# SimenvDemo object for the demo simulation.
# 
# Author: Oliver Mannion
###############################################################################

SimenvDemo <- proto(. = Simenv, expr = {
			
	#' Creates a new Demo simulation environment object.
	#' Call after initialisation.
	#' 
	#' @param name
	#'  environment name
	#' @param simframe
	#'  simframe. Stored in the environment
	#' 
	#' @examples
	#' 
	#' . <- SimenvDemo
	#' env.scenario <- SimenvDemo$new(name = "My Scenario")
	new <- function(., name=NULL, simframe=simframe.master, dict=dict_demo) {
		
		cat.adjustments <- createEmptyCatAdjustments(simframe, dict)
		
		modules <- list(demo = SimmoduleDemo$new(simframe))
		
		.super$new(.,
				name = name, 
				simframe = simframe,
				dict = dict,
				cat.adjustments = cat.adjustments,
				modules = modules)
	}
	
	#' Create base tables.
	#'
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
	#' 
	#' @examples
	#' simframe <- simframe.master
	#' dict <- dict_demo
	#' cat.adjustments <- createEmptyCatAdjustments(simframe, dict)
	createEmptyCatAdjustments <- function(simframe, dict) {
		
		catvars <- getOutcomeVars(simframe, "categorical")
		
		NUM_ITERATIONS <- 100
		
		createAdjustmentMatrices(catvars, dict, NUM_ITERATIONS)
	}
	
})


