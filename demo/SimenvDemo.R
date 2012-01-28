# TODO: Add comment
# 
# Author: oman002
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
	#' . <- SimenvMELC
	#' env.scenario <- SimenvDemo$new(name = "My Scenario")
	new <- function(., name=NULL, simframe=simframe.master, dict=dict_demo) {
		
		cat.adjustments <- 	createEmptycat.adjustments(simframe, dict)
		
		siml <- .super$new(.,
				name = name, 
				simframe = simframe,
				dict = dict,
				cat.adjustments = cat.adjustments)
		
		siml$modules <- list(main = SimmoduleMain$new(simframe))
		
		siml
		
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
	#' cat.adjustments <- createEmptycat.adjustments(simframe, dict)
	createEmptycat.adjustments <- function(simframe, dict) {
		
		catvars <- getOutcomeVars(simframe, "categorical")
		
		createAdjustmentMatrices(catvars, dict, 100)
	}
	
})


