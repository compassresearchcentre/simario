# TODO: Add comment
# 
# Author: oman002
###############################################################################


#' runScenario1()
runScenario1 <- function() {
	env.scenario <<- SimenvDemo$new("Scenario 1")
	
	#this is a work around to make the modifyprops function work
	#modifyprops needs one of each category to work
	env.scenario$simframe$disability_state <- c(2,3,4,rep(1, 997))
	
	# test changes at the beginning of the simulation
	env.scenario$cat.adjustments$disability_state[1,] <- c(0.1,0.1,0.6,0.2)
	
	# test a change during the simulation
	env.scenario$cat.adjustments$disability_state[50,] <- c(0.2,0.2,0.3,0.3)
	
	#with(Simenv, debug(applyCatAdjustmentToSimframe))
	#env.base$applyAllCatAdjustmentsToSimframe(1, propensities)
	env.scenario$simulate(2)
	
	# output
	cat("Disability state: year 1\n")
	print(stripMeta(env.scenario$modules$demo$run_results_collated$freqs$disability_state[1,]))
	cat("Disability state: year 50\n")
	print(stripMeta(env.scenario$modules$demo$run_results_collated$freqs$disability_state[50,]))
	
	#print(env.scenario$presim.stats$SESBTH)
	
	#prop.table(table(env.scenario$simframe$disability_state))
	
}
