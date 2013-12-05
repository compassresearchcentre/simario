# Scenario testing example for the demo simulation.
# 
# Author: Oliver Mannion 
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

#' subgroup scenario (single expression)
runScenario2 <- function() {
	env.scenario <<- SimenvDemo$new("Scenario 2")
	# test a change on female only
	subgroupExpression <- "sex=='F'"
	setGlobalSubgroupFilterExpression(subgroupExpression)
	env.scenario$simframe$disability_state <- c(2,3,4,rep(1, 497),2,3,4,rep(1,497))
	env.scenario$cat.adjustments$disability_state[1,] <- c(0.1,0.1,0.6,0.2)
	env.scenario$cat.adjustments$disability_state[50,] <- c(0.2,0.2,0.3,0.3)
	env.scenario$simulate(2)
	
	### test if the adjustment work
	#test<-env.scenario$modules$demo$outcomes$disability_state[,1]
	#test<-env.scenario$modules$demo$outcomes$disability_state[,50]
	#table(test[env.scenario$simframe$sex=='F'])
}

#' subgroup scenario (multiple expression)
runScenario3 <- function() {
	env.scenario <<- SimenvDemo$new("Scenario 3")
	subgroupExpression <- "sex=='F' & age_grp==1"
	setGlobalSubgroupFilterExpression(subgroupExpression)
	env.scenario$simframe$disability_state <- c(2,3,4,rep(1, 497),2,3,4,rep(1,497))
	env.scenario$cat.adjustments$disability_state[59,] <- c(0.1,0.1,0.6,0.2)
	env.scenario$simulate(2)
	
	### test if the adjustment work
	#test1<-env.scenario$modules$demo$outcomes$disability_state[,59]
	#test2<-test1[env.scenario$simframe$sex=='F']
	#table(test2[env.scenario$modules$demo$outcomes$age_grp[,59]==1])
}

#' subgroup scenario for time-variant variable
runScenario4 <- function() {
	env.scenario <<- SimenvDemo$new("Scenario 4")
	subgroupExpression <- "age_grp==2"
	setGlobalSubgroupFilterExpression(subgroupExpression)
	env.scenario$simframe$disability_state <- c(2,3,4,rep(1, 497),2,3,4,rep(1,497))
	env.scenario$cat.adjustments$disability_state[60,] <- c(0.2,0.2,0.3,0.3)
	env.scenario$simulate(2)
	
	### test if the adjustment work
	#test<-env.scenario$modules$demo$outcomes$disability_state[,60]
	#table(test[env.scenario$modules$demo$outcomes$age_grp[,60]==2])
}

