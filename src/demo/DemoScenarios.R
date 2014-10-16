# Scenario testing example for the demo simulation.
# 
# Author: Oliver Mannion 
###############################################################################

#' scenario on categorical variable
#' runScenario1()
runScenario1 <- function() {
	env.scenario <<- SimenvDemo$new("Scenario 1")
	
	#this is a work around to make the modifyprops function work
	#modifyprops needs one of each category to work
	#env.scenario$simframe$disability_state <- c(2,3,4,rep(1, 997))
	#env.scenario$simframe$disability_state <- rep(1, 1000)
	
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
	subgroupExpression <- "sex==2"
	setGlobalSubgroupFilterExpression(subgroupExpression)
	env.scenario$cat.adjustments$disability_state[1,] <- c(0.1,0.1,0.6,0.2)
	env.scenario$cat.adjustments$disability_state[50,] <- c(0.2,0.2,0.3,0.3)
	env.scenario$simulate(2)
	
	### test if the adjustment work
	#test1<-env.scenario$modules$demo$outcomes$disability_state[,1]
	#test2<-env.scenario$modules$demo$outcomes$disability_state[,50]
	#table(test1[env.scenario$simframe$sex==2])
	#table(test2[env.scenario$simframe$sex==2])
}

#' subgroup scenario (multiple expression)
 runScenario3 <- function() {
	env.scenario <<- SimenvDemo$new("Scenario 3")	
	subgroupExpression <- "sex==2 & qualification==2"
	setGlobalSubgroupFilterExpression(subgroupExpression)
	env.scenario$cat.adjustments$disability_state[1,] <- c(0.1,0.1,0.6,0.2)
	env.scenario$cat.adjustments$disability_state[70,] <- c(0.2,0.2,0.3,0.3)
	
	env.scenario$simulate(2)
	
	### test if the adjustment work
	# env.scenario$modules$demo$run_results_collated$freqs_by_subgroup$disability_state[1,]
	# env.scenario$modules$demo$run_results_collated$freqs_by_subgroup$disability_state[70,]

}

#' subgroup scenario for time-variant variable
runScenario4 <- function() {
	env.scenario <<- SimenvDemo$new("Scenario 4")
	#env.scenario$simframe$disability_state <- c(2,rep(1, 999))
	subgroupExpression <- "disability_state==1 & earnings>10000"
	setGlobalSubgroupFilterExpression(subgroupExpression)
	env.scenario$cat.adjustments$IQ[1,] <- rep(1/5, 5)
	env.scenario$cat.adjustments$IQ[50,] <- c(0.1,0.1,0.5,0.2,0.1)
	env.scenario$simulate(2)
	
	### test if the adjustment work
	#env.scenario$modules$demo$run_results_collated$freqs_continuousGrouped_by_subgroup$IQ[1,]
	#env.scenario$modules$demo$run_results_collated$freqs_continuousGrouped_by_subgroup$IQ[50,]
}


#' scenario on continuous variable
runScenario5 <- function() {
	env.scenario <<- SimenvDemo$new("Scenario 5")
	env.scenario$cat.adjustments$IQ[1,] <- rep(1/5, 5)
	env.scenario$cat.adjustments$IQ[50,] <- c(0.1,0.1,0.5,0.2,0.1)
	env.scenario$simulate(2)
	
	### test if the adjustment work
	#env.scenario$modules$demo$run_results$run1$freqs_continuousGrouped$IQ$`1`
	#env.scenario$modules$demo$run_results$run1$freqs_continuousGrouped$IQ$`50`
}


#' scenario on subgroup of continuous variable
runScenario6 <- function() {
	env.scenario <<- SimenvDemo$new("Scenario 6")
	subgroupExpression <- "sex==2"
	setGlobalSubgroupFilterExpression(subgroupExpression)
	env.scenario$cat.adjustments$IQ[1,] <- rep(1/5, 5)
	env.scenario$cat.adjustments$IQ[50,] <- c(0.1,0.1,0.5,0.2,0.1)
	env.scenario$simulate(1)
	
	### test if the adjustment work
	#env.scenario$modules$demo$run_results_collated$freqs_continuousGrouped_by_subgroup$IQ[1,]
	#env.scenario$modules$demo$run_results_collated$freqs_continuousGrouped_by_subgroup$IQ[50,]

}


#' scenario on subgroup of categorical variable
runScenario7 <- function() {
	env.scenario <<- SimenvDemo$new("Scenario 7")
	subgroupExpression <- "IQ < 100 & alive==TRUE"
	setGlobalSubgroupFilterExpression(subgroupExpression)
	env.scenario$cat.adjustments$disability_state[1,] <- rep(1/4, 4)
	env.scenario$cat.adjustments$disability_state[50,] <- c(0.1,0.1,0.6,0.2)
	env.scenario$simulate(2)
	
	### test if the adjustment work
	#env.scenario$modules$demo$run_results_collated$freqs_by_subgroup$disability_state[1,]
	#env.scenario$modules$demo$run_results_collated$freqs_by_subgroup$disability_state[50,]

}


runScenario8 <- function() {
	env.scenario <<- SimenvDemo$new("Scenario 8")
	subgroupExpression <- "sex==2"
	setGlobalSubgroupFilterExpression(subgroupExpression)
	#env.scenario$cat.adjustments$qualification[1,] <- rep(1/4, 4)
	env.scenario$cat.adjustments$qualification[50,] <- c(0.1,0.1,0.6,0.2)
	env.scenario$simulate(2)
	
	### test if the adjustment work
	#table(env.scenario$modules$demo$outcomes$qualification[,1])
	#table(env.scenario$modules$demo$outcomes$qualification[,50])
	
	#env.scenario$modules$demo$run_results_collated$freqs_by_subgroup$qualification[1,]
	#env.scenario$modules$demo$run_results_collated$freqs_by_subgroup$qualification[50,]
}


