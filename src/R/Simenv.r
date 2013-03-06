library(proto)

#' Simenv object - a simulation environment.
#'
#' A simulation environment contains everything required to perform a simulation. Typically 1 Simenv will be created 
#' and used to run a base simulation, and additional Simenvs will be created to test different scenarios.
#'
#' A Simenv consists of a:
#' - a simframe (possibly with adjustments to test a scenario)  
#' - one or more simulation modules (Simmodule). A Simmodule contains outcomes, run stats, and runs.averaged for a simulation
#'   as well as the code to generate them.
#' 
#' Uses the global environment list variable "propensities" when performing categorical adjustment  
#' 
#' This class will be subclassed by specific simulation problems which will provide their own simframe,
#' Simmodules and adjustments.  
#'  
#' @export 
Simenv <- proto(
expr = {  

	#' Create new simenv object
	#' 
	#' @param name
	#'  simulation name
	#' 
	#' @param simframe
	#'  simframe
	#' 
	#' @param dict
	#'  a Dictionary object
	#' 
	#' @param cat.adjustments
	#' Categorical variable adjustment matrices.
	#' 
	#' Each element is an adjustment matrix:
	#' 
	#'            Non-smoker (%) Smoker (%)
	#'     Year 1             NA         NA
	#'     Year 2             NA         NA
	#' attr(,"varnames")
	#' [1] "z1msmokeLvl0" "z1msmokeLvl1"
	#' 
	#' The values in the first row are used to make adjustments before the simulation begins.
	#' Values in subsequent rows can be used during the simulation to set the required proportion
	#' during the specified iteration (eg: iteration 2 if a value is specified in Year 2).
	#' The variables in the simframe to adjust are specified by the varnames attribute.
	#' 
	#' @param cont.adjustments
	#' A list of time-variant continuous variable adjustment matrices.
	#' 
	#' Each element is an adjustment matrix with number of rows equal to the number of micro units
	#' amd number of columns equal to the number of iterations plus 1 (for the presimulation adjustments).
	#' 
	#' The user specifies from the user interface desired increments (or decrements) for all micro units
	#' in particular categories (e.g. decrease the number of cigarettes smoked per day by 20 for every 
	#' child with a mother who smokes 40 or more cigarettes a day), these adjustments are made to the 
	#' simulated data from the base simulation and results stored in these matrices.  At each year in the 
	#' simulation these cont.adjustment matrices are checked and, if they contain values, they are used
	#' instead of the simulated values at that year. 
	#' 
	#' @param modules
	#'  the list of Simmodules for this Simenv
	#' 
	#' @examples
	#' env <- Simenv$new(name = "Base", simframe=simframe.master, dict=dict_demo)
	new <- function (., name, simframe, dict, cat.adjustments=list(), modules=list()) {
		proto(.,
				name=name,
				num_runs_simulated <- 0L,
				simframe=simframe,
				presim.stats=list(),
				cat.adjustments=cat.adjustments,
				modules=modules,
				fixed.outcomes=list(),
				dict=dict
		)
	}
	
	clone <- function(.) as.proto(.$as.list(all.names=TRUE))
	
	class <- function(.) "Simenv"
	
	#' Apply categorical adjustments to simframe.
	#' 
	#' @param .
	#'  simenv receiving object. .$simframe is modified.  
	#' @param iteration
	#' iteration number - corresponds to a row number in the matrix elements of the cat.adjustments list
	#' @param propensities
	#' 		named list of propensities for the cat.adjustments
	#' @param printAdj
	#' 		if TRUE will print new proportions of modified simframe vars
	#' @param cat.adjustments
	#' 	a list of categorical adjustment matrices whose rows each correpond to desired adjustments for an iteration.
	#' 	Each matrix has a 'varname' attribute, indicating which variable in the simenv object is to be adjusted.
	#' 	Each matrix may also have a 'logisetexpr' attribute - if so, this is evaluated and becomes a logical vector indicating which observations
	#'  of the 'varname' variable to adjust (i.e. the "logisetexpr" attribute gives which subset of the data the row of adjustments are intended for).
	#'
	#' @return 
	#'  NULL. simframe in receiving object is modified directly.
	#'   
	#' @examples
	#'  . <- env.scenario
	#' 	iteration = 1 ; print_adj = TRUE
	#' 
	#' 	.$cat.adjustments$z1accomLvl1[1,] <- c(0.5,0.5)
	#'  .$cat.adjustments$SESBTH[1,] <- c(0.1,0.1,0.8)
	#'  .$cat.adjustments$catpregsmk2[1,] <- c(0.01,0.02,0.03,0.04,0.90)
	#' .$cat.adjustments$INTERACT[1,] <- c(0, 0, 0, 0, 0, 0, 0, 0, 1)
	#' 
	#'  print(prop.table(table(.$simframe$z1accomLvl1)),digits=3)
	#'  print(prop.table(table(binary.levels.combine(.$simframe$SESBTHLvl1 , .$simframe$SESBTHLvl2, .$simframe$SESBTHLvl3))),digits=3)
	#'  print(prop.table(table(.$simframe$catpregsmk2)),digits=3)	
	#' 
	#' .$applyAllCatAdjustmentsToSimframe(iteration, propensities, print_adj)
	#'
	#'	
	#'
	#'
	#'	. <- env.scenario
	#'	propensities<-list(examplevariable=array(c(0.9,0.8,0.8,0.7,0.5,0.4,0.1,0.1,0.15,0.7,0.9,0.9,0.9) , dim=c(13,1,1)),var2=array(c(0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.25,0.2,0.2,0.2,0.2) , dim=c(13,1,1)))
	#'	iteration=1
	#' 	.$simframe<-.$simframe[1:13,]
	#'  .$simframe$examplevariable<-c(1,1,1,1,0,0,0,0,0,1,1,1,1)
	#' 	.$cat.adjustments$examplevariable <-matrix(rep(NA,10),ncol=2)
	#' 	.$cat.adjustments$examplevariable[1,] <- c(0.5,0.5)
	#' 	colnames(.$cat.adjustments$examplevariable)<-c("example variable =0 (%)","example variable =1 (%)")
	#' 	.$cat.adjustments$examplevariable<-structure(.$cat.adjustments$examplevariable, varnames="examplevariable")
	#' 	.$cat.adjustments$examplevariable<-structure(.$cat.adjustments$examplevariable, logisetexpr="residential")
	#' 	.$simframe$residential<-c(0, 0,  1,  1, 0,  1,  1,  1,  1,  1,  1,  1, 1)
	#'	applyAllCatAdjustmentsToSimframe(., iteration, propensities, print_adj = TRUE,cat.adjustments=.$cat.adjustments)
	#'
	#'
	#'	. <- env.scenario
	#'	propensities<-list(examplevariable=array(c(0.9,0.8,0.8,0.7,0.5,0.4,0.1,0.1,0.15,0.7,0.9,0.9,0.9) , dim=c(13,1,1)),var2=array(c(0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.25,0.2,0.2,0.2,0.2) , dim=c(13,1,1)))
	#'	iteration=1
	#' 	.$simframe<-.$simframe[1:13,]
	#'	env.scenario$simframe$examplevariableLvl1<-c(1,1,1,1,0,0,0,0,0,1,1,1,1)
	#' 	env.scenario$simframe$examplevariableLvl0<-c(0,0,0,0,1,1,1,1,1,0,0,0,0)
	#' 	.$cat.adjustments$examplevariable <-matrix(rep(NA,10),ncol=2)
	#' 	.$cat.adjustments$examplevariable[1,] <- c(0.6,0.4)
	#' 	colnames(.$cat.adjustments$examplevariable)<-c("example variable =0 (%)","example variable =1 (%)")
	#' 	.$cat.adjustments$examplevariable<-structure(.$cat.adjustments$examplevariable, varnames=c("examplevariableLvl0", "examplevariableLvl1"))
	#' 	.$cat.adjustments$examplevariable<-structure(.$cat.adjustments$examplevariable, logisetexpr="residential")
	#' 	.$simframe$residential<-c(0, 0,  1,  1, 0,  1,  1,  1,  1,  1,  1,  1, 1)
	#'	applyAllCatAdjustmentsToSimframe(., iteration, propensities, print_adj = TRUE,cat.adjustments=.$cat.adjustments)
	#' 

	applyAllCatAdjustmentsToSimframe <- function(., iteration, propensities, print_adj = TRUE, cat.adjustments=.$cat.adjustments) {

		invisible(lapply(cat.adjustments, function (catadj) {
			#catadj <- .$cat.adjustments[[1]]
			#catadj <- .$cat.adjustments$z1single
			#catadj <- .$cat.adjustments$SESBTH
			#catadj <- .$cat.adjustments$catpregsmk2
			#catadj <- .$cat.adjustments$INTERACT
			cat_adj_vector <- catadj[iteration, ]
			
			#have to do this line - as cat_adjust_vector does not inherit this meta info of catadj for some reason
			cat_adj_vector <- structure(cat_adj_vector, logisetexpr=attr(catadj,"logisetexpr"))
			
			if (!any(is.na(cat_adj_vector))) {
				
				varnames <- attr(catadj, "varnames")
				catToContModels <- attr(catadj, "catToContModel")
				cont.binbreaks <- attr(catadj, "cont.binbreaks")
				
				if (is.null(varnames)) {
					stop(gettextf("Missing varnames attribute"))
				}
				
				if (!is.null(catToContModels)) {
					.$applyContAdjustmentToSimframe(varnames, iteration, cat_adj_vector, catToContModels, cont.binbreaks, propensities=NULL)
				} else {
					.$applyCatAdjustmentToSimframe(varnames, cat_adj_vector, iteration, propensities, print_adj)
				}
			}
			
		}))

	}
	
	applyAllFixedOutcomesIfSetToSimframe <- function(.) {
		iteration <- 1
		
		lapply(names(.$fixed.outcomes), function(fixedOutcomeName){
					#fixedOutcomeName <- "kids"
					.$simframe[[fixedOutcomeName]] <- selectFixedOutcomeIfSet(., iteration, .$simframe[[fixedOutcomeName]], fixedOutcomeName)
				
				})
	}

	#' Apply categorical adjustments to simframe.
	#' 
	#' @param .
	#'  simenv receiving object. .$simframe is modified.  
	#' @param varnames
	#'  varname(s) of variable(s) to adjust, eg: "catpregsmk2" or c("z1msmokeLvl0","z1msmokeLvl1")
	#' @param desired_props
	#'  a vector of desired proportions, eg: c(0.1, 0.1, 0.8).
	#'  Can have a "logisetexpr" attribute - if so, this is evaulated, and becomes a logical vector indicating which observations of "varname" to adjust.
	#' 		(i.e. the "logisetexpr" attribute gives which subset of the data the desired_props are intended for).
	#' @param propensities
	#' 		named list of propensities for the cat.adjustments
	#' @param printAdj
	#' 		if TRUE will print new proportions of modified simframe vars
	#'
	#' @return 
	#'  NULL. simframe in receiving object is modified directly.
	#' 
	#' @examples
	#'  desired_props <- cat_adj_vector  
	#'  
	#' 	desired_props<-c(0.5,0.5)
	#' 	desired_props<-structure(desired_props, logisetexpr="residential")
	#' 	.<-env.scenario
	#' 	.$simframe<-.$simframe[1:13,]
	#'  env.scenario$simframe$examplevariable<-c(1,1,1,1,0,0,0,0,0,1,1,1,1)
	#' 	varnames<-"examplevariable"
	#'  env.scenario$simframe$residential<-c(0,0,1,1,0,1,1,1,1,1,1,1,1)
	#' 	iteration=1
	#' 	propensities<-list(examplevariable=array(c(0.9,0.8,0.8,0.7,0.5,0.4,0.1,0.1,0.15,0.7,0.9,0.9,0.9) , dim=c(13,1,1)),var2=array(c(0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.25,0.2,0.2,0.2,0.2) , dim=c(13,1,1)))
	#' 	prop.table(table(env.scenario$simframe["examplevariable"][env.scenario$simframe$residential==1,]))
	#'	applyCatAdjustmentToSimframe(.,varnames, desired_props, iteration, propensities, print_adj = TRUE)
	#' 	env.scenario$simframe$examplevariable[env.scenario$simframe$residential==1]
	#' 
	#' 	desired_props<-c(0.6,0.4)
	#' 	desired_props<-structure(desired_props, logisetexpr="residential")
	#' 	.<-env.scenario
	#' 	.$simframe<-.$simframe[1:13,]
	#'  env.scenario$simframe$examplevariableLvl1<-c(1,1,1,1,0,0,0,0,0,1,1,1,1)
	#'  env.scenario$simframe$examplevariableLvl0<-c(0,0,0,0,1,1,1,1,1,0,0,0,0)
	#' 	varnames<-c("examplevariableLvl0", "examplevariableLvl1")
	#'  env.scenario$simframe$residential<-c(0,0,1,1,0,1,1,1,1,1,1,1,1)
	#' 	iteration=1
	#' 	propensities<-list(examplevariable=array(c(0.9,0.8,0.8,0.7,0.5,0.4,0.1,0.1,0.15,0.7,0.9,0.9,0.9) , dim=c(13,1,1)),var2=array(c(0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.25,0.2,0.2,0.2,0.2) , dim=c(13,1,1)))
	#' 	prop.table(table(env.scenario$simframe["examplevariableLvl1"][env.scenario$simframe$residential==1,]))
	#'	applyCatAdjustmentToSimframe(.,varnames, desired_props, iteration, propensities, print_adj = TRUE)
	#' 	env.scenario$simframe$examplevariableLvl1[env.scenario$simframe$residential==1]

	applyCatAdjustmentToSimframe <- function(., varnames, desired_props, iteration, propensities, print_adj = TRUE) {
		is_single_variable_to_adjust <- length(varnames) == 1
		
		logiset <- as.logical(evaluateLogisetExprAttribute(desired_props, .$simframe, varnames))
		
		if (is_single_variable_to_adjust) {
			propens <- propensities[[varnames]][,,iteration]
			.$applyCatAdjustmentToSimframeVarSingle(varnames, desired_props, propens, print_adj, logiset=logiset)
		} else {
			propens <- propensities[[strip_lvl_suffix(varnames[1])]][,,iteration]
			.$applyCatAdjustmentToSimframeVarMultipleBinary(varnames, desired_props, propens, print_adj,logiset=logiset)	
		}
		
	}

	#' Adjust the proportions of a single simframe variable.
	#' 
	#' @param .
	#'  simenv receiving object. .$simframe is modified.
	#' @param varname
	#'  simframe variable to adjust  
	#' @param desired_props
	#'  a vector of desired proportions, eg: c(0.1, 0.1, 0.8)
	#' @param propens
	#'  propensities for this variable, if any
	#' @param printAdj
	#'  if TRUE, display adjusted proportions after adjustment
	#' @param logiset
	#' 	logical vector indicating which rows to include, or NULL (the default) to include all. 
	#' 
	#' @return 
	#'  NULL. simframe in receiving object is modified directly.
	#' 
	#' @examples
	#' . <- env.scenario
	#' varname <- "catpregsmk2" ; varname <- varnames
	#' desired_props <- c(0.01,0.02,0.03,0.04,0.90)
	#' propens <- NULL
	#' print_adj = T
	#' applyCatAdjustmentToSimframeVarSingle(., varname, desired_props, propens, print_adj)
	#' 
	#' .<-env.scenario
	#' .$simframe<-.$simframe[1:13,] 
	#' .$simframe$examplevariable<-c(1,1,1,1,0,0,0,0,0,1,1,1,1)
	#' varname<-"examplevariable"
	#' desired_props<-c(0.5,0.5)
	#' propens<-c(0.90, 0.80, 0.80, 0.70, 0.50, 0.40, 0.10, 0.10, 0.15, 0.70, 0.90, 0.90, 0.90)
	#' logiset<-c(FALSE, FALSE,  TRUE,  TRUE, FALSE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE, TRUE)
	#' prop.table(table(.$simframe[varname][logiset,]))
	#' applyCatAdjustmentToSimframeVarSingle(., varname, desired_props, propens, print_adj = T, logiset=logiset)
	#' .$simframe$examplevariable[logiset==TRUE]


	applyCatAdjustmentToSimframeVarSingle <- function(., varname, desired_props, propens, print_adj = T, logiset=NULL) {
		if (print_adj) {
			if(is.null(logiset) || length(logiset) == 0) {
				cat(varname,"\n")
			} else {
				cat(varname,"- just for the logiset subset: ", "\n")
			}
		}
		
		if (!is.null(logiset) && length(logiset) > 0){
			.$simframe[varname]<-modifypropsVarSingle_on_subset(default.vec=.$simframe[varname], desired_props=desired_props, propens=propens, logiset=logiset)
		}
		else {
			.$simframe[varname] <- modifyProps(.$simframe[[varname]], desired_props, propens)
		}
		
		if (print_adj) {
		
			if (is.null(logiset) || length(logiset) == 0) {print(prop.table(table(.$simframe[varname])), digits=3)}
			else {print(prop.table(table(subset(.$simframe[varname],logiset))), digits=3)}
			
		}
	}
	
	#' Adjust the proportions of a simframe variable that exists in multiple binary level vectors,
	#' eg: SESBTHLvl1, SESBTHLvl2, SESBTHLvl3.
	#' 
	#' @param .
	#'  simenv receiving object. .$simframe is modified.  
	#' @param binLevelVarnames
	#'  vector of binary level varnames, eg: c("z1accomLvl0","z1accomLvl1")
	#' @param desiredProps
	#'  desired proportions
	#' @param propens
	#'  propensities, if ANY
	#' @param printAdj
	#'  if TRUE, display adjusted proportions after adjustment
	#' 	@param logiset
	#' 	logical vector indicating which rows to include, or NULL (the default) to include all. 
	#'
	#' @return 
	#'  NULL. simframe in receiving object is modified directly.
	#'  
	#' @examples
	#' . <- env.scenario
	#' binLevelVarnames <- c("SESBTHLvl1","SESBTHLvl2", "SESBTHLvl3") 
	#' desiredProps=c(0.1,0.1,0.8)
	#' propens=propensities$SESBTH[,,1]
	#' 
	#' binLevelVarnames <- c("z1single0Lvl0","z1single0Lvl1")
	#' binLevelVarnames <- c("z1accomLvl0","z1accomLvl1") ; propens <- propensities$z1accom[,1]
	#' desiredProps <- c(0,1) ; desiredProps <- c(0.5,0.5)
	#' desiredProps <- desired_props  
	#' propens <- NULL 
	#' 
	#' .$applyCatAdjustmentToSimframeVarMultipleBinary(binLevelVarnames, desiredProps, propens, TRUE)
	#'	
	#'
	#'
	#' binLevelVarnames<-c("examplevariableLvl0", "examplevariableLvl1")
	#' binLevelVarnames<-varnames
	#' .<-env.scenario
	#' .$simframe<-.$simframe[1:13,]; .$simframe$examplevariableLvl1<-c(1,1,1,1,0,0,0,0,0,1,1,1,1)
	#' .$simframe$examplevariableLvl0<-c(0,0,0,0,1,1,1,1,1,0,0,0,0)
	#' logiset<-c(FALSE, FALSE,  TRUE,  TRUE, FALSE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE,TRUE)
	#' desiredProps<-c(0.6,0.4)
	#' propens<-c(0.90, 0.80, 0.80, 0.70, 0.50, 0.40, 0.10, 0.10, 0.15, 0.70, 0.90, 0.90, 0.90)
	#' .$simframe$examplevariableLvl1[logiset==TRUE]
	#' apply(subset(.$simframe[c("examplevariableLvl0", "examplevariableLvl1")], logiset), COL, sum) / apply(subset(.$simframe[c("examplevariableLvl0", "examplevariableLvl1")], logiset), COL, length)
	#' applyCatAdjustmentToSimframeVarMultipleBinary(., binLevelVarnames, desiredProps, propens, printAdj = TRUE, logiset=logiset)
	#' .$simframe$examplevariableLvl1[logiset==TRUE]
	


	applyCatAdjustmentToSimframeVarMultipleBinary <- function (., binLevelVarnames, desiredProps, propens, printAdj = TRUE, logiset=NULL) {
		
		
		#NB: simframe may not always contain Lvl0 var. So we construct one if this is 2 level var.
		is2Level <- length(binLevelVarnames) == 2
		varnames <- intersect(binLevelVarnames, names(.$simframe))
		missingLevel <- setdiff(binLevelVarnames, names(.$simframe))
		
		vecs.list <- .$simframe[varnames]
		
		if(is2Level && length(missingLevel)) {
			# add in generated missing level
			vecs.list[missingLevel] <- as.integer(!.$simframe[varnames])  
			
			# order correctly
			vecs.list <- vecs.list[binLevelVarnames] 
		}
		
		if (!is.null(logiset) && length(logiset) > 0) {
			#subsetting the propensities according to logiset
			propens<-subset(propens, logiset)
			
			#adding a temporary ID variable - a rank column - onto a copy of the simframe portion
			#will enable the subsets to be put back into the same order later
			n <- dim(vecs.list)[1]
			sf <- data.frame(vecs.list,1:n)
			rankcolnum <- ncol(sf) 
			
			
			#subsetting the copy of the simframe according to logiset
			subset_to_change <- subset(sf,logiset)
			
			#keeping those not in the logiset - those that aren't to be passed to modifyprops
			rest_not_to_be_modified <- subset(sf,!logiset)
			
			#modifying the logiset
			subset_to_change_modified <- modifyPropsAsBinLevels(
					as.list(subset_to_change[,-rankcolnum]), 
					desiredProps=desiredProps, 
					propens=propens)
			
			#putting changed set back with those that weren't in the logiset
			new_sf <- rbind(subset_to_change_modified, rest_not_to_be_modified[,-rankcolnum]) 
			
			original.position <- rbind(as.matrix(subset_to_change[,rankcolnum]), as.matrix(rest_not_to_be_modified[,rankcolnum]))
			
			#putting the records back in their orignal order according to the rank column created earlier
			if (length(varnames)==length(binLevelVarnames)) {
				.$simframe[varnames] <- new_sf[order(original.position),]
			} else if ((length(varnames)!=length(binLevelVarnames)) & is2Level) {
				.$simframe[varnames] <- new_sf[order(original.position),2]
			} else {
				stop("add new if clause in applyCatAdjustmentToSimframeVarMultipleBinary()")
			}
		} else {
			#if there is no logiset and the scenario is being applied to everyone
			result <- modifyPropsAsBinLevels(
				vecs.list, 
				desiredProps=desiredProps, 
				propens=propens)
		
			.$simframe[varnames] <- result[varnames] 
		}
		
		if (printAdj) {
			
			if (is.null(logiset) || length(logiset) == 0) {
				print(apply(.$simframe[varnames], COL, sum) / apply(.$simframe[varnames], COL, length), digits=3)
				cat("\n")
			} else {
				cat("Just for the logiset subset: ", "\n")
				print(apply(subset(.$simframe[varnames], logiset), COL, sum) / apply(subset(.$simframe[varnames], logiset), COL, length), digits=3)
				cat("\n")
			}
			
		}
	}
	
	
	applyContAdjustmentToSimframe <- function(., varname, iteration, desiredProps, catToContModels, cont.binbreaks, propensities) {
		propens <- propensities[[varname]][,,iteration]
		logiset <- as.logical(evaluateLogisetExprAttribute(desiredProps, .$simframe))
		cat("Adjusting", varname, ": ", desiredProps, "\n")
		.$simframe[varname] <- adjust.proportions(.$simframe[[varname]], desiredProps, propens, logiset, catToContModels, cont.binbreaks, envir=.$simframe)
		.$simframe[varname]
	}
	
	
	#' Generate pre simulation stats after adjustment but before simulation begins.
	#' 
	#' Typically these will be descriptive statistics of input variables that don’t change eg: gender, ethnicity
	#' 
	#' Sub-classes override this function.
	#' 
	generatePreSimulationStats <- function(., simframe) {
		
	}
	
	#' Perform a simulation of X runs.
	#' 
	#' NB: if it exists, uses propensities in global environment when doing adjustments for year 1
	#' 
	#' @param .
	#'  Simenv receiving object
	#' @param total_runs
	#'  total number of runs to simulate
	#' @return 
	#'  NULL
	#' @examples 
	#'  . <- env.base
	#'  env.base$simulate()
	#'  . <- env.scenario
	
	simulate <- function(., total_runs=1) {
		start_time <- proc.time()
		
		cat(gettextf("Simulating %s\n", .$name))
		
		if (!exists("propensities")) propensities <- NULL
		
		.$applyAllCatAdjustmentsToSimframe(1, propensities)
		
		.$presim.stats <- .$generatePreSimulationStats(.$simframe)
		

		for (i in 1:total_runs) {
			#i = 1
			cat("Run",i,"of",total_runs,"\n")

			invisible(.$applyAllFixedOutcomesIfSetToSimframe())
			
			if (exists(".DEBUG")) {
				cat("DEBUG: Stopping to allow manual execution\n")
				return()
			}
			
			#execute simulateRun on all modules (may only be one module)
			invisible(lapply(.$modules, function(module) #module <- .$modules[[1]] 
								module$outcomes <- module$simulateRun(simenv=.)  ))
			
			#execute map_outcomes_to_run_results on all modules and store run results
			invisible(lapply(.$modules, function(module) {
								#module <- .$modules[[1]]
								run_results <- module$map_outcomes_to_run_results(.$simframe, module$outcomes, .$cat.adjustments)
								module$run_results <- c(module$run_results, list(run_results))
								names(module$run_results)[i] <- paste("run", i, sep="")
							}))
			
			.$num_runs_simulated <- .$num_runs_simulated + 1
			
		}
		
		invisible(lapply(.$modules, function(module) {
							module$run_results_collated <- module$collate_all_run_results(module$run_results, .$cat.adjustments)
						}))

		# call garbage collector to release memory used during calculation (sometimes this is a lot)
		gc()
		
		end_time <- proc.time()
		
		return(end_time - start_time)
		
	}
	
	numberOfUnits <- function(.) {
		dim(.$simframe)[1]
	}
})
