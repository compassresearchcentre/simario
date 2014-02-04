#' Change values in a categorical vector to a lower or higher value
#' 
#' @param num
#'  the number of categories above you want to steal from
#'  if you are giving and not stealing, you always give to the category  
#'  directly above  and num=1   
#' @param rank.col
#'   the colum in which the rankings will be
#' @param i
#'   the interation you are up to in the while/for loop
#' @param new.all.dat
#'   the data you are working on and want modified
#' @param n.change
#'   the number of observations that need moving into or out of each 
#'   category to get the requested proportions 
#' 
#' @return
#' the categorical vector after modified
#' 
#' @seealso This function is called by the function \code{\link{modifyProps}}
#' 
#' @export
#' @example
change.cat <- function(num, rank.col, i, new.all.dat, n.change) {
	if (sign(n.change[i])==1) {
		steal=F
	}
	else if (sign(n.change[i])== -1) {
		steal=T
	}
	#identify those in the category you want to change
	#if giving it is category i
	#if stealing it is category i+1 or above
	ch.cat.id = which(new.all.dat[,1]==(i+num*steal))
	
	#put in separate dataset
	dat.ch.cat = new.all.dat[ch.cat.id,] 
	#add the rankings of the propensity scores as the last column
	dat.ch.cat = cbind(dat.ch.cat, rank(dat.ch.cat[,i+(num-1)+1], 
					ties.method="random"))
	if (steal==F) {
		#calculate cut-off for deciding which ones to change to a higher value
		cut.off = nrow(dat.ch.cat) - n.change[i] + 1
		#change those that are greater than or equal to the cut-off to have a higher value
		dat.ch.cat[,1][dat.ch.cat[,rank.col]>=cut.off] <- (i+1)
	}
	if (steal==T) {
		#calculate cut-off for deciding which ones to change to a lower value
		cut.off = n.change[i]*(-1)
		#change those that are below the cut-off to have a lower value
		#
		dat.ch.cat[,1][dat.ch.cat[,rank.col]<=cut.off] <- i
	}
	#identify those not in the current category (category i) and put in separate 
	#dataset
	rest.id = which(new.all.dat[,1]!=(i+num*steal))
	
	dat.rest = new.all.dat[rest.id,]
	#combine the datasets so all data is together again
	new.all.dat = rbind(dat.ch.cat[,-rank.col], dat.rest)
	return(new.all.dat)
}




#' Change the default simulated values to proportions requested by the user.
#' The values of a vector are changed so that the proportions of each discrete value 
#' are that requested by the user.
#' The values that are changed are the ones with the highest propensity to do so
#' The propensity score(s) of an observation to be a higher (or lower) value can 
#' be given as input to the function.
#' If the propensity score(s) are not provided by the user then random 
#' propensity scores are generated.
#' 
#' @param props
#'  a vector that is the proportions requested by the user.
#'  The vector is the length of the number of distinct values of the variable
#'  being modified.
#' 
#' @param default.vec
#'  a vector after a run of the simulation. The values of this
#'  variable will be changed in accordance with what the user requests
#' 
#' @param propens
#'  matrix or vector of the propensity scores for each child
#'  For binary variables there is one column of propensity scores: the
#'  propensities to change from a 0 to a 1.
#'  For categorical variables with more than two categories there are multiple
#'  columns of propensity scores: E.g. for a three category variables the
#'  propensities to change from category 1 to category 2 are in the first
#'  column and the propensities to change from category 2 to category 3 are
#'  in the second column.
#' 
#' @param accuracy
#' 	gives how close the end proportions are allowed to be away from the desired proportions before an error message is given
#' 	- the default is 0.01.
#'  If the '.accuracy' global variable exists, its value will be used instead of that in function call.
#' 
#' @note Assumptions made by the function:
#' It is assumed that the proportions given in props are given in consectuive 
#' increasing order (e.g. {0,1}, {1, 2, 3} or {2, 5, 9, 23}).  If the user 
#' wants to make it so no observations are in a particular category the value 
#' 0 must be put in the corresponding place in the vector props
#' If the propensity scores (propens) are provided by the user then it is assumed 
#' that default.vec and propens are given in the same order and exactly the 
#' same children are in each vector (i.e. there are no children in one vector 
#' that are not in the other).  In other words, the propensity score for a 
#' specific child is in the same row in propens as that same child's value of 
#' the variable in default.vec.
#' 
#' @return 
#' a modified vector with the proportions requested.
#' 
#' @seealso This function calls \code{\link{change.cat}}
#' 
#' @export 
#' @examples
#' \dontrun{
#' default.vec <- children$SESBTH
#' desired_props <- c(0.1,0.1,0.8)
#' propens <- data.frame(propensities$SESBTH)
#' new.vec <- modifyProps(default.vec, desired_props, propens)
#' table(default.vec)/sum(table(default.vec))
#' table(new.vec)/sum(table(new.vec))
#' 
#' default.vec <- env.scenario$simframe$z1accomLvl1
#' desired_props <- c(0.1,0.9)
#' propens <- propensities$z1accom[,,5]
#' #propensities$z1accom is a 3 dimentional array so we take only the the 5th z dimension
#' 	#(the propensities for year 5)
#'  table(default.vec)/sum(table(default.vec))
#' new.vec <- modifyProps(default.vec, desired_props, propens)
#'  table(new.vec)/sum(table(new.vec))
#' 
#' default.vec <- env.scenario$simframe$catpregsmk2
#' desired_props <- c(0.1, 0.1, 0.1, 0.5, 0.2)
#' propens <- NULL
#' 
#' prop.table(table(default.vec))
#' prop.table(table(modifyProps(default.vec, desired_props, propens)))
#' 
#' desired_props <- c(.05, .1, .2, .15, .3, .12, .08)
#' fhrs.binbreaks = attr(env.scenario$cat.adjustments$fhrswrk, "cont.binbreaks")
#' x.cat <- bin(x.cont,fhrs.binbreaks)
#' adj.x.cat <- modifyProps(x.cat, desired_props, propens, accuracy)
#' }
modifyProps2 <- function(default.vec, desired_props, propens=NULL, accuracy=.01) {
	if (exists(".accuracy")) {accuracy<-.accuracy}
	
	if (is.null(desired_props) || any(is.na(desired_props))) {
		#no props, silently do nothing	  
		return(default.vec)
	}
	
	if (is.null(default.vec)) {
		stop(gettextf("%s is NULL", as.character(sys.call())[2]))
	}
	
	#keep original vector
	orig.default.vec <- default.vec
	
	dim(propens)
	dim(orig.default.vec)
	if (!is.null(propens) 
			&& ( 
				(length(dim(propens)) != 2 && length(orig.default.vec) != length(propens))
				|| (length(dim(propens)) == 2 && length(orig.default.vec) != dim(propens)[1])
				) ) {
		firstParamName <- as.character(sys.call())[2]
		stop(gettextf("Propensities must be the same length as %s ",firstParamName))
	}
	
	#check that the number of categories in the provided vector of data 
	#(default.vec) is the same number of categories as given in desired_props
	num.cat <- length(table(orig.default.vec))
	num.cat2 <- length(desired_props)
	if (num.cat!=num.cat2) {
		note2 = cat("Note: Length of desired_props not equal to number of categories in variable:", 
				"\n", "Assumed that there were unobserved categories in the variable", 
				"\n")
	}
	
	#n is the number of children in one year
	n <- length(orig.default.vec)
	type <- is(orig.default.vec)[1] 
	
	#if propensity scores not provided then create them
	if (length(propens)==0 || any(is.na(propens))) {
		
		note2 <- "Note: No propensity scores available so random propensity scores were created\n"
		cat(note2)
		
		#col.num is the number of vectors of propensity scores required.
		#E.g. for a 2-category varaible, 1 vector of propensity scores is required
		#and for a 3-category variable, 2 vectors of propensity scores are
		#required etc.
		col.num <- num.cat2 - 1
		
		#create random propensity score
		propens.score <- NULL
		for (i in 1:col.num) {
			propens.score <- c(propens.score, runif(n))
		}
		propens <- matrix(propens.score, ncol=col.num, nrow=n)
	}
	
	#if the the default.vec values are not consecutive numbers starting at 1 change them 
	#so they are
	if (type=="factor") {
		tab.names <- names(table(orig.default.vec))
	} else if ((type=="integer")|(type=="numeric")) {
		tab.names <- as.numeric(names(table(orig.default.vec)))
	} else {
		stop("Add additional type in modifyProps()")	
	}
	
	if (sum(tab.names!= 1:length(desired_props))>=1) {
		default.vec2 <- numeric(length(default.vec))
		for (i in 1:length(desired_props)) {
			default.vec2[default.vec==tab.names[i]] <- i
		}
		default.vec <- default.vec2
	}
	
	#match propensity scores to children
	#(this function assumed that default.vec and propens have the same children
	#in the same order) 
	#the last column is a child identifier so I can put them back in the right
	#order
	all.dat <- data.frame(default.vec, propens, 1:n)
	new.all.dat <- all.dat
	
	#rank.col identifies the column that the rankings will be in
	#this is used later in the change.cat function where the propensities are converted to ranks
	rank.col <- ncol(all.dat) + 1
	
	#create table of given data and calculate current proportions and the numbers 
	#that need moving into or out of categories to get the requested proportions 
	#(n.change)
	tab <- table(new.all.dat[,1])
	cats <- c(1:length(desired_props))
	#if any categories in desired_props are not present in default.vec then this merge is needed to fix the 
	#problem
	tab.df <- merge(cats, tab, by = 1, all=TRUE)
	#after the merge any categories in cat that were not present in tab appear as NAs
	#these NAS are changed to 0s
	na.id <- which(is.na(tab.df$Freq))
	tab.df$Freq[na.id] <- 0
	tab.df$x <- 1:nrow(tab.df)
	current.props <- tab.df$Freq/sum(tab.df$Freq)
	n.change <- round(current.props*n) - round(desired_props*n)
	#(e.g n.change[1] is the excess/deficient number of observations in the first 
	#category in observed data)
	
	num <- 1
	i <- 1 #i = current category
	while (i < length(desired_props)) {
		if (n.change[i]==0) {
			#if no change needs to be made for category i then move onto next category
			i <- i + 1
			num <- 1 
		} else if (sign(n.change[i])==1) {
			#category i has too many obs - give to a higher category
			num <- 1
			new.all.dat <- change.cat(num, rank.col, i, new.all.dat, n.change)
			i <- i + 1
		} else if (sign(n.change[i])== -1) {
			#category i has too few obs - steal from a higher category
			new.all.dat <- change.cat(num, rank.col, i, new.all.dat, n.change)
			num <- num + 1
			
			#the maximum value of num is the number of categories above to steal 
			#from
			#by incrementing i here we ensure that we don't get into an infinite 
			#loop due to the while condition never being satisfied
			if (num>(length(desired_props)-(i-1))) {
				i <- i + 1
			}
		}
		#at this point changes have been made for one iteration of category i (may need more than 
		#one iteration if couldn't steal enough observations from the category  
		#immediately above it
		#create table of current data (tab.df) and calculate numbers that need
		#moving into or out of categories to get the requested proportions 
		#(n.change)
		tab <- table(new.all.dat[,1])
		cats <- c(1:length(desired_props))
		tab.df <- merge(cats, tab, by = 1, all.x=TRUE)
		na.id <- which(is.na(tab.df$Freq))
		tab.df$Freq[na.id] <- 0
		tab.df$x <- 1:nrow(tab.df)
		current.props <- tab.df$Freq/sum(tab.df$Freq)
		n.change <- round(current.props*n) - round(desired_props*n)
	} 
	#check if requested proportions acheived
	if (sum(abs(desired_props - current.props))<=accuracy) {
		#if correct
		#change back to orignal category names (if they were changed earlier)
		#e.g. if the orginal variable was a {0, 1} variable then all 0s would have 
		#been changed to 1s and alls 1s changed to 2s.  At this step, after the 
		#changes to get the right proportions, the 1s are changed back to 0s and 
		#the 2s are changed back to 1s.
		if (sum(tab.names!= 1:length(desired_props))>=1) {
			default.vec2 = numeric(nrow(new.all.dat))
			if (type=="factor") {
				for (i in 1:length(desired_props)) {
					default.vec2[new.all.dat[,1]==i] <- levels(orig.default.vec)[i]
					default.vec2 <- factor(default.vec2, levels=levels(orig.default.vec))
				}
				new.all.dat[,1] = default.vec2
			} else if ((type=="integer")|(type=="numeric")) {
				for (i in 1:length(desired_props)) {
					default.vec2[new.all.dat[,1]==i] <- tab.names[i]
				}
				new.all.dat[,1] = default.vec2
			} else {
				stop("Add additional type in modifyProps()")
			}
		}
		
		#and put children back in the right order
		new.all.dat2 = new.all.dat[order(new.all.dat[,rank.col-1]),]
		
		return(new.all.dat2[,1])
		
	} else if (sum(abs(desired_props - current.props))>accuracy) {
		#if not correct - still do these things but give output with warning
		#(output should all be correct if I have thought of everything and made no 
		#mistakes)
		#change back to orignal category names
		if (sum(tab.names!= 1:length(desired_props))>=1) {
			default.vec2 = numeric(nrow(new.all.dat))
			if (type=="factor") {
				for (i in 1:length(desired_props)) {
					default.vec2[new.all.dat[,1]==i] <- levels(orig.default.vec)[i]
					default.vec2 <- factor(default.vec2, levels=levels(orig.default.vec))
				}
				new.all.dat[,1] = default.vec2
			} else if ((type=="integer")|(type=="numeric")) {
				for (i in 1:length(desired_props)) {
					default.vec2[new.all.dat[,1]==i] <- tab.names[i]
				}
				new.all.dat[,1] = default.vec2
			} else {
				stop("Add additional type in modifyProps()")
			}
		}
		
		#put observations back in the right order
		new.all.dat2 = new.all.dat[order(new.all.dat[,rank.col-1]),]
		
		warning("Proportions may not be correct - Source code may need to be modified to handle this situaion")
		return(new.all.dat2[,1])
	}
}




#' Takes a categorical var specified in separate binary level variables
#' and applies modifyProps and returns the result as a list of modified
#' binary level variables.
#' 
#' @param vecs.list
#'  vectors to modify, eg: simframe[binLevelVarnames]
#' @param desiredProps
#'  desired proportions
#' @param propens
#'  propensities, if ANY
#'  
#' @return
#'  same list of vectors, but with proportions modified
#' 
#' @export
#' @examples
#' \dontrun{
#' level1 <- env.scenario$simframe$SESBTHLvl1
#' level2 <- env.scenario$simframe$SESBTHLvl2
#' level3 <- env.scenario$simframe$SESBTHLvl3
#' vecs.list <- list(level1=level1, level2=level2, level3=level3) ; vecs.list <- list(level1, level2, level3)
#' desiredProps <- c(0.1,0.1,0.8)
#' propens <- propensities$SESBTH[,,1]
#' r <- modifyPropsAsBinLevels(vecs.list, desiredProps = desiredProps, propens = propens)
#' 
#' level0 <- simframe$z1chparLvl0; level1 <- simframe$z1chparLvl1
#' desiredProps <- c(0,1)
#' vecs.list <- list(level0=level0, level1=level1) ; vecs.list <- list(level0, level1)  
#' vecs.list <- simframe[binLevelVarnames]
#' r <- modifyPropsAsBinLevels(vecs.list, desiredProps = desiredProps, propens = propens)
#' }
modifyPropsAsBinLevels <- function (vecs.list, desiredProps, propens=NULL) {
	
	#catvar <- binary.levels.combine(simframe[binLevelVarnames])
	cats <- seq(length(vecs.list))
	
	catvar <- binary.levels.combine(vecs.list)
	# prop.table(table(catvar))
	
	adjcatvar <- modifyProps(catvar, desiredProps, propens)
	
	# quick check: this will fail if modifyProps returns any NAs
	# like for example, where there is only 1 category catvar
	assert(!is.na(adjcatvar))
	
	# prop.table(table(adjcatvar))
	
	# split back into seperate level vars
	result <- binary.levels.split(adjcatvar, f=cats)
	
	result.names <- names(vecs.list)[as.integer(names(result))]
	
	structure(result, names=names(vecs.list))
}


#' Calls modifyProps on a subset, returning the whole vector, but with the subset modified
#' 
#' @param desired_props
#'  a vector that is the proportions requested by the user.
#'  The vector is the length of the number of distinct values of the variable
#'  being modified.
#' 
#' @param default.vec
#'  a vector after a run of the simulation. The values of this
#'  variable will be changed in accordance with what the user requests
#' 
#' @param propens
#'  matrix or vector of the propensity scores for each child
#'  For binary variables there is one column of propensity scores: the
#'  propensities to change from a 0 to a 1.
#'  For categorical variables with more than two categories there are multiple
#'  columns of propensity scores: E.g. for a three category variables the
#'  propensities to change from category 1 to category 2 are in the first
#'  column and the propensities to change from category 2 to category 3 are
#'  in the second column.
#' 
#' @param logiset
#' logical vector indicating which observations to include, or NULL to include all.
#' 
#' @param accuracy
#'  gives how close the end proportions are allowed to be away from the desired 
#' proportions - the default is 0.01. It is passed to function modifyProps().
#'  
#' 
#' @note Assumptions made by the function:
#' It is assumed that the proportions given in props are given in consectuive 
#' increasing order (e.g. {0,1}, {1, 2, 3} or {2, 5, 9, 23}).  If the user 
#' wants to make it so no observations are in a particular category the value 
#' 0 must be put in the corresponding place in the vector props
#' If the propensity scores (propens) are provided by the user then it is assumed 
#' that default.vec and propens are given in the same order and exactly the 
#' same children are in each vector (i.e. there are no children in one vector 
#' that are not in the other).  In other words, the propensity score for a 
#' specific child is in the same row in propens as that same child's value of 
#' the variable in default.vec.
#' 
#' @return
#' a vector with the subset modified
#' 
#' @seealso This function calls \code{\link{modifyProps}}
#' 
#' @export
#' @examples
#' default.vec<-c(1,1,1,1,0,0,0,0,0,1,1,1,1)
#' logiset<-c(FALSE, FALSE,  TRUE,  TRUE, FALSE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE,TRUE)
#' desired_props<-c(0.5,0.5)
#' propens<-c(0.90, 0.80, 0.80, 0.70, 0.50, 0.40, 0.10, 0.10, 0.15, 0.70, 0.90, 0.90, 0.90)
#' a<-modifypropsVarSingle_on_subset(default.vec=default.vec, desired_props=desired_props, propens=propens, logiset=logiset)
#' prop.table(table(a[logiset]))
#' 
#' 
#' default.vec<-c(1,1,1,1,0,0,0,0,0,1,1,1,1)
#' logiset<-NULL
#' desired_props<-c(0.5,0.5)
#' propens<-c(0.90, 0.80, 0.80, 0.70, 0.50, 0.40, 0.10, 0.10, 0.15, 0.70, 0.90, 0.90, 0.90)
#' a<-modifypropsVarSingle_on_subset(default.vec=default.vec, desired_props=desired_props, propens=propens, logiset=logiset)
#' prop.table(table(a[logiset]))

modifypropsVarSingle_on_subset<-function(default.vec, desired_props, propens=NULL, logiset=NULL, accuracy=.01) {
	if (is.null(logiset)) {logiset<-rep(T, length(default.vec))}
	default.df<-as.data.frame(default.vec)
	propens<-subset(propens, logiset)
	
	#adding a temporary ID variable - a rank column - onto a copy of the simframe portion
	#will enable the subsets to be put back into the same order later
	n<-dim(default.df)[1]
	sf<-data.frame(default.df,1:n)
	rankcolnum<-2 
	
	
	#subsetting the copy of the simframe according to logiset
	subset_to_change<-subset(sf,logiset)
	
	#keeping those not in the logiset - those that aren't to be passed to modifyprops
	rest_not_to_be_modified<-subset(sf,!logiset)
	
	#modifying the logiset
	subset_to_change_modified <- modifyProps(subset_to_change[,-rankcolnum], desired_props, propens, accuracy)
	
	#putting changed set back with those that weren't in the logiset
	new_sf<-rbind(as.matrix(subset_to_change_modified), as.matrix(rest_not_to_be_modified[,1])) 
	
	original.position<-rbind(as.matrix(subset_to_change[,rankcolnum]), as.matrix(rest_not_to_be_modified[,rankcolnum]))
	
	#putting the records back in their orignal order according to the rank column created earlier
	new_sf[order(original.position),]

}


#' Runs modifyProps on a continuous variable
#' Takes a continuous variable, converts it to a categorical variable using the binbreaks,
#' modifyProps is then called on that categorical variable.
#' The categorical variable is then converted back to a continuos variable using the catToContModels
#' 
#' @param x.cont
#'  a continuous variable to be adjusted
#' 
#' @param desired_props
#'  desired proportions
#' 
#' @param catToContModels
#' a list of models which will to used to convert the adjusted categorical variable back 
#' to continuous
#' 
#' @param cont.binbreaks
#' binbreaks for the continuous variable to be adjusted
#' 
#' @param propens 
#'  matrix or vector of the propensity scores for each child
#'  For binary variables there is one column of propensity scores: the
#'  propensities to change from a 0 to a 1.
#'  For categorical variables with more than two categories there are multiple
#'  columns of propensity scores: E.g. for a three category variables the
#'  propensities to change from category 1 to category 2 are in the first
#'  column and the propensities to change from category 2 to category 3 are
#'  in the second column.
#' 
#' @param logiset
#'  logical vector indicating which observations to include, or NULL to include all.
#' 
#' @param accuracy
#' 	gives how close the end proportions are allowed to be away from the desired 
#' proportions before an error message is given	- the default is 0.01.
#'  If the '.accuracy' global variable exists, its value will be used instead of that in 
#' function call.
#' 
#' @param envir
#'  environment in which to evaluate model variables.
#' 
#' @return
#'  an 'adjusted' continuous variable that if binned will have the same proportions as requested in desired_props
#' 
#' @examples
#' \dontrun{
#' simframe.master$age=2
#' desired_props <- rep(1/7, 7)
#' desired_props <-c(.05, .1, .15, .2, .25, .2, .05) 
#' test <- modifyPropsContinuous(simframe.master$fhrswrk, modifyPropsContinuous, catToContModels$fhrswrk, cont.binbreaks=attr(env.scenario$cat.adjustments$fhrswrk, "cont.binbreaks"), envir=simframe.master)
#' fhrs.binbreaks = attr(env.scenario$cat.adjustments$fhrswrk, "cont.binbreaks")
#' check <- bin(test, fhrs.binbreaks)
#' table(check)
#' table(check)/sum(table(check))
#' }
modifyPropsContinuous <- function(x.cont, desired_props, catToContModels, cont.binbreaks, propens=NULL, logiset=NULL, accuracy=.01, envir=parent.frame()) {
	x.cat <- bin(x.cont, cont.binbreaks)
	adj.x.cat <- modifyProps(x.cat, desired_props, propens, accuracy)
	adj.x.cont <- predSimModSelect(adj.x.cat, catToContModels, cont.binbreaks, logiset, envir)
	adj.x.cont
}


#' A wrapper for modifyProps.  
#' Subsets by the logiset call the appropriate version of modifyProps 
#' (modifyPropsContinuous or modifyProps) then, if modifyProps was called on a logiset,
#' combine and reorder the data using combine.and.reorder(). 
#' Called in adjustCatVar(), adjustContVar, and applyContAdjustmentToSimframe(). 
#' 
#' @param x
#' A categorical vector to be adjusted.
#' 
#' @param desiredProps
#' Vector of desired proportions
#' 
#' @param propens
#' propensity scores used to decide who should change categories
#' 
#' @param logiset
#' A TRUE/FALSE vector indicating the subset of units to apply the scenari (change in 
#' proportions) to
#' 
#' @param catToContModels
#' A list of models which will to used to convert the adjusted categorical variable back 
#' to continuous.
#' 
#' @param cont.binbreaks
#' Binbreaks for the variable being adjusted if exist.  Used to ensure the imputed
#' continuous values (only for continuous variables) are within the bounds of the 
#' category.
#' 
#' @param envir
#' environment - for the MELC MSM is usually the simframe of the scenario environment.
#' 
#' @return
#' an adjusted vector, either categorical or continuous depending on whether catToCont 
#' models were provided.
#' 
#' @export 
#' @example
#' 

adjust.proportions <- function(x, desiredProps, propens=NULL, logiset=NULL, catToContModels=NULL, cont.binbreaks=NULL, envir=parent.frame()) {
	if (!is.null(logiset) && length(logiset)>0) {
		#subset the propensities according to the logiset
		propens_subset <- if (!is.null(propens)) { subsetFirstDimension(propens, logiset) } else NULL
		
		#subset x according to the logiset
		subset_to_change <- x[logiset]
		
		if (!is.null(catToContModels)) {
			subset_to_change_modified <- modifyPropsContinuous(subset_to_change, desiredProps, catToContModels, cont.binbreaks, propens_subset, logiset, accuracy=.05, envir)
		} else {
			subset_to_change_modified <- modifyProps(subset_to_change, desiredProps, propens_subset, accuracy=.05)
		}
		
		non.modified.x <- x[!logiset]
		
		modified.in.order <- combine.and.reorder(subset_to_change_modified, non.modified.x, logiset)
		
		return(modified.in.order)
	} else {
		#there is no logiset and the scenario is applied to all units
		if (!is.null(catToContModels)) {
			adj.x.cont <- modifyPropsContinuous(x, desiredProps, catToContModels, cont.binbreaks, propens, envir=envir)
			return(adj.x.cont)
		} else {
			adj.x.cat <- modifyProps(x, desiredProps, propens, accuracy=.05)
			return(adj.x.cat)
		}
	}
}



#' Combines and reorders (so correct original order) after modifyProps has been called on
#' a logiset.
#' Called in adjust.proportions().  
#' 
#' @param modified.x
#' A vector of modified values (i.e. those tht were in the logiset.  Either categorical or
#' continuous. 
#' 
#' @param non.modified.x
#' A vector of non-modified values of the same variable (i.e. those tht were not in the 
#' logiset.  Either categorical or continuous.
#' 
#' @param logiset
#' A TRUE/FALSE vetor defining which units are in the logical subset on which modifyProps
#' was called.
#' 
#' @return 
#' A combined and reordered vector.  Contains evereyone in the population in the correct 
#' order.
#' 
#' @export 
#' @example 
#' modified.x <- 1:10*2-1
#' non.modified.x <- 1:10*2
#' logiset<-rep(c(TRUE,FALSE),10) 
#' combine.and.reorder(modified.x, non.modified.x, logiset)
combine.and.reorder <- function(modified.x, non.modified.x, logiset) {
	n = length(modified.x) + length(non.modified.x)
	original.position <- 1:n
	modified.out.of.order <- rbind(cbind(modified.x, original.position[logiset]), cbind(non.modified.x, original.position[!logiset]))
	modified.in.order <- modified.out.of.order[order(modified.out.of.order[,2]), 1]
	return(modified.in.order)
}

cat("Loaded modifyProps.r\n")






########### Add missing categories ##############
modifyProps <- function(default.vec, desired_props, propens=NULL, accuracy=.01) {

				
	# If the length of desired_props not equal to number of categories in variable which means
	# there were unobserved categories in the variable, then we choose the units randomly from the 
	# the category in majority and apply the missing categories to them.
	
	addMissingCategories <- function(){
		
		varname <- attr(desired_props,"varname")
		
		# The categories of the variable
		#categories<-as.vector(dict_demo$codings[[varname]])
		#binbreak <- attr(desired_props,"binbreak")
		#if(length(binbreak)==0){
			#categories <- names(binbreak)[-1]
		#}else{
			categories <- attr(desired_props,"levels")
		#}
		
		num.missingCategories=0
		
		# The list to store all missing categories
		missingCategories <- list()
		
		# Store all missing categories into missingCategories 
		for(i in categories){
			if(!any(default.vec == categories[i])){
				num.missingCategories <- num.missingCategories+1
				missingCategories[[num.missingCategories]] <- categories[i]
			}
		}
		
		# check the type of the categories and get the category in majority
		if (type=="factor") {
			majorityCategory <- names(which.max(table(default.vec)))
		} else if ((type=="integer")|(type=="numeric")) {
			majorityCategory <- as.numeric(names(which.max(table(default.vec))))
		} else {
			stop("Add additional type in modifyProps()")	
		}
		
		# A logical vector to store if each element belongs to the category in majority
		isMajority <- default.vec == majorityCategory 
		
		# The number of units in majority category
		num.majority <- max(table(default.vec))
		
		# Choose the units randomly from the category in majority
		randomPeople <- sample(1:num.majority, num.missingCategories, replace=TRUE)
		
		# Apply the missing units to random units and store into default.vec
		if(num.majority >= num.missingCategories){
			for(i in (1:length(missingCategories))){
				default.vec[isMajority][randomPeople[i]] <<- missingCategories[[i]]
			}
		}else{
			stop("Error: The total number of micro-units in the subgroup being adjusted should be at least the number of categories in the variable being adjusted")
		}
	}
	
	
	
	#if the the default.vec values are not consecutive numbers starting at 1 change them 
	#so they are
	convertCategoryName <- function(){
		if (type=="factor") {
			tab.names <<- names(table(default.vec))
		} else if ((type=="integer")|(type=="numeric")) {
			tab.names <<- as.numeric(names(table(default.vec)))
		} else {
			stop("Add additional type in modifyProps()")	
		}
		
		if (sum(tab.names!= 1:length(desired_props))>=1) {
			default.vec2 <- numeric(length(default.vec))
			for (i in 1:length(desired_props)) {
				default.vec2[default.vec==tab.names[i]] <- i
			}
			default.vec <<- default.vec2
		}
	}
	
	
	
	#check if requested proportions acheived with the accuracy and transform back to orignal category names.
	checking <- function(){
		if (sum(abs(desired_props - current.props))<=accuracy) {
			#if correct
			#change back to orignal category names (if they were changed earlier)
			#e.g. if the orginal variable was a {0, 1} variable then all 0s would have 
			#been changed to 1s and alls 1s changed to 2s.  At this step, after the 
			#changes to get the right proportions, the 1s are changed back to 0s and 
			#the 2s are changed back to 1s.
			if (sum(tab.names!= 1:length(desired_props))>=1) {
				default.vec2 = numeric(nrow(new.all.dat))
				if (type=="factor") {
					for (i in 1:length(desired_props)) {
						default.vec2[new.all.dat[,1]==i] <- levels(orig.default.vec)[i]
						default.vec2 <- factor(default.vec2, levels=levels(orig.default.vec))
					}
					new.all.dat[,1] = default.vec2
				} else if ((type=="integer")|(type=="numeric")) {
					for (i in 1:length(desired_props)) {
						default.vec2[new.all.dat[,1]==i] <- tab.names[i]
					}
					new.all.dat[,1] = default.vec2
				} else {
					stop("Add additional type in modifyProps()")
				}
			}
			
			#and put children back in the right order
			new.all.dat2 = new.all.dat[order(new.all.dat[,rank.col-1]),]
			
			return(new.all.dat2[,1])
			
		} else if (sum(abs(desired_props - current.props))>accuracy) {
			#if not correct - still do these things but give output with warning
			#(output should all be correct if I have thought of everything and made no 
			#mistakes)
			#change back to orignal category names
			if (sum(tab.names!= 1:length(desired_props))>=1) {
				default.vec2 = numeric(nrow(new.all.dat))
				if (type=="factor") {
					for (i in 1:length(desired_props)) {
						default.vec2[new.all.dat[,1]==i] <- levels(orig.default.vec)[i]
						default.vec2 <- factor(default.vec2, levels=levels(orig.default.vec))
					}
					new.all.dat[,1] = default.vec2
				} else if ((type=="integer")|(type=="numeric")) {
					for (i in 1:length(desired_props)) {
						default.vec2[new.all.dat[,1]==i] <- tab.names[i]
					}
					new.all.dat[,1] = default.vec2
				} else {
					stop("Add additional type in modifyProps()")
				}
			}
			
			#put observations back in the right order
			new.all.dat2 = new.all.dat[order(new.all.dat[,rank.col-1]),]
			
			warning("Proportions may not be correct - Source code may need to be modified to handle this situaion")
			return(new.all.dat2[,1])
		}
	}
	
	
	
	if (exists(".accuracy")) {accuracy<-.accuracy}
	
	if (is.null(desired_props) || any(is.na(desired_props))) {
		#no props, silently do nothing	  
		return(default.vec)
	}
	
	if (is.null(default.vec)) {
		stop(gettextf("%s is NULL", as.character(sys.call())[2]))
	}
	
	if(length(default.vec)==0){
		#if the default vector is empty, silently do nothing	
		return(default.vec)
	}
	
	#keep original vector
	orig.default.vec <- default.vec
	#n is the number of children in one year
	n <- length(orig.default.vec)
	type <- is(orig.default.vec)[1] 
	
	dim(propens)
	dim(orig.default.vec)
	if (!is.null(propens) 
			&& ( 
				(length(dim(propens)) != 2 && length(orig.default.vec) != length(propens))
				|| (length(dim(propens)) == 2 && length(orig.default.vec) != dim(propens)[1])
				) ) {
		firstParamName <- as.character(sys.call())[2]
		stop(gettextf("Propensities must be the same length as %s ",firstParamName))
	}
	
	#check that the number of categories in the provided vector of data 
	#(default.vec) is the same number of categories as given in desired_props
	# If not, add the missing categories to the provided vector.
	num.cat <- length(table(orig.default.vec))
	num.cat2 <- length(desired_props)
	if (num.cat!=num.cat2) {
		note2 = cat("Note: Length of desired_props not equal to number of categories in variable:", 
				"\n", "Assumed that there were unobserved categories in the variable", 
				"\n")
		addMissingCategories()
	}
	

	
	#if propensity scores not provided then create them
	if (length(propens)==0 || any(is.na(propens))) {
		
		note2 <- "Note: No propensity scores available so random propensity scores were created\n"
		cat(note2)
		
		#col.num is the number of vectors of propensity scores required.
		#E.g. for a 2-category varaible, 1 vector of propensity scores is required
		#and for a 3-category variable, 2 vectors of propensity scores are
		#required etc.
		col.num <- num.cat2 - 1
		
		#create random propensity score
		propens.score <- NULL
		for (i in 1:col.num) {
			propens.score <- c(propens.score, runif(n))
		}
		propens <- matrix(propens.score, ncol=col.num, nrow=n)
	}
	
	convertCategoryName()
	
	#match propensity scores to children
	#(this function assumed that default.vec and propens have the same children
	#in the same order) 
	#the last column is a child identifier so I can put them back in the right
	#order
	all.dat <- data.frame(default.vec, propens, 1:n)
	new.all.dat <- all.dat
	
	#rank.col identifies the column that the rankings will be in
	#this is used later in the change.cat function where the propensities are converted to ranks
	rank.col <- ncol(all.dat) + 1
	
	#create table of given data and calculate current proportions and the numbers 
	#that need moving into or out of categories to get the requested proportions 
	#(n.change)
	tab <- table(new.all.dat[,1])
	cats <- c(1:length(desired_props))
	#if any categories in desired_props are not present in default.vec then this merge is needed to fix the 
	#problem
	tab.df <- merge(cats, tab, by = 1, all=TRUE)
	#after the merge any categories in cat that were not present in tab appear as NAs
	#these NAS are changed to 0s
	na.id <- which(is.na(tab.df$Freq))
	tab.df$Freq[na.id] <- 0
	tab.df$x <- 1:nrow(tab.df)
	current.props <- tab.df$Freq/sum(tab.df$Freq)
	n.change <- round(current.props*n) - round(desired_props*n)
	#(e.g n.change[1] is the excess/deficient number of observations in the first 
	#category in observed data)
	
	num <- 1
	i <- 1 #i = current category
	while (i < length(desired_props)) {
		if (n.change[i]==0) {
			#if no change needs to be made for category i then move onto next category
			i <- i + 1
			num <- 1 
		} else if (sign(n.change[i])==1) {
			#category i has too many obs - give to a higher category
			num <- 1
			new.all.dat <- change.cat(num, rank.col, i, new.all.dat, n.change)
			i <- i + 1
		} else if (sign(n.change[i])== -1) {
			#category i has too few obs - steal from a higher category
			new.all.dat <- change.cat(num, rank.col, i, new.all.dat, n.change)
			num <- num + 1
			
			#the maximum value of num is the number of categories above to steal 
			#from
			#by incrementing i here we ensure that we don't get into an infinite 
			#loop due to the while condition never being satisfied
			if (num>(length(desired_props)-(i-1))) {
				i <- i + 1
			}
		}
		#at this point changes have been made for one iteration of category i (may need more than 
		#one iteration if couldn't steal enough observations from the category  
		#immediately above it
		#create table of current data (tab.df) and calculate numbers that need
		#moving into or out of categories to get the requested proportions 
		#(n.change)
		tab <- table(new.all.dat[,1])
		cats <- c(1:length(desired_props))
		tab.df <- merge(cats, tab, by = 1, all.x=TRUE)
		na.id <- which(is.na(tab.df$Freq))
		tab.df$Freq[na.id] <- 0
		tab.df$x <- 1:nrow(tab.df)
		current.props <- tab.df$Freq/sum(tab.df$Freq)
		n.change <- round(current.props*n) - round(desired_props*n)
	} 
	
	
	checking()

}


########### Tidy up the original function (modifyProps2) ##############
modifyProps1 <- function(default.vec, desired_props, propens=NULL, accuracy=.01) {
	
	
	#if the the default.vec values are not consecutive numbers starting at 1 change them 
	#so they are
	convertCategoryName <- function(){
		if (type=="factor") {
			tab.names <<- names(table(default.vec))
		} else if ((type=="integer")|(type=="numeric")) {
			tab.names <<- as.numeric(names(table(default.vec)))
		} else {
			stop("Add additional type in modifyProps()")	
		}
		
		if (sum(tab.names!= 1:length(desired_props))>=1) {
			default.vec2 <- numeric(length(default.vec))
			for (i in 1:length(desired_props)) {
				default.vec2[default.vec==tab.names[i]] <- i
			}
			default.vec <<- default.vec2
		}
	}
	
	
	
	#check if requested proportions acheived with the accuracy and transform back to orignal category names.
	checking <- function(){
		if (sum(abs(desired_props - current.props))<=accuracy) {
			#if correct
			#change back to orignal category names (if they were changed earlier)
			#e.g. if the orginal variable was a {0, 1} variable then all 0s would have 
			#been changed to 1s and alls 1s changed to 2s.  At this step, after the 
			#changes to get the right proportions, the 1s are changed back to 0s and 
			#the 2s are changed back to 1s.
			if (sum(tab.names!= 1:length(desired_props))>=1) {
				default.vec2 = numeric(nrow(new.all.dat))
				if (type=="factor") {
					for (i in 1:length(desired_props)) {
						default.vec2[new.all.dat[,1]==i] <- levels(orig.default.vec)[i]
						default.vec2 <- factor(default.vec2, levels=levels(orig.default.vec))
					}
					new.all.dat[,1] = default.vec2
				} else if ((type=="integer")|(type=="numeric")) {
					for (i in 1:length(desired_props)) {
						default.vec2[new.all.dat[,1]==i] <- tab.names[i]
					}
					new.all.dat[,1] = default.vec2
				} else {
					stop("Add additional type in modifyProps()")
				}
			}
			
			#and put children back in the right order
			new.all.dat2 = new.all.dat[order(new.all.dat[,rank.col-1]),]
			
			return(new.all.dat2[,1])
			
		} else if (sum(abs(desired_props - current.props))>accuracy) {
			#if not correct - still do these things but give output with warning
			#(output should all be correct if I have thought of everything and made no 
			#mistakes)
			#change back to orignal category names
			if (sum(tab.names!= 1:length(desired_props))>=1) {
				default.vec2 = numeric(nrow(new.all.dat))
				if (type=="factor") {
					for (i in 1:length(desired_props)) {
						default.vec2[new.all.dat[,1]==i] <- levels(orig.default.vec)[i]
						default.vec2 <- factor(default.vec2, levels=levels(orig.default.vec))
					}
					new.all.dat[,1] = default.vec2
				} else if ((type=="integer")|(type=="numeric")) {
					for (i in 1:length(desired_props)) {
						default.vec2[new.all.dat[,1]==i] <- tab.names[i]
					}
					new.all.dat[,1] = default.vec2
				} else {
					stop("Add additional type in modifyProps()")
				}
			}
			
			#put observations back in the right order
			new.all.dat2 = new.all.dat[order(new.all.dat[,rank.col-1]),]
			
			warning("Proportions may not be correct - Source code may need to be modified to handle this situaion")
			return(new.all.dat2[,1])
		}
	}
	
	
	
	if (exists(".accuracy")) {accuracy<-.accuracy}
	
	if (is.null(desired_props) || any(is.na(desired_props))) {
		#no props, silently do nothing	  
		return(default.vec)
	}
	
	if (is.null(default.vec)) {
		stop(gettextf("%s is NULL", as.character(sys.call())[2]))
	}
	
	#keep original vector
	orig.default.vec <- default.vec
	#n is the number of children in one year
	n <- length(orig.default.vec)
	type <- is(orig.default.vec)[1] 
	
	dim(propens)
	dim(orig.default.vec)
	if (!is.null(propens) 
			&& ( 
				(length(dim(propens)) != 2 && length(orig.default.vec) != length(propens))
				|| (length(dim(propens)) == 2 && length(orig.default.vec) != dim(propens)[1])
				) ) {
		firstParamName <- as.character(sys.call())[2]
		stop(gettextf("Propensities must be the same length as %s ",firstParamName))
	}
	
	#check that the number of categories in the provided vector of data 
	#(default.vec) is the same number of categories as given in desired_props
	# If not, add the missing categories to the provided vector.
	num.cat <- length(table(orig.default.vec))
	num.cat2 <- length(desired_props)
	if (num.cat!=num.cat2) {
		note2 = cat("Note: Length of desired_props not equal to number of categories in variable:", 
				"\n", "Assumed that there were unobserved categories in the variable", 
				"\n")
	}
	
	
	
	#if propensity scores not provided then create them
	if (length(propens)==0 || any(is.na(propens))) {
		
		note2 <- "Note: No propensity scores available so random propensity scores were created\n"
		cat(note2)
		
		#col.num is the number of vectors of propensity scores required.
		#E.g. for a 2-category varaible, 1 vector of propensity scores is required
		#and for a 3-category variable, 2 vectors of propensity scores are
		#required etc.
		col.num <- num.cat2 - 1
		
		#create random propensity score
		propens.score <- NULL
		for (i in 1:col.num) {
			propens.score <- c(propens.score, runif(n))
		}
		propens <- matrix(propens.score, ncol=col.num, nrow=n)
	}
	
	convertCategoryName()
	
	#match propensity scores to children
	#(this function assumed that default.vec and propens have the same children
	#in the same order) 
	#the last column is a child identifier so I can put them back in the right
	#order
	all.dat <- data.frame(default.vec, propens, 1:n)
	new.all.dat <- all.dat
	
	#rank.col identifies the column that the rankings will be in
	#this is used later in the change.cat function where the propensities are converted to ranks
	rank.col <- ncol(all.dat) + 1
	
	#create table of given data and calculate current proportions and the numbers 
	#that need moving into or out of categories to get the requested proportions 
	#(n.change)
	tab <- table(new.all.dat[,1])
	cats <- c(1:length(desired_props))
	#if any categories in desired_props are not present in default.vec then this merge is needed to fix the 
	#problem
	tab.df <- merge(cats, tab, by = 1, all=TRUE)
	#after the merge any categories in cat that were not present in tab appear as NAs
	#these NAS are changed to 0s
	na.id <- which(is.na(tab.df$Freq))
	tab.df$Freq[na.id] <- 0
	tab.df$x <- 1:nrow(tab.df)
	current.props <- tab.df$Freq/sum(tab.df$Freq)
	n.change <- round(current.props*n) - round(desired_props*n)
	#(e.g n.change[1] is the excess/deficient number of observations in the first 
	#category in observed data)
	
	num <- 1
	i <- 1 #i = current category
	while (i < length(desired_props)) {
		if (n.change[i]==0) {
			#if no change needs to be made for category i then move onto next category
			i <- i + 1
			num <- 1 
		} else if (sign(n.change[i])==1) {
			#category i has too many obs - give to a higher category
			num <- 1
			new.all.dat <- change.cat(num, rank.col, i, new.all.dat, n.change)
			i <- i + 1
		} else if (sign(n.change[i])== -1) {
			#category i has too few obs - steal from a higher category
			new.all.dat <- change.cat(num, rank.col, i, new.all.dat, n.change)
			num <- num + 1
			
			#the maximum value of num is the number of categories above to steal 
			#from
			#by incrementing i here we ensure that we don't get into an infinite 
			#loop due to the while condition never being satisfied
			if (num>(length(desired_props)-(i-1))) {
				i <- i + 1
			}
		}
		#at this point changes have been made for one iteration of category i (may need more than 
		#one iteration if couldn't steal enough observations from the category  
		#immediately above it
		#create table of current data (tab.df) and calculate numbers that need
		#moving into or out of categories to get the requested proportions 
		#(n.change)
		tab <- table(new.all.dat[,1])
		cats <- c(1:length(desired_props))
		tab.df <- merge(cats, tab, by = 1, all.x=TRUE)
		na.id <- which(is.na(tab.df$Freq))
		tab.df$Freq[na.id] <- 0
		tab.df$x <- 1:nrow(tab.df)
		current.props <- tab.df$Freq/sum(tab.df$Freq)
		n.change <- round(current.props*n) - round(desired_props*n)
	} 
	
	
	checking()
	
}



