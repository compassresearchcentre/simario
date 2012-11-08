# TODO: Add comment
# 
# Author: jtho139
###############################################################################
#trying to change modifyProps so can use for 


#test <- modifyPropsContinuous(simframe.master$fhrswrk, rep(1/7, 7), catToContModels$fhrswrk, attr(env.scenario$cat.adjustments$fhrswrk, "cont.binbreaks"))
#x.cont = simframe.master$fhrswrk
#desired_props <- rep(1/7, 7)
desired_props <- c(.05, .1, .2, .15, .3, .12, .08)
#propens=NULL
#accuracy=.01
#envir=simframe.master
#fhrs.binbreaks = attr(env.scenario$cat.adjustments$fhrswrk, "cont.binbreaks")
#x.cat <- bin(x.cont,fhrs.binbreaks)
#default.vec = x.cat
#adj.x.cat <- modifyProps(x.cat, desired_props, propens, accuracy)

#check
# x.cat2 <- bin(adj.x.cont, fhrs.binbreaks)
# table(x.cat2)

##tab.names = as.integer(sort(unique(default.vec)))

modifyProps <- function(default.vec, desired_props, propens=NULL, accuracy=.01) {
	
	if (exists(".accuracy")) {accuracy<-.accuracy}
	
	if (is.null(desired_props) || any(is.na(desired_props))) {
		#no props, silently do nothing	  
		return(default.vec)
	}
	
	if (is.null(default.vec)) {
		stop(gettextf("%s is NULL", as.character(sys.call())[2]))
	}
	
	dim(propens)
	dim(default.vec)
	if (!is.null(propens) 
			&& ( 
				(length(dim(propens)) != 2 && length(default.vec) != length(propens))
				|| (length(dim(propens)) == 2 && length(default.vec) != dim(propens)[1])
				) ) {
		firstParamName <- as.character(sys.call())[2]
		stop(gettextf("Propensities must be the same length as %s ",firstParamName))
	}
	
	#check that the number of categories in the provided vector of data 
	#(default.vec) is the same number of categories as given in desired_props
	num.cat = length(table(default.vec))
	num.cat2 = length(desired_props)
	if (num.cat!=num.cat2) {
		note2 = cat("Note: Length of desired_props not equal to number of categories in variable:", 
				"\n", "Assumed that there were unobserved categories in the variable", 
				"\n")
	}
	
	#n is the number of children in one year
	n <- length(default.vec)
	type <- is(default.vec)[1] 
	
	#if propensity scores not provided then create them
	if (length(propens)==0 || any(is.na(propens))) {
		
		note2 = "Note: No propensity scores available so random propensity scores were created\n"
		cat(note2)
		
		#col.num is the number of vectors of propensity scores required.
		#E.g. for a 2-category varaible, 1 vector of propensity scores is required
		#and for a 3-category variable, 2 vectors of propensity scores are
		#required etc.
		col.num = num.cat2 - 1
		
		#create random propensity score
		propens.score = NULL
		for (i in 1:col.num) {
			propens.score = c(propens.score, runif(n))
		}
		propens = matrix(propens.score, ncol=col.num, nrow=n)
	}
	
	#if the the default.vec values are not consecutive numbers starting at 1 change them 
	#so they are
	##tab.names = as.numeric(names(table(default.vec)))
	##tab.names = names(table(default.vec))
	tab.names = sort(unique(default.vec))
	
	if (sum(tab.names!= 1:length(desired_props))>=1) {
		default.vec2 = numeric(length(default.vec))
		for (i in 1:length(desired_props)) {
			default.vec2[default.vec==tab.names[i]] <- i
		}
		default.vec = default.vec2
	}
	
	#match propensity scores to children
	#(this function assumed that default.vec and propens have the same children
	#in the same order) 
	#the last column is a child identifier so I can put them back in the right
	#order
	all.dat = data.frame(default.vec, propens, 1:n)
	new.all.dat = all.dat
	
	#rank.col identifies the column that the rankings will be in
	#this is used later in the change.cat function where the propensities are converted to ranks
	rank.col = ncol(all.dat) + 1
	
	#create table of given data and calculate current proportions and the numbers 
	#that need moving into or out of categories to get the requested proportions 
	#(n.change)
	tab = table(new.all.dat[,1])
	cats <- c(1:length(desired_props))
	#if any categories in desired_props are not present in default.vec then this merge is needed to fix the 
	#problem
	tab.df = merge(cats, tab, by = 1, all.x=TRUE)
	#after the merge any categories in cat that were not present in tab appear as NAs
	#these NAS are changed to 0s
	na.id = which(is.na(tab.df$Freq))
	tab.df$Freq[na.id] <- 0
	current.props = tab.df$Freq/sum(tab.df$Freq)
	n.change = round(current.props*n) - round(desired_props*n)
	#(e.g n.change[1] is the excess/deficient number of observations in the first 
	#category in observed data)
	num = 1
	i = 1 #i = current category
	while (i < length(desired_props)) {
		if (n.change[i]==0) {
			#if no change needs to be made for category i then move onto next category
			i = i + 1
			num =1 
		} else if (sign(n.change[i])==1) {
			#category i has too many obs - give to a higher category
			num = 1
			new.all.dat = change.cat(num, rank.col, i, new.all.dat, n.change)
			i = i + 1
		} else if (sign(n.change[i])== -1) {
			#category i has too few obs - steal from a higher category
			new.all.dat = change.cat(num, rank.col, i, new.all.dat, n.change)
			num = num + 1
			
			#the maximum value of num is the number of categories above to steal 
			#from
			#by incrementing i here we ensure that we don't get into an infinite 
			#loop due to the while condition never being satisfied
			if (num>(length(desired_props)-(i-1))) {
				i = i + 1
			}
		}
		#at this point changes have been made for one iteration of category i (may need more than 
		#one iteration if couldn't steal enough observations from the category  
		#immediately above it
		#create table of current data (tab.df) and calculate numbers that need
		#moving into or out of categories to get the requested proportions 
		#(n.change)
		tab = table(new.all.dat[,1])
		cats <- c(1:length(desired_props))
		tab.df = merge(cats, tab, by = 1, all.x=TRUE)
		na.id = which(is.na(tab.df$Freq))
		tab.df$Freq[na.id] <- 0
		current.props = tab.df$Freq/sum(tab.df$Freq)
		n.change = round(current.props*n) - round(desired_props*n)
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
					default.vec2[new.all.dat[,1]==i] <- levels(tab.names)[i]
					default.vec2 <- factor(default.vec2, levels=levels(tab.names))
				}
				new.all.dat[,1] = default.vec2
			} else if (type=="integer") {
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
					default.vec2[new.all.dat[,1]==i] <- levels(tab.names)[i]
					default.vec2 <- factor(default.vec2, levels=levels(tab.names))
				}
				new.all.dat[,1] = default.vec2
			} else if (type=="integer") {
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

