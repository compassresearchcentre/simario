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
#' @seealso This function is called by the function \code{\link{modifyProps}}
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
	dat.ch.cat = cbind(dat.ch.cat, rank(dat.ch.cat[,i+1+(num-1)], 
					ties.method="random"))
	if (steal==F) {
		#calculate cut-off for deciding which ones to change to a higher value
		cut.off = nrow(dat.ch.cat) - n.change[i] + 1
		#change those that are above the cut-off to have a higher value
		dat.ch.cat[,1][dat.ch.cat[,rank.col]>=cut.off] <- (i+1)
	}
	if (steal==T) {
		#calculate cut-off for deciding which ones to change to a lower value
		cut.off = n.change[i]*(-1)
		#change those that are below the cut-off to have a lower value
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
#' @param default.vec
#'  a vector after a run of the simulation. The values of this
#'  variable will be changed in accordance with what the user requests
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
#' @seealso This function calls \code{\link{change.cat}}
#' 
#' @examples
#' \dontrun{
#' default.vec <- children$SESBTH
#' props <- c(0.1,0.1,0.8)
#' propens <- data.frame(propensities$SESBTH)
#' new.vec <- modifyProps(default.vec, props, propens)
#' table(default.vec)/sum(table(default.vec))
#' table(new.vec)/sum(table(new.vec))
#' 
#' default.vec <- env.scenario$simframe$z1accomLvl1
#' props <- c(0.1,0.9)
#' propens <- propensities$z1accom[,,5]
#' #propensities$z1accom is a 3 dimentional array so we take only the the 5th z dimension
#' 	#(the propensities for year 5)
#'  table(default.vec)/sum(table(default.vec))
#' new.vec <- modifyProps(default.vec, props, propens)
#'  table(new.vec)/sum(table(new.vec))
#' 
#' default.vec <- env.scenario$simframe$catpregsmk2
#' props <- c(0.1, 0.1, 0.1, 0.5, 0.2)
#' propens <- NULL
#' 
#' prop.table(table(default.vec))
#' prop.table(table(modifyProps(default.vec, props, propens)))
#' }
modifyProps <- function(default.vec, props, propens=NULL) {
  if (is.null(props) || any(is.na(props))) {
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
    #(default.vec) is the same number of categories as given in props
  num.cat = length(table(default.vec))
  num.cat2 = length(props)
  if (num.cat!=num.cat2) {
    note2 = cat("Note: Length of props not equal to number of categories in variable:", 
      "\n", "Assumed that there were unobserved categories in the variable", 
      "\n")
  }
  
  #n is the number of children in one year
  n = length(default.vec)
  
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
  
  #if the category names are not consecutive numbers starting at 1 change them 
    #so they are
  tab.names = as.numeric(names(table(default.vec)))
  if (sum(tab.names!= 1:length(props))>=1) {
    default.vec2 = numeric(length(default.vec))
     for (i in 1:length(props)) {
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
  cats <- c(1:length(props))
  #if any categories in prop are not present in default.vec then this merge is needed to fix the 
	#problem
  tab.df = merge(cats, tab, by = 1, all.x=TRUE)
  #after the merge any categories in cat that were not present in tab appear as NAs
	#these NAS are changed to 0s
  na.id = which(is.na(tab.df$Freq))
  tab.df$Freq[na.id] <- 0
  current.props = tab.df$Freq/sum(tab.df$Freq)
  n.change = round(current.props*n) - round(props*n)
    #(e.g n.change[1] is the excess/deficient number of observations in the first 
      #category in observed data)
  num = 1
  i = 1
  while (i < length(props)) {
    if (n.change[i]==0) {
      #if no change needs to be made for category i then move onto next category
      i = i + 1
      num =1 
    } else if (sign(n.change[i])==1) {
        #category i has too many obs - give to a higer category
        num = 1
        new.all.dat = change.cat(num, rank.col, i, new.all.dat, n.change)
        i = i + 1
    } else if (sign(n.change[i])== -1) {
        #category i has too few obs - steal from a higer category
        new.all.dat = change.cat(num, rank.col, i, new.all.dat, n.change)
        num = num + 1
        
        #the maximum value of num is the number of categories above to steal 
          #from
        #by incrementing i here we ensure that we don't get into an infinite 
          #loop due to the while condition never being satisfied
        if (num>(length(props)-(i-1))) {
          i = i + 1
        }
      }
    #at this point changes have been made for one iteration of category i (may need more than one 
      #iteration if couldn't steal enough observations from the category 
      #immediately above it
    #create table of current data (tab.df) and calculate numbers that need
      #moving into or out of categories to get the requested proportions 
      #(n.change)
    tab = table(new.all.dat[,1])
    cats <- c(1:length(props))
    tab.df = merge(cats, tab, by = 1, all.x=TRUE)
    na.id = which(is.na(tab.df$Freq))
    tab.df$Freq[na.id] <- 0
    current.props = tab.df$Freq/sum(tab.df$Freq)
    n.change = round(current.props*n) - round(props*n)
  } 
  #check if requested proportions acheived
  if (sum(abs(props - current.props))<=.01) {
    #if correct
    #change back to orignal values (if they were changed earlier)
      #e.g. if the orginal variable was a {0, 1} variable then all 0s would have 
        #been changed to 1s and alls 1s changed to 2s.  At this step, after the 
        #changes to get the right proportions, the 1s are changed back to 0s and 
        #the 2s are changed back to 1s.
    if (sum(tab.names!= 1:length(props))>=1) {
      default.vec2 = numeric(nrow(new.all.dat))
      for (i in 1:length(props)) {
        default.vec2[new.all.dat[,1]==i] <- tab.names[i]
      }
      new.all.dat[,1] = default.vec2
    }
  
    #and put children back in the right order
    new.all.dat2 = new.all.dat[order(new.all.dat[,rank.col-1]),]
    
    return(new.all.dat2[,1])
    
  } else if (sum(abs(props - current.props))>.01) {
      #if not correct - still do these things but give output with warning
      #(output should all be correct if I have thought of everything and made no 
        #mistakes)
      #change back to orignal values
      if (sum(tab.names!= 1:length(props))>=1) {
        default.vec2 = numeric(nrow(new.all.dat))
        for (i in 1:length(props)) {
          default.vec2[new.all.dat[,1]==i] <- tab.names[i]
        }
        new.all.dat[,1] = default.vec2
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

cat("Loaded modifyProps.r\n")
