# Common functions for analysing data in microsimulation models.
# 
# Author: Oliver Mannion
###############################################################################

ROW <- 1
COL <- 2
ZDIM <- 3


addRowPercents <- function (counts) {
	#adds row percentages to a set of counts
	#eg: addRowPercents(yearlyFreq(children$sol, "sol"))
	pcents <- prop.table(counts,1) * 100
	
	# add (%) to the end of column headings
	dimnames(pcents)[[2]] <- sapply(dimnames(pcents)[[2]], paste, "(%)")
	
	combined <- cbind(counts, pcents)
	names(dimnames(combined)) <- names(dimnames(counts))
	
	#keep meta attribute
	attr(combined, "meta") <- attr(counts, "meta")
	
	combined
}

#' Add a column totals row, preserving meta and names.
#' 
#' @examples
#' x <- runs.mean.mean$all$gpmorb
#' x <- env.base$years1_5$results$quantiles$kids
#' x <- years1_5$runs$means$all$gpmorb
#' x <- results$means$all$kids
#' addColumnTotals(x)
addColumnTotals <- function (x) {
	#rows <- !rownames(x) %in% "All Years"
	#xr <- x[rows,,drop=FALSE]
	#xt <- structure(rbind(x,"Total"=colSums(xr, na.rm=TRUE)), meta=attr(x, "meta"))
	
	xt <- structure(rbind(x,"Total"=colSums(x, na.rm=TRUE)), meta=attr(x, "meta"))
	names(dimnames(xt)) <- names(dimnames(x))
	xt
}

#' Produces a csv string from x, returned as a length 1 chr vector.
#' 
#' @param title
#'  optional title to prepend to output
#' 
#' @examples
#' x <- matrix(c(2,3,4,5,6,7,8,9),nrow=2)
#' cat(as.csv.string(x))
#' cat(as.csv.string(x,"title"))	#title <- "title"
#' str(as.csv.string(x))
#' length(as.csv.string(x))
#' 
#' cat(as.csv.string(t.runs.mean.cfreq.prop.base$o.gpprev, "o.gpprev")) 
as.csv.string <- function(x, title = NULL, row.names = T) {
	if (!(is.null(title))) title <- dQuote(title) 
	
	if (is.vector(x)) {
		#print as one line
		result <- paste(x,collapse=",")
	} else {
		result <- paste(c(title,capture.output(write.csv(x, row.names=row.names))),sep="",collapse="\n")
	}
	
	#add ending newline
	paste(result,"\n")
}

as.csv.string.list <- function(xlist, row.names = T) {
	#apply to a list using the names from the list
	#eg: cat(as.csv.string.list(t.runs.mean.cfreq.base))
	mapply(function (x, name) {
						as.csv.string(x,name, row.names = row.names)
					}, xlist, names(xlist))
}


#' Stops and displays indices of any element that is FALSE.
#' 
#' @param xvec
#'  logical vector
#' @examples
#' assert(c(T,T))
#' assert(c(T,F))
assert <- function(xvec) {
	if (!all(xvec)) {
		if (is.null(names(xvec))) names(xvec) <- paste("[",seq(length(xvec)),"]",sep="")
		#firstParamName <- as.character(sys.call())[2]
		stop(gettextf("failed for %s",  
						paste(names(xvec[!xvec]),collapse=", ")))
	}
}

#' divides x into bins specified by breaks or a bin size
#' 
#' @param x numeric vector to be binned
#' @param breaks either a numeric vector of two or more cut 
#' points, or a single number giving the size of the bins in
#' which case x is divided in bins of size breaks.
#' NB: if cut points are specified, note that the cut point
#' value is not included in the bin (ie: include.lowest = FALSE)
#' Therefore the very first cut point must be less than min(x)
#' @param blabels labels for the levels of the resulting category. 
#' If NULL, labels are constructed using "(a,b]" interval notation.
#' If specified, NAs are removed first.
#' If unspecified, names(breaks) is used.
#' @param breaklast if breaks is a bin size and breaklast is
#' specified then this is the position of the last break
#' @return the values of x factored into bins 
#' 
#' @examples 
#' table(bin(env.scenario$simframe$bwkg, binbreaks$bwkg)) 
#' table(bin(children$bwkg, 0.5)) # x <- children$bwkg; breaks <- 0.5
#' table(bin(children$ga, 1, breaklast=37)) #x <- children$ga; breaks = 1; breaklast = 37
#' table(bin(children$pregalc,1,breaklast=7)) #x <- children$pregalc; breaks = 1; breaklast = 7
#'
#' binbreaks <- list()
#' binbreaks$pregalc <- c(-1,0,1,2,3,4,5,6,7,max(children$pregalc))
#' names(binbreaks$pregalc) <- c(NA, 0:7,"8+")
#' table(bin(children$pregalc,binbreaks$pregalc))  x <- children$pregalc;breaks<-binbreaks$pregalc 
#' 
#' binbreaks$bwgrams <- c(0,2499,2999,3499,3999,max(children$bwgrams))
#' names(binbreaks$bwgrams) <- c(NA, "< 2500", "2500 - 2999", "3000 - 3499", "3500 - 3999", "4000 + ") 
#' table(bin(children$bwgrams,binbreaks$bwgrams))
#' 
#' binbreaks$ga <- c(0,34,35,36,37,max(children$ga))
#' names(binbreaks$ga) <- c(NA, "< 35", "35", "36", "37", "38+")
#' table(bin(children$ga,binbreaks$ga))
#' 
#' breaks <- binbreaks$INTERACT
#' blabels <- names(breaks)
#' breaklast=NULL
#' table(bin(x, breaks, breaklast=NULL), useNA="always")
bin <- function (x, breaks, blabels = names(breaks), breaklast=NULL) {
	if (length(breaks) == 1L) {
		# breaks is a binsize. calculate actual breaks
		binsize <- breaks
		
		breakmin <- (floor(min(x,na.rm=TRUE)/binsize)*binsize)
		
		# TODO: comparing doubles here, use all.equal instead?
		# depends what cut uses to compare I guess?
		if (min(x) == breakmin) {
			#min(x) == breakmin, so go down one binsize
			breakmin <- breakmin-binsize
		}
		
		breakmax <- ceiling(max(x,na.rm=TRUE)/binsize)*binsize
		
		if (is.null(breaklast)) {
			breaks <- seq(breakmin,breakmax,binsize)
		} else {
			breaks <- c(seq(breakmin,breaklast,binsize),breakmax)
		} 
	}
	if (!is.null(blabels)) blabels <- na.omit(blabels)
		
	cut(x, breaks=breaks, labels=blabels, include.lowest = FALSE)
}

#' Combine levels specified in seperate binary level variables into one
#' variable.
#' 
#' @param ...
#'  a series of binary (0,1) vectors, specified as multiple arguments
#'  or as a list
#' @param levelvalues
#'  a vector indicating the level values to use. The first value of this vector is the value used
#'  for the first binary vector argument, the second  for the second binary vector argument etc.
#'  If unspecified, the defaults to (1,2,...n) where n is the number of binary vectors
#'  specified by ... 
#' 
#' @return
#'  a single vector
#' 
#' @examples 
#' binlevel1 <- c(1,0,0)
#' binlevel2 <- c(0,1,0)
#' binlevel3 <- c(0,0,1)
#' levelvalues <- c(1,2,3)
#' binary.levels.combine(binlevel1, binlevel2, binlevel3)
#' [1] 1 2 3
#' binary.levels.combine(list(binlevel1, binlevel2, binlevel3))
#' [1] 1 2 3
#' 
#' level1 <- env.scenario$simframe$SESBTHLvl1
#' level2 <- env.scenario$simframe$SESBTHLvl2
#' level3 <- env.scenario$simframe$SESBTHLvl3
#' 
#' binary.levels.combine(level1, level2, level3)
#' binary.levels.combine(list(level1, level2, level3))
#' levelvalues <- c(1,2,3)
#' binlevels <- list(level1, level2, level3)
#' binlevels <- simframe[c("z1chparLvl0", "z1chparLvl1")] ; levelvalues = c(0,1)
#' binary.levels.combine (binlevels, levelvalues = levelvalues)
binary.levels.combine  <- function (..., levelvalues = NULL) {
	
	binlevels <- if (is.list(c(...))) c(...) else list(...)
	
	if (is.null(levelvalues)) levelvalues <- seq(length(binlevels))
	
	mbinlevels <- as.matrixFromList(binlevels)
	
	#check we have one and only one binary value across each of the levels 
	if (any(colSums(mbinlevels) != 1)) {
		stop (c("invalid bin levels in position(s): ", paste(which(colSums(mbinlevels) != 1), collapse = ", ")))	
	}
	
	# multiply the binary levels by the corresponding level value
	binlevels.x.levelvalues <-  mbinlevels * levelvalues 
	
	# flatten into a single vector
	colSums(binlevels.x.levelvalues)
}


#' Create a list of binary levels from factors.
#' Compares a vector x against a set of factors and returns
#' a list for each factor which indicates whether x 
#' is equal to that factor or not.
#' 
#' @param x
#'  vector
#' @param f
#'  factors. Defaults to the unique set of values in x.
#' @return 
#'  list of binary levels
#' @examples
#' x <- c(1,2,3,2,1)
#' f <- c(1,2,3)
#' 
#' binary.levels.split (x, f):
#' 
#' $`1`
#' [1] 1 0 0 0 1
#' 
#' $`2`
#' [1] 0 1 0 1 0
#' 
#' $`3`
#' [1] 0 0 1 0 0
#' 
#' x <- adjcatvar
#' f=sort(unique(x))
#' binary.levels.split(x)
binary.levels.split <- function(x, f=sort(unique(x))) {
	if (is.null(names(f))) names(f) <- f 
	lapply(f, function(fac) { as.integer(x == fac) } )
}

#' increments x by factor
#' 
#' @param x numeric vector to be incremented
#' @param factoredx x factored
#' @param factorincrements amounts to increment each factor of factoredx by
#' @return x incremented by the factor increments
#' 
#' @examples
#' x <- children$bwkg
#' xi <- incByFactor(children$bwkg, bin(children$bwkg, 0.5), c(0,0.5,0,0,0,0,0,0,0,0))
#' table(bin(x, 0.5))
#' table(bin(xi, 0.5))
#' 
#' 
#' x <- env.scenario$simframe$INTERACT
#' factoredx <- bin(x, binbreaks$INTERACT, breaklast=NULL)
#' factorincrements <- rep(0,9)
#' r <- incByFactor(x, factoredx, factorincrements)
#' table(r, useNA="always")
incByFactor <- function(x, factoredx, factorincrements) {
	if (nlevels(factoredx) != length(factorincrements)) {
		stop("factor increments must be of length ", nlevels(factoredx))
	}
	
	# add factor increment to x based on its factoring (factoredx)
	x + factorincrements[factoredx]
}


#' Check a list for any NAs, producing error if they exist, otherwise
#' silently exit.
#' 
#' @examples
#' 	foo <- c(1,NA,3)
#'  names(foo) <- c("a","b","c")
#'  xlist <- foo
#' 	checkNAs(foo)
#' 	checkNAs(mpropens)
checkNAs <- function (xlist) {

	nas <- sapply(xlist, function(x) { any(is.na(x)) })
	
	if (any(nas)) {
		if (is.null(names(nas))) names(nas) <- paste("[",seq(length(nas)),"]",sep="")
		firstParamName <- as.character(sys.call())[2]
		stop(gettextf("NAs in %s for %s", firstParamName, 
						paste(names(nas[nas]),collapse=", ")))
	}
}

#' Detach returning the modified attached environment.
#' NB: the returned environment contains the modified
#'     contents of the attached environment but is
#'     actually a newly created different environment
#'     object from the original. 
detachReturn <- function(envname) {
	#store modified env
	#env <- updatelist(env, as.list(as.environment(envname)))
	#env <- as.list(as.environment(envname))
	
	# gets the attached environment specified by envname
	# and returns it. Because an attached environment is
	# a copy of the original (see ?attach), this will return 
	# a different environment than the original
	# TODO: use proto or R.oo so we can avoid this problem
	# and have functions modify the original environment
	# rather than an attached copy 
	env <- as.environment(envname)
	
	#detach
	detach(envname, character.only = TRUE)
	
	#return modified env
	env
}

err <- function (x) {
	## calc the 95% error from the t Distribution
	## see http://www.cyclismo.org/tutorial/R/confidence.html
	## used by meanOfRuns
	if (length(x) < 2) {
		return(NA)
	}
	qt(0.975,df=length(x)-1)*sd(x)/sqrt(length(x))
}

#' Evaluate a list/vector of strings as expressions.
#' Errors if expressions cannot be evaluated.
#' 
#' @param exprlist list/vector of strings to evaluate
#' @param enviro environment to evaluate in, defaults to global environment.
#' @param allowEmptyExpr allow expressions to return no value, defaults to FALSE
#' 
#' @examples
#' exprlist <- initial_value
#' envir <- bf
#' eval.list(exprlist, bf)
eval.list <- function (exprlist, envir = .GlobalEnv, allowEmptyExpr = FALSE) {
	
	#evaluate list of strings as expressions.
	#Any objects in the exprs must exist in envir or if not then in parent.frame()
	values <- lapply(exprlist,
			function (x) try(eval(parse(text=x), envir = envir), silent = TRUE))
	
	# if any parse/evaluation errors, fail
	tryerrs <- tryerrorMsgs(values)
	if (length(tryerrs) > 0) {
		firstParamName <- as.character(sys.call())[2]
		# concate errors together with names 
		msg <- paste(firstParamName, " ", names(tryerrs), ": ",tryerrs, sep="",collapse="")
		stop(msg)
	}
 	
	emptyexprs <- lapply(values,length)==0
	if (!allowEmptyExpr && any(emptyexprs)) {
		# expr evaluated to empty value, fail with error message
		firstParamName <- as.character(sys.call())[2]
		msg <- paste(firstParamName, " ",
				names(emptyexprs)[emptyexprs],
				" : expr '",
				exprlist[emptyexprs],
				"' returns no value\n",
				sep="",collapse="")
		stop(msg)
	}
	
	values
}


factorWeights <- function (xvecfactors, desiredProp) {
	#generates a weighting for each factor in xvecfactors such that 
	#propWtdtable(xvecfactors, factorWeights(xvecfactors, desiredProp)) == desiredProp
	#
	#eg: xvecfactors = children$SESBTH
	#eg: desiredProp <- c(0.2,0.3,0.5)
	#eg: factorWeights(children$SESBTH,c(0.2,0.3,0.5)) 
	
	# get existing proportions of factor levels
	baseProp <- prop.table(table(xvecfactors))
	
	# weight for each factor level is desiredProp/baseProp
	weight <- desiredProp/baseProp
	
	# get position of each factor level (each xvecfactors) in weight names
	weightPos <- match(xvecfactors, names(weight))
	
	# get weight for xvecfactors
	result <- sapply(weightPos, function (x) weight[x], USE.NAMES = FALSE )
	names(result) <- NULL
	result
}




freq <- function(variable, varname) {
	# frequency table with percent
	# v = variable
	#
	# eg: freq(a(children$z1msmokeLvl1)[,1], "msmoke")
	# eg: freq(a(children$z1msmokeLvl1)[,2], "msmoke")
	tbl <- as.data.frame( table(variable, dnn = varname), responseName = "Frequency")
	tbl$Percent <- prop.table(tbl$Frequency) * 100
	tbl$"Cumulative Percent" <- cumsum (tbl$Percent) 
	tbl
}

globalNamed <- function (varname, x, pos = 1) {
	#save x into global variable, ie: top frame, not just this function
	#using the supplied varname
	assign(varname, x, pos = pos)	
}

global <- function (x, pos = 1) {
	#save x into global variable as it's own name
	param1Name <- as.character(sys.call())[2]
	globalNamed(param1Name, x, pos)
}


#' Returns whether x is a scalar (i.e. length 1)
#' and numeric
is.numeric.scalar <- function (x) {
	length(x) == 1 && is.numeric(x)
}

labelColFromVec <- function (xlist, colnameslist) {
	# set the columns names of each x in xlist to
	# each char vector c in colnameslist
	labelCol <- function(x, colnames) {dimnames(x)[[COL]] <- colnames;x}
	mapply(labelCol,
			xlist, colnameslist, SIMPLIFY = FALSE)
}	

labelColTitleFromList <- function(xnamedlist) {
	# names each object's column title with the 
	# object's name in the named list
	labelCol <- function(x,xname) { names(dimnames(x))[COL] <- c(xname);x }
	mapply(labelCol, xnamedlist, names(xnamedlist), SIMPLIFY = FALSE)
}


#' Set the first column to label.
#' 
#' @param x
#'  object with columns
#' @param onlyIfNull
#'  if TRUE, then naming will only occued if x 
#'  has no existing colnames
#' 
#' @examples
#' xnamedlist <- env.base$years1_5$results$means$all
#' xnamedlist <- results$means$all
#' x <- xnamedlist$gptotvis
#' labelCol1FromList(x, "Mean")
labelCol1 <- function(x, label, onlyIfNull = TRUE) {
	if (!onlyIfNull || is.null(colnames(x))) {
		colnames(x) <- label
	}
	x
}

#' Set name of each object's 1st column to the
#' object's name in the named list.
#' 
#' @param xlist
#'  list
#' @param xlabels
#'  labels, defaults to names(xlist)
#'  if length(xlist) > length(labels) then the labels are repeated
#' @param onlyIfNull
#'  if TRUE, then naming will only occur if the xlist item 
#'  has no existing colnames
#' 
#' @examples
#' xlist <- env.base$years1_5$results$means$all
#' xlist <- results$means$all
#' x <- xlist$gptotvis
#' labelCol1.list(xlist, "Mean")
labelCol1.list <- function(xlist, xlabels = names(xlist), onlyIfNull = TRUE) {
	mapply(labelCol1, xlist, xlabels, MoreArgs = list(onlyIfNull = onlyIfNull), SIMPLIFY = FALSE)
}

labelTitle <- function (xm, along, title) {
	# names the "along" dimension with "title"
	# eg: labelTitle(y, ROW, "Year") 
	names(dimnames(xm))[[along]] <- title
	xm
}

labelprefixseq <- function(xm, along, prefix) {
	# names the "along" dimension of xm with a prefixed sequence
	# eg: labelColWithSeq(y, COL, "Run") #COL will be labelled "Run 1", "Run 2"..etc.

	dnames <- dimnames(xm)
	if (is.null(dnames)) {
		dnames <- list()
	}
	
	dnames[[along]] <- paste(prefix, seq(dim(xm)[along]))
	dimnames(xm) <- dnames
	
	xm
}

labelseq <- function(xm, along, title) {
	# names the "along" dimension of xm with a sequence
	# and give that dimension the name "title"
	# eg: labelseq(y, ROW, "Year") 
	dimnames(xm)[[along]] <- seq(dim(xm)[along])
	names(dimnames(xm))[[along]] <- title
	xm
}


#' Return amount of memory, in bytes, used by each
#' element of a list
#' @param lx
#'  list
#' @return
#'  num vector of bytes per element
#' @examples
#' mem.lx(env.base)
#' prettyNum(sum(mem.lx(env.base)), big.mark=",")
#' mem.lx(env.base$years1_5)
mem.lx <- function(lx) {
	sapply(lx, object.size)
}

#' Add a meta attribute. Appends to existing meta attribute, if it exists, 
#' overwriting any meta elements with the same name.
#' 
#' @param x
#'  an object. May have an existing meta attribute.
#' 
#' @param new.meta
#'  a named vector to add to the meta attribute of x.
#'  if an element of new.meta already exists in the x's meta, 
#'  then the existing meta element will be replaced.
#'  if new.meta is NULL, nothing is done.
#'   
#' @return 
#'  x with "meta" attribute
#' 
#' @examples
#' 
#' x <- 1:3
#' x <- structure(1:3, meta=c(grpby.tag="r1stchildethn", set="females only"))
#' new.meta <- c(varname="gptotvis")
#' new.meta <- c(grpby.tag="newtag")
#' new.meta <- c(grpby.tag=NULL)
#' meta.add(x, new.meta)
#' meta.add(x, new.meta)
meta.add <- function(x, new.meta) {
	if (is.null(new.meta)) return(x)
	
	meta <- attr(x, "meta")
	keep.meta <- setdiff(names(meta), names(new.meta))
	
	result.meta <- c(meta[keep.meta], new.meta)
	
	structure(x, meta=result.meta)
}

#' Appends a "varname" meta element to the meta of each element
#' in a list.
#' 
#' @param xlist
#'  list of objects
#' @param varnames
#'  parallel vector of varnames. Defaults to names(xlist).
#'  If NULL, does nothing
#' 
#' @return 
#'  xlist with the corresponding varname append to the meta
#'  attribute of each xlist element.
#' 
#' @examples
#' xlist <- list(gpmorb = 1:3, hadmtot = 10:13)
#' xlist <- list(1:3, 10:13)
#' varnames <- names(xlist)
#' 
#' meta.add.list.varname(xlist, varnames) 
meta.add.list.varname <- function(xlist, varnames = names(xlist)) {
	if (is.null(varnames)) {
		xlist
	} else {
		mapply(function(x, varname) {
					meta.add(x, c(varname=varname))
					#lapply(x, `attr<-`, "varname.tag", varname)
				}, xlist, varnames, SIMPLIFY = FALSE)
	}
} 


#' Sort numerically. 
#' If x can be represented numerically, then
#' sort numerically otherwise sort normally. 
#' 
#' @param x
#'  object to sort
#' @param stripAlpha
#'  remove alpha characters before attempting sort
#' 
#' @examples
#'  
#' x <- c("1 (%)", "-2 (%)", "2 (%)", "10 (%)")
#' x <- c("1 (%)", "0 (%)", "2 (%)", "10 (%)")
#' x <- c("1","0","2","10")
#' x <- c("b","d","a","c")
#' x <- c(1,4,3,2)
#' x <- c("1", "2", "3", "4", "5", "6", "7", "8", "10", NA, "9", NA)
#' nsort(x)
nsort <- function (x, stripAlpha = TRUE, ...) {

	# remove any NAs
	index.nas <- is.na(x)
	if (any(index.nas)) {
		x <- x[!index.nas]
	}
	
	if (stripAlpha) xs <- strip.alpha(x) else xs <- x
	
	# if can be converted via as.numeric without NAs, convert
	if (!any(is.na(suppressWarnings(as.numeric(xs))))) {
		result <- x[order(as.numeric(xs))]
	} else {
		result <- sort(x, ...)	
	} 
	
	# add back any NAs to end
	if (any(index.nas)) {
		result <- c(result, rep(NA, sum(index.nas)))	
	}
	
	result
}


orderByDictLookup <- function (...) {
	# order the list by the results returned applying
	# dictLookup to the list's elements
	xlist <- c(...)
	ordering <- sort.list(sapply(xlist, dictLookup))
	xlist[ordering]
}

prependRowMeanInfo <- function (xm) {
	#prepend mean, err, left & right CI to each row of matrix 
	#
	#input: a matrix/dataframe containing columns of values
	#       to calculate mean info for, eg:
	#
	#          Run 1        Run 2
	#1  0.0039392527 4.189704e-03
	#2  0.0052892006 5.554406e-03
	#3  0.0500477200 4.921984e-02
	#4  0.0061327012 6.273054e-03
	#
	#output: the original values plus the additional variables: 
	#Mean, Err, Left, Right prepended to the start of each row
	#
	#eg: xm <- ymo.gptotvis
	#eg: prependRowMeanInfo(ymo.gptotvis)
	
	#calculate mean of each row
	meanRuns <- apply(xm,ROW,mean)
	
	#calculate error of each row
	errRuns <- apply(xm,ROW,err)
	
	#calculate left CI
	leftRuns <- meanRuns - errRuns
	
	#calculate right CI
	rightRuns <- meanRuns + errRuns
	
	#return with mean, error, and confidence intervals prepended
	result <- cbind(Mean = meanRuns, Err = errRuns, 
			Left = leftRuns, Right = rightRuns, xm)
	
	names(dimnames(result)) <- names(dimnames(xm))
	
	#keep meta attribute
	attr(result, "meta") <- attr(xm, "meta")
	
	result
}



readXLSSheet1 <- function (filedir, filename) {
	#returns first sheet of XLS as dataframe
	oldOpt <- options(stringsAsFactors = FALSE)
	on.exit(options(oldOpt))
	read.xlsx2(paste(filedir, filename, sep=""), sheetIndex = 1)
} 

removeObs <- function(xframe, indices) {
	#remove observations (ie. rows) of 
	#xframe specified by indices
	#eg: DF <- data.frame(x = c(1, 2, 3), y = c(0, 10, NA))
	#removeObs(DF, c(1,3))
	
	#create inverted logical array of nas
	invlogi <- rep(TRUE, dim(xframe)[1])
	invlogi[indices] = FALSE
	
	xframe[invlogi, ]
}

#' Remove all objects in global environment.
#' 
#' @param exceptions
#' 	names of vars not to remove
#' @examples
#' 
#' exceptions <- c("ov")
#' rmall(exceptions)
rmall <- function (exceptions = NULL) {
	vars <- ls(".GlobalEnv", all.names=TRUE)
	if (!is.null(exceptions)) {
		vars <- vars[!vars %in% exceptions]
	}
	rm(pos = ".GlobalEnv", list = vars)
}


stripEmpty <- function (xvec) {
	#remove empty values (NAs, empty string) from vector
	xvec <- xvec[!is.na(xvec)]	#remove NAs
	xvec <- xvec[xvec != ""]		#remove empty strings
	xvec
}

stripClass <- function (x) {
	#remove the class attribute
	`attr<-`(x, "class", NULL)
}

#' Remove meta attribute from an object
#' 
#' @param x
#'  object
#' @return
#'  x without "meta" attribute
stripMeta <- function (x) {
	`attr<-`(x, "meta", NULL)
}

#' Remove meta attribute from a list
#' 
#' @param xlist
#'  list
#' @return
#'  xlist with elements without a "meta" attribute
stripMeta.list <- function (xlist) {
	lapply(xlist, stripMeta)	
}

#' Execute summary on the columns of a matrix.
#' 
#' @param mx
#'  matrix
#' 
#' @return
#'  summary of each column, returned as a row
#' 
#' @examples
#' 
#' mxc <- env.base$years6_13$outcomes[["cond"]]
#' mx <- env.base$years1_5$outcomes[["gptotvis"]]
#' summary.mx(mxc)
#' summary.mx(mx)
summary.mx <- function (mx) {
	sm <- apply(mx, COL, summary)
	
	if(is.list(sm)) {
		# a list was returned which means 
		# we have vectors of different lengths.
		# this is because some have an NA's column
		# and some not.
		#
		# add "NA's" value if it doesn't exist,
		# simplify to matrix,
		# and rotate so iterations are rows
		t(sapply(sm, function(s) {
							if(is.na(s["NA's"])) c(s, "NA's"=0) else s
						}))
	} else {
		# add NA's row if it doesn't exist
		if (!("NA's" %in% rownames(sm))) {
			sm <- rbind(sm, "NA's" = 0)	
		}
		t(sm)
	}
}

#' Convience method to apply summary.mx to a subset of elements of 
#' list.
#' 
#' @param xlist
#'  a list of matrices
#' @param indices
#'  indices, either numeric or names, of elements in x for which to apply summary.mx,
#'  or NULL to apply to all elements of x
#' @return 
#'  list of summaries
#' 
#' @examples
#' 
#' xlist <- env.base$years6_13$outcomes
#' indices <- names(env.base$years6_13$runs$means$all)
#' summary.mx.list(xlist, indices) 
summary.mx.list <- function (xlist, indices) {
	lapply.subset(xlist, indices, summary.mx)
}


tryerrorMsgs <- function (xlist) {
	# return the messages of any element that is a try-error 
	unlist(sapply(xlist, function (x) if (class(x)=="try-error") { stripClass(x) }))
}


updateScenarioWeights <- function(bf, varnamefactor, desiredProp) {
	#return the bf with an updated weightScenario variable
	#based on the factor "varnamefactor" and the desired proportions
	#"desiredProp"
	#eg: children <- updateScenarioWeights(children, "SESBTH", c(0.2,0.3,0.5))
	#prop.table(wtdtable(children$SESBTH, children$weightScenario))
	bf$weightScenario <- factorWeights(bf[[varnamefactor]], desiredProp)
	bf
}

#' Same as within, but the expr executed is a function.
#' Unlike within, the func will be executed within data.
#' Functions will read variables from data, and if <<- is used 
#' then values will be assigned in data IF the variable already exists in data.
#' 
#' @param data data to use for constructing an environment.  Can be a list or a data frame.
#' @param func function to evaluate in data
#' @param ... arguments to pass to func
#' @return data modified
#' @examples
#' myvar <- "globalenv"
#' data <- list(myvar="dataenv") 
#' func <- function() {
#' 	print(myvar)
#' 	myvar <<- "func.changed"	
#' }
#'
#' args <- list() 
#' data <- withinfunc(data, func)
#' 
#' data <- base
#' func <- setupOutputs
#' args <- list()
withinfunc <- function(data, func, ...) {
	# create environment from base that has
	# parent environment that is the environment 
	# in which within.func was called
	parent <- parent.frame()
	dataenv <- evalq(environment(), data, parent)
	
	# set func environment to dataenv
	environment(func) <- dataenv
	
	args <- list(...) 
	do.call(func, args=args)
	
	# returned modified dataenv
	updatelist(data, as.list(dataenv))
}

library("plyr")

cat("Loaded support functions\n")
