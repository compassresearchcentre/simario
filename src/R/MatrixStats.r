# Calculations performed on lists and matrices of values.
# 
# Author: oman002
###############################################################################

library(plyr)

colmeans.list <- function (xlistm) {
	# xlistm is a list of matrices
	# calculates the row means of each matrix
	# NAs are treated as zero.
	#
	# eg: colmeans.list(by.year$base$o.gptotvis)
	#	  xlistm <- by.year$base$o.gptotvis
	
	lapply(xlistm, function (cfreq) {
				# replace NAs with 0
				cfreq[is.na(cfreq)] <- 0
				
				# apply mean by col
				apply(cfreq, COL, mean)
			} 
	)
}

#' Mean across Z dimension of a 3D array. Each row and column cell is
#' averaged across the Z dimension.
#' 
#' @param xa
#'  an array with a Z dimension
#' @param CI
#'  if TRUE and xa has more than 1 Z dimension, lower and upper confidence intervals 
#'  are returned in additional columns
#' @param NA.as.zero
#'  if TRUE (default), treat NAs as if they are zeros
#' 
#' @return
#'  a matrix of means across the Z dimension. The "meta" attribute
#'  of xa, if any, is retained.
#' 
#' @export
#' @examples 
#' \dontrun{
#' xa <- env.base$years1_5$runstats$means$all$gpmorb
#' xa <- env.base$years6_13$runstats$means$all$burt
#' xa <- env.base$years1_5$runstats$means$all.by.SESBTH.2cat$gpmorb
#' xa <- years1_5$runstats$means$all.by.gender$gptotvis
#' 
#' xa <- flatten_mxlists_to_array(env.scenario$modules$years6_13$run_results$confreqs[["cond"]])
#' 
#' xa <- env.base$years1_5$runstats$means$all.by.ethnicity$gptotvis
#' xa <- env.base$years1_5$runstats$means$all.by.gender$gptotvis
#' 
#' result <- mean_array_z(xa)
#' }
mean_array_z <- function (xa, CI = TRUE, NA.as.zero = T, re_write_colnames=T) {
	if (NA.as.zero) xa[is.na(xa)] <- 0
	
	result <- apply(xa, c(ROW,COL), mean)
	numZ <- dim(xa)[ZDIM]
	
	# CIs only make sense if more than 1 Z dim
	if (CI && numZ > 1) {
		
		#calculate error of each row
		errZ <- apply(xa,c(ROW,COL),err)
		
		#calculate left CI
		leftZ <- result - errZ
		
		#calculate right CI
		rightZ <- result + errZ
		
		#add left and right Z to the right hand side of result
		resultCI <- cbind(Mean=result,  
				Lower=leftZ, Upper=rightZ)
		#dimnames(result)[[2]] <- c("Mean", "Lower", "Upper")
		
		# reorder so that lower and upper is next to the mean of each grouping
		numGroups <- dim(xa)[COL]
		reordering <- as.vector(sapply(c(1:numGroups), function (x) { seq(from=x, length.out=3, by=numGroups)}))
		resultCI <- resultCI[, reordering]
		
		newsuffix<-rep(c("Mean", "Lower", "Upper"), numGroups)
		
		if(re_write_colnames) {
			colnames(resultCI)<-paste(colnames(resultCI), newsuffix)
		} else { 
				attr(resultCI, "means_suffix")<-newsuffix
		}
				
		names(dimnames(resultCI)) <- names(dimnames(result))
		result <- resultCI
	}
	
	#keep meta attribute
	attr(result, "meta") <- attr(xa, "meta")
	
	result
}


#' A version of mean_array_z() where the confidence intervals are percentile based 
#' rather than using asymptotic theory.
#' Mean across Z dimension of a 3D array. Each row and column cell is
#' averaged across the Z dimension.
#' 
#' For the vector of statistic values (e.g. a vector of means from multiple runs),
#' the 2.5 and 97.5 percentiles are used as the upper and lower limits of the 95% C.I..
#' 
#' @param xa
#'  an array with a Z dimension
#' @param CI
#'  if TRUE and xa has more than 1 Z dimension, lower and upper confidence intervals 
#'  are returned in additional columns
#' @param NA.as.zero
#'  if TRUE (default), treat NAs as if they are zeros
#' 
#' @return
#'  a matrix of means across the Z dimension. The "meta" attribute
#'  of xa, if any, is retained.
#' 
#' @export
#' @examples
#' #xa <- runs_array
mean_array_z_pctile_CIs <- function (xa, CI=TRUE, NA.as.zero=T) {
	if (NA.as.zero) xa[is.na(xa)] <- 0 #turns any 0s to NAs (this is the default)
	
	result <- apply(xa, c(ROW,COL), mean, na.rm=TRUE) 
	numZ <- dim(xa)[ZDIM]
	
	# CIs only make sense if more than 1 Z dim
	if (CI && numZ > 1) {
		
		##errZ <- apply(xa,c(ROW,COL),err)
		
		#calculate left CI
		leftZ <- apply(xa, c(ROW,COL), function(x) {quantile(x, .025, na.rm=T)})
		
		#calculate right CI
		rightZ <- apply(xa, c(ROW,COL), function(x) {quantile(x, .975, na.rm=T)})
		
		#add left and right Z to the right hand side of result
		resultCI <- cbind(Mean=result,  Lower=leftZ, Upper=rightZ)
		
		# reorder resultCI so that lower and upper is next to the mean of each grouping
		numGroups <- dim(xa)[COL]
		reordering <- as.vector(sapply(c(1:numGroups), function (x) { seq(from=x, length.out=3, by=numGroups)}))
		resultCI <- resultCI[, reordering]

		attr(resultCI, "colnames_original") <- colnames(resultCI)
		colnames_suffix <- rep(c("Mean", "Lower", "Upper"), numGroups)
		attr(resultCI, "means_suffix") <- colnames_suffix
				
		colnames(resultCI) <- paste(colnames(resultCI), colnames_suffix)
		names(dimnames(resultCI)) <- names(dimnames(result))
		result <- resultCI
	}
	
	#keep meta attribute
	attr(result, "meta") <- attr(xa, "meta")
	
	result
}

#' A new version of mean_array_z_pctile_CIs().  Differs from mean_array_z_pctile_CIs() in
#' that the percentages are calculated within each run and the mean of the percentages
#' taken as the point estimate. In mean_array_z_pctile_CIs() the mean numerator and 
#' denominator are taken over the runs and to get the point estimate this mean numerator 
#' is divided by the mean denominatoror to get a point estimate of the percentage.  This
#' is equivalent to the first method (implemented in this function) if the denominator
#' does not change over time, but if the denominator does change over time (e.g. this
#' might happen when we are taking percentages by groups where the grouping variable is
#' time-variant) the results are different and the method in this function is preferable.
#' The same difference between the two versions of the function is seen in the calculation
#' of confidence intervals.  In mean_array_z_pctile_CIs() the 2.5th and 97.5th percentiles
#' of the frequencies over the runs are taken and then these are divided by the mean
#' denominator.  If the we have a situation as described above where percentages are being
#' calculated by a time-variant grouping variable then we can end up with confidence
#' intervals that are greater than 100%. In this function, the 2.5th and 97.5th percentiles
#' of the percentages are taken so that, in the case where percentages are calculated by a 
#' time-variant grouping variable, the confidence intervals are more accurate and will not
#' go above 100%.   
#' This function has additional arguments compared to mean_array_z_pctile_CIs(), namely,
#' cat.adjustments, dict, and binbreaks. 
#' @param xa
#'  an array with a Z dimension
#' @param CI
#'  if TRUE and xa has more than 1 Z dimension, lower and upper confidence intervals 
#'  are returned in additional columns
#' @param NA.as.zero
#'  if TRUE (default), treat NAs as if they are zeros
#' @param cat.adjustments
#' The cat.adjustments list containing the cat.adjustments for multiple variables.
#' Within the function the specific cat.adjustments for the variable of interest are 
#' extracted from the list.  Either cat.adjustments or binbreaks are needed if frequencies
#' of a continuous variable are being requested.  
#' @param dict
#' the specific project dictionary
#' @param binbreaks 
#' The binbreaks for the specific outcome variable.  Either binbreaks or cat.adjustments 
#' may be provided to the function.
mean_array_z_pctile_CIs2 <- function (xa, CI=TRUE, NA.as.zero=T, cat.adjustments=NULL, dict, binbreaks=NULL) {
	if ((NA.as.zero)&(sum(is.na(xa))>0)) xa[is.na(xa)] <- 0
	
	varname <- attr(xa, "meta")["varname"]
	#binbreaks will be null unless the variable is a continuous one with specified binbreaks
	
	if (is.null(binbreaks)) {
		binbreaks <- attr(cat.adjustments[[varname]], "cont.binbreaks")
	}
	grpby.tag <- attr(xa, "meta")["grpby.tag"]
	
	pct.array <- proportions_at_each_run(xa, grpby.tag, binbreaks, varname, dict)
	
	#take the mean across the z-dimensions to get the mean percentage in each category in each year across runs
	result <- apply(pct.array, c(ROW,COL), mean)
	col.names <- colnames(xa)
	colnames(result) <- col.names
	
	numZ <- dim(xa)[ZDIM]
	
	# CIs only make sense if more than 1 Z dim
	if (CI && numZ > 1) {
		
		#calculate left CI
		leftZ <- apply(pct.array, c(ROW,COL), function(x) {quantile(x, .025, na.rm=T)})
		colnames(leftZ) <- col.names
		
		#calculate right CI
		rightZ <- apply(pct.array, c(ROW,COL), function(x) {quantile(x, .975, na.rm=T)})
		colnames(rightZ) <- col.names
		
		#add left and right Z to the right hand side of result
		resultCI <- cbind(Mean=result,  Lower=leftZ, Upper=rightZ)
		
		# reorder resultCI so that lower and upper is next to the mean of each grouping
		numGroups <- dim(xa)[COL] #CHECK
		reordering <- as.vector(sapply(c(1:numGroups), function (x) { seq(from=x, length.out=3, by=numGroups)}))
		resultCI <- resultCI[, reordering]
		
		colnames(resultCI) <- paste(colnames(resultCI), rep(c("Mean", "Lower", "Upper"), numGroups))
		names(dimnames(resultCI)) <- names(dimnames(result))
		result <- resultCI
	}
	
	#keep meta attribute
	attr(result, "meta") <- attr(xa, "meta")
	rownames(result) <- rownames(xa)
	
	result
}


#' Calculates proportions for each iteration for each run.
#' Called by mean_array_pctile_CIs2().   
#' If xa has a group-by tag then proportions are calculated separately for each group.
#' 
#' Other way had the means of the numerator collated.  A percentage was calculated by 
#' assuming that the denominator did not change over time.  In this function the group-by 
#' variable can be a time-variant variable and hence the denominator changes over time. 
#' Because of this, the percentages are calculated for each group (if there is grouping),
#' for each year for each run.  Then later the mean of the percentages is taken rather 
#' than the mean of the frequencies.  For more details see the preamble to 
#' mean_array_pctile_CIs2(). 
#' 
#' @param xa
#' an array of run results.  nrow = num iterations, 
#' ncol = num variable groups x num group-by groups, num z dimensions = num runs
#' @param grpby.tag
#' NA or the name of the groupby variable
#' @param binbreaks
#' Passed from mean_array_pctile_CIS2().  Used to calculate the number of categories for
#' the outcome variable.
#' @param varname
#' a string of length 1 with the name of the outcome variable.  Passed from 
#' mean_array_pctile_CIS2().  
#'@param dicr
#' the dictionary of the specific MSM project.
proportions_at_each_run <- function(xa, grpby.tag, binbreaks, varname, dict) {
	
	if (!is.na(grpby.tag)) {
		if (grpby.tag=="") {
			grpby.tag <- NA
		}
	}
	
	if (!is.na(grpby.tag)) {
		#if there is grouping
		#empty array into which the proportions will be placed
		pct.array <- array(dim=dim(xa))
		#for each run calculate proportions
		for (i in 1:dim(xa)[3]) {
			#take data from run i
			mat <- xa[,,i]
			#num of categories for the primary variable
			num.categories <- length(binbreaks) - 1
			#number of groups for the group-by variable
			numgroups <- ncol(mat)/num.categories
			#if there are no binbreaks for the primary variable then num.categories is -1
			#and numgroups is negative.
			#numgroups and num.categories must be calculated in a different way
			if (numgroups<0) {
				numgroups <- length(names(dict$codings[[grpby.tag]]))
				if ((numgroups==0)&(dict$dlookup_exists(grpby.tag)==1)) {
					#there is a user specified subgroup
					numgroups <- 2
				}
				
				if (is.null(dict$codings[[varname]])) {
					stop(gettextf("No binbreaks or dictionary codings for %s", varname))
				}
				
				num.categories <- length(names(dict$codings[[varname]]))
			}
			grpby <- rep(1:numgroups, each=num.categories)
			#calculate proportions
			props <- apply(mat, ROW, function(x) {
						grpsum <- tapply(x, grpby, sum)
						grpsum2 <- rep(grpsum, each=num.categories)
						x/grpsum2})
			pct.array[,,i] <- t(props)
		}
		return(pct.array)
	}
	
	if (is.na(grpby.tag)) {
		#if there is no grouping, xa is an array (z=number of runs), 
		#each z-dimension is a cross-tab of years by categories
		#xa has the raw numbers
		#pct.list is a list (1 element per run), each element of the list is the percentages in each category in each year
		pct.list <- apply(xa, 3, function(x) {list(t(apply(x, ROW, function(x) {x/sum(x)})))})
		#convert the list to a 3D array
		pct.array <- array(dim=c(nrow(xa), ncol(xa), length(pct.list)))
		for (i in 1:length(pct.list)) {
			pct.array[,,i] <- matrix(as.vector(unlist(pct.list[[i]])), ncol=ncol(xa), nrow=nrow(xa))
		}
		return(pct.array)
	}
}

#' Mean applied over a list of matrices/vectors. Aligns matrices/vectors first
#' so rows and columns match (see align.by.name.list.mx).
#' 
#' @param listmx
#'  a list of matrices
#' 
#' @return 
#'  mean of each matrix element across all list elements 
#' 
#' @export
#' @examples 
#'
#' # list of 2 2x3 matrices 
#' listmx1 <- structure(list(`1` = structure(c(646L, 284L, 23L, 12L, 52L, 58L), .Dim = 2:3, .Dimnames = structure(list(c("0", "1"), c("1","2", "3")), .Names = c("", "")), class = "table"), `2` = structure(c(670L,260L, 24L, 11L, 54L, 56L), .Dim = 2:3, .Dimnames = structure(list(    c("0", "1"), c("1", "2", "3")), .Names = c("", "")), class = "table")), .Names = c("1","2"))
#' 
#' # list of vectors
#' listmx2 <- list(A = c("0"=720L, "1"=353L), B = c("0"=722L, "1"=355L))
#'
#' # list of 1 vector
#' listmx3 <- list(A = c("0"=720L, "1"=353L))
#' 
#' # list of 1 2D vector 
#' listmx4 <- structure(c(200L, 400L), .Dim = c(2L, 1L), .Dimnames = structure(list(c("0", "1"), NULL), .Names = c("", "")), class = "table")
#' 
#' # list of 2 1x1 matrices without dimnames
#' listmx5 <- list(matrix(100), matrix(200)) 
#' 
#' # list of 2 4x1 matrices with different number of rows 
#' listmx6 <- list(matrix(c(10,20,30,40), dimnames = list(c(1:4), NULL)), matrix(c(20,30,40,50,60), dimnames = list(c(2:6), NULL)))
#'
#' # list of 2 4x1 matrices with same number of rows but different row names, and no col names 
#' listmx7 <- list(matrix(c(10,20,30,40), dimnames = list(c(1:4), NULL)), matrix(c(20,30,40,50), dimnames = list(c(2:5), NULL)))
#'
#' # list of 3x2 matrix and 4x2 matrix  
#' listmx8 <- list(matrix(c(10,20,30,40,50,60), nrow = 3, ncol = 2, dimnames = list(c("a","b","c"), c(2:1))), matrix(c(20,30,40,50,60,70,80,90), nrow = 4, ncol = 2, dimnames = list(c("b","d","e","f"), c(2:3))))
#'
#' #' list of 5x1 table and 4x1 table
#' listmx9 <-  list(structure(c(1L, 15L, 3L, 27L, 1029L), .Dim = c(5L, 1L), .Dimnames = structure(list(    c("1", "2", "3", "4", "5"), NULL), .Names = c("", "")), class = "table"),    structure(c(11L, 1L, 18L, 1045L), .Dim = c(4L, 1L), .Dimnames = structure(list(        c("2", "3", "4", "5"), NULL), .Names = c("", "")), class = "table"))
#'
#' #' list of 2 matrices with meta
#' listmx10 <- list(structure(matrix(100, dimnames = list(1, NULL)), meta=c("grpby.tag"="r1stchildethn")), matrix(c(200,300), dimnames = list(c(1,2), NULL))) 
#' 
#' listmx <- listmx1
#' listmx <- listmx2
#' listmx <- listmx3
#' listmx <- listmx4
#' listmx <- listmx5
#' listmx <- listmx6
#' listmx <- listmx7
#' listmx <- listmx8
#' listmx <- listmx9
#' listmx <- listmx10
#'
#' test <- lapply(list(listmx1,listmx2,listmx3,listmx4, listmx5, listmx6, listmx7, listmx8, listmx9, listmx10), mean_list_mx)
#'  
#' mean_list_mx(listmx)
#' mean_list_mx(listmx1)
#' mean_list_mx(listmx2)
#' mean_list_mx(listmx3)
#' mean_list_mx(listmx4)     
#' mean_list_mx(listmx5)
#' mean_list_mx(listmx6)
#' mean_list_mx(listmx7)
#' mean_list_mx(listmx8)
#' mean_list_mx(listmx9)
#' mean_list_mx(listmx10)
mean_list_mx <- function(listmx) {
	# only 1 element, so just return that element
	if (is.list(listmx) && length(listmx) == 1) {
		return(listmx[[1]])
	}
	
	#align matrices
	#listmx.aligned <- listmx
	listmx.aligned <- align.by.name.list.mx(listmx)
	
	#convert to array, each matrix is flattened into a single row (names not preserved)
	listmx.flat <- laply(listmx.aligned, .fun=c, .drop = FALSE)
	
	#convert any NAs to zero
	listmx.flat[is.na(listmx.flat)] <- 0
	
	# get mean as vector
	listmx.flat <- colMeans(listmx.flat)
	
	#reapply dimensions, names and meta from first element
	aligned.element <- listmx.aligned[[1]]
	structure(listmx.flat, 
			.Dim = dim(aligned.element), 
			.Dimnames = dimnames(aligned.element), 
			.Names = names(aligned.element),
			meta = attr(aligned.element, "meta"),
			class = if(!class(aligned.element) %in% c("numeric","integer")) class(aligned.element) else NULL)
	
}

#' Produce proportions of x in relation to the sum of group
#' specified by grpby.
#' 
#' @param x
#'  a vector of values
#' @param grpby
#'  a vector of indices, the length of x, which specify the group
#'  each value of x belongs to
#' @param na.rm
#'   logical. Should missing values (including NaN) be removed?
#' @param CI
#' logical.  Are confidence intervals present?
#' @param num.runs
#' number of runs which were used to generate x
#' 
#' @return 
#'  vector of proportions, calculated according to group
#' 
#' @export
#' @examples
#' 
#' x <- 1:6
#' grpby <- c(1,1,2,2,3,3) 
#' grpby <- c('M','M','F','F','F','F')
#' prop.table.grpby(x, grpby, num.runs = 1)
prop.table.grpby <- function (x, grpby, na.rm=TRUE, CI=FALSE, num.runs) {
	if ((CI==FALSE)|(num.runs==1)) {
		grpsum <- tapply(x, grpby, sum, na.rm=na.rm)
		#check for grouping
		if (length(x)>length(grpsum)) {
			#grouping 
			n<-length(x)/length(grpsum)
			grpsum <- rep(grpsum, each=n)
			result <- structure(as.vector(x/grpsum), .Names=names(x))
		} else {
			#no grouping
			result <- structure(as.vector(x / grpsum[grpby]), .Names=names(x))
		}
		
	} else if ((CI==TRUE)&(length(unique(grpby))==1)&(num.runs>1)) {
		# if CI = TRUE, there is no grouping, and there was more than 1 run
		#take the 1st, 4th, 7th, etc element of x to get the sum
		n <- length(x)/3 #number of unique means
		id <- (3*(1:n) - 3) + 1
		grpsum <- sum(x[id])
		result <- structure(as.vector(x / grpsum[grpby]), .Names=names(x))
		
	} else if ((CI==TRUE)&(length(unique(grpby))>1)&(num.runs>1)) {
		# if CI = TRUE, there is grouping, and there was more than 1 run
		n <- length(x)/3 #number of unique means
		id <- (3*(1:n) - 3) + 1 #identifies where the means are
		num.cats.outcome <- n/length(unique(grpby)) #number of categories for the outcome variable (not the grouping variable)
		id2 <- rep(1:(n/num.cats.outcome), each=num.cats.outcome) #which means belong to which category
		grpsum <- tapply(x[id], id2, sum, na.rm=na.rm)
		num.grps <- length(unique(grpby))
		grpsum <- rep(grpsum, each=(n*3)/num.grps)
		result <- structure(as.vector(x / grpsum), .Names=names(x))
	}
	#x / grpsum[grpby] #old code before JT modifed it to handle CIs
	return(result)
}

#' Execute quantile on the columns of a matrix.
#' The group-by variable must be time-invariant.
#' 
#' @param mx
#'  matrix
#' @param new.names
#'  if specified, the names of the result will be 
#'  set to this vector
#' @param ...
#'  additional arguments to pass to \code{\link{quantile}}
#' 
#' @return
#'  quantile of each column returned as a row
#' 
#' @export
#' @examples 
#' \dontrun{ 
#' mx <- years6_13$outcomes[["cond"]]
#' mx <- env.base$modules$years1_5$outcomes[["hadmtot"]]
#' mx <- env.base$modules$years6_13$outcomes[["cond"]]
#' mx <- env.base$modules$years1_5$outcomes[["gptotvis"]]
#' quantile_mx_cols(mx)
#' quantile_mx_cols(mx, probs=seq(0, 1, 0.2), na.rm = TRUE)
#' quantile_mx_cols(mx, new.names=c("Min", "20th", "40th", "60th","80th","Max"), probs=seq(0, 1, 0.2), na.rm = TRUE)
#' quantile_mx_cols(mx, probs=seq(0, 1, 0.02), na.rm = TRUE)
#' }
quantile_mx_cols <- function (mx, new.names=NULL, ...) {
	#quantile(mx[,1], probs=seq(0.2, 1, 0.2))
	result <- t(apply(mx, COL, function (x) {
						qx <- quantile(x, ...)
						if (!is.null(new.names)) {
							names(qx) <- new.names
						}
						qx
					}))
	
	structure(result, meta=c(varname=attr(mx, "varname")))
}


#' Execute quantile on the columns of a matrix by a grouping variable.
#' Only used for user-specified subgroups.
#' Extension of quantile_mx_cols to handle a time-variant grouing variable.
#' grpby can be a vector or a matrix with columns representing the value of the 
#' grpby variable in different years.
#' Does not have ability to include weights or logisets.   
#' The group-by variable may be time-invariant or time-variant.  
#' Not sure if can handle no grouping. 
#' 
#' @param mx
#'  matrix
#' @param grpby
#' a vector or matrix indicating the group to whic the unit belongs.  
#' Quantiles will be calculated separately for each group.
#' If the group-by variable is time-invariant grpby can be provided as a vector or as a 
#' matrix with every column the same.  If the group-by variable is time-variable then 
#' grpby should be a matrix with number of columns equal to number of years. 
#' @param grpby.tag
#' string. grouping variable names
#' @param new.names
#'  if specified, the names of the result will be 
#'  set to this vector
#' @param ...
#'  additional arguments to pass to \code{\link{quantile}}
#' e.g. na.rm=T
#' 
#' @return
#'  quantile of each column returned as a row.  
#' Quantiles for each group are put side-by-side in a rbind fashion.
#' 
#' @export
quantile_mx_cols_BCASO <- function (mx, grpby=NULL, grpby.tag=NULL, new.names=NULL, probs=c(0,.1,.25,.5,.75,.9,1), logiset=NULL, dict=dict, ...) {
	#quantile(mx[,1], probs=seq(0.2, 1, 0.2))
	
	# check that grpby dimensions are correct   
	if (!is.null(grpby)){
		if (is.vector(grpby)) {
			if (!is.null(dim(grpby))) {
				#if a matrix
				if(nrow(mx) != nrow(grpby)) {
					param1Name <- as.character(sys.call())[2]
					stop(gettextf("Number of rows in %s != number of rows of grpby", param1Name))
				}
			} else if (nrow(mx)!=length(grpby)) {
				param1Name <- as.character(sys.call())[2]
				stop(gettextf("Number of rows in %s != number of rows of grpby", param1Name))
			}
		}	
	}
	
	if(is.vector(grpby)) {
		grpby <- matrix(rep(grpby, ncol(mx)), ncol=ncol(mx))
	}
	
	if (!is.null(logiset)) {
		logiset[is.na(logiset)] <- 0
		#converts logiset from T/F to 0/1 (even if no NAs)
		if (is.vector(logiset)) {
			#mx <- mx[logiset, , drop=FALSE] #appears to be incorrect 
			mx <- mx[logiset==1,]
			#subset grpby which is a matrix by the logiset vector
			grpby <- grpby[logiset==1,]
		}
		if (is.matrix(logiset)) {
			mx2 <- as.vector(mx)
			if (length(mx2)!=length(logiset)) {
				#logiset can be matrix with one column,  in thsi case we have to repeat out
				#logiset so the entire dataset gets subset rather than just the first column
				logiset <- rep(logiset, ncol(mx))
				logiset <- matrix(logiset, byrow=F, nrow=nrow(mx))
			}
			if (length(mx)!=length(logiset)) {
				stop("Check logiset subsetting in quantile_mx_col_BCASO()")
			}
			NA_index <- which(logiset==F)
			
			mx2[NA_index] <- NA
			mx <- matrix(mx2, byrow=F, nrow=nrow(mx))
			if (!is.null(grpby)) {
				grpby2 <- as.vector(grpby)
				grpby2[NA_index] <- NA
				grpby <- matrix(grpby2, byrow=F, nrow=nrow(grpby))
			}
		}
	}

	
	if ((is.null(grpby))|(sum(is.na(grpby))==length(grpby))) {
		result <- t(apply(mx, COL, function(x) {quantile(x, probs=probs, ...)}))
		colnames(result) <- new.names
	} else {
		result.by.col<- lapply(1:ncol(mx), function(i) {
					#i=1
					aggregate(mx[,i], by=list(grpby[,i]), FUN=quantile, probs=probs, ...)
				})
		num.groups <- nrow(result.by.col[[1]])
		num.yrs <- length(result.by.col)
		#empty, then gets filled
		result <- matrix(ncol=length(probs)*num.groups, nrow=num.yrs)

		for (j in 1:num.groups) {
			grp.result <- matrix(nrow=num.yrs, ncol=length(probs))
			for (i in 1:num.yrs) {
				grp.result[i,]<-result.by.col[[i]][j,][,2]
			}
			result[,((j*7)-6):(j*7)]<-grp.result
		}
		
		#name columns
		start.colnames <- rep(new.names, num.groups)
		if ((!is.null(grpby.tag))&(dict$dlookup_exists(grpby.tag)==1)) {
			#colnames(result) <- c("Not in subgroup", "In subgroup")
			cat.names <- c("Not in subgroup", "In subgroup")
		} else if (!is.null(grpby.tag)) {
			cat.names <- unlist(dict$cnamesLookup(grpby.tag))
		} else {
			cat("Checking naming sections in quantile_mx_cols_BCASO \n")
		}
		colname.prefixes <- rep(cat.names, each=ncol(result)/length(cat.names))
		final.colnames <- character(length(start.colnames))
		for (i in 1:length(colname.prefixes)) {
			final.colnames[i] <- paste(colname.prefixes[i], start.colnames[i])
		}
		colnames(result) <- final.colnames

	}
	
	structure(result, meta=c(varname=attr(mx, "varname")))
}

#' Summary table, with option to group and weight results.
#'  
#' @param x
#'  vector of values which can be interpreted as factors 
#' @param grpby
#'  elements to group by, or NULL to do no grouping
#'  Same length as the columns of x.
#' @param wgts
#'  vector of weights, or NULL to do no weighting
#'  Same length as the columns of x.
#' @return
#'  a weighted summary table, or rows of weighted summary tables if grpby is specified.
#' function also returns the weighted number of NA's, but does not include NA's with weights of NA.
#' 
#' @export
#' @examples
#' library(Hmisc)
#' x <- c(8,8,2,1,1,8)
#' x <- c(8,8,2,1,1,NA)
#' x <- c(NA,NA,2,1,1,8)
#' grpby <- c('M','M','F','F','F','F')
#' wgts<- c(1,2,2,3,1,1)
#' summary.grpby(x)
#' summary.grpby(x, grpby)
#' summary.grpby(x=x, grpby=grpby,wgts=wgts)
summary.grpby <- function (x, grpby = NULL, wgts=NULL) {
	
# 1. beginning check - that weight dimensions are correct
	if (!is.null(wgts))	{
			if(length(x) != length(wgts)) {
				param1Name <- as.character(sys.call())[3]
				stop(gettextf("Number of rows in %s != length of wgts", param1Name))
			}
		
	}	
# 2. beginning check  - that grpby dimensions are correct	
	if (!is.null(grpby)){
			if(length(x) != length(grpby)) {
				param1Name <- as.character(sys.call())[2]
				stop(gettextf("Number of rows in %s != length of grpby", param1Name))
			}
	}
	
#bulk of function starts	
	if (is.null(grpby) & is.null(wgts)) {
		summary(x)
	} else { 
			non.nas <-  !is.na(x)
			isnullgrpby=F
			if (is.null(grpby)) {grpby<-rep(1, length(x)) ;isnullgrpby=T}
			if (is.null(wgts)) wgts<-rep(1, length(x))
		
			if (isnullgrpby==F) {unique_grpby<-unique(sort(grpby))} else {unique_grpby<-1}
			
			# note using for statements here, as aggregate statement wont work with weights parameter
			agg<-matrix(data=NA, nrow=length(unique(sort(grpby))), ncol=5)
			
			for (i in 1:length(unique_grpby)) agg[i,]<- wtd.quantile(x=x[non.nas & grpby==unique_grpby[i]], weight=wgts[non.nas & grpby==unique_grpby[i]])
			
			means<-matrix(data=NA, nrow=length(unique(sort(grpby))), ncol=1)
			
			for (i in 1:length(unique_grpby)) means[i,]<- wtd.mean(x=x[non.nas & grpby==unique_grpby[i]], weight=wgts[non.nas & grpby==unique_grpby[i]])
			
			NA_column<-matrix(data=NA, nrow=length(unique(sort(grpby))), ncol=1)
			
			#only count an NA if the correspondning weight is also not NA. Wtdtable does not work without this.
	
			wgts[is.na(x) & is.na(wgts)]<-0
			
			for (i in 1:length(unique_grpby)) {NA_column[i,]<- if ( (length(x[is.na(x) & grpby==unique_grpby[i]])==0 ) | sum(wgts[is.na(x) & grpby==unique_grpby[i]])==0 ) 0 else {as.matrix(wtdtable(x[is.na(x) & grpby==unique_grpby[i]], wgts[is.na(x)& grpby==unique_grpby[i]]))}}
			
			result<-  if (!isnullgrpby) {cbind(agg[,1:3], means, agg[,4:5], NA_column)} else {cbind(t(agg[,1:3]), means, t(agg[,4:5]), NA_column)}
			colnames(result)<-c("Min." ,   "1st Qu.", "Median" , "Mean", "3rd Qu." ,"Max." , "NA's" )
			rownames(result)<-unique_grpby
			
			return(result)
	}
}


#' Execute summary on the columns of a matrix, allowing for weighting and grouping.
#' work in progress
#' 
#' @param mx
#'  matrix
#' 
#' @param logiset
#'  logical vector indicating which rows to include, or NULL to include all.
#' 
#' @return
#' returns each column's summary as an
#' element of a list. The element of the list
#' consists of a summary in a row (or multiple summary rows if the grpby
#' parameter is used - one row for each grpby category for the column).
#' 
#' @export
#' @examples
#' mx <- matrix(c(8,2,2,2,8,2,3,2,3,2,2,4,8,2,3,4,2,2,4,3),nrow=4,ncol=5,dimnames=list(NULL, LETTERS[1:5]))
#' logiset <- c(FALSE, TRUE, FALSE, TRUE)
#' mx <- matrix(c(8,2,2,2,8,2,3,2,3,2,2,4,8,2,3,4,2,2,4,3),nrow=4,ncol=5,dimnames=list(NULL, LETTERS[1:5]))
#' summary_mx_grpby_cols(mx, logiset=logiset)
#' grpby<-c("M","F","F","M")
#' wgts<-c(1,2,2,2)
#' logiset <- c(FALSE, TRUE, TRUE, TRUE)
#' summary_mx_grpby_cols (mx=mx, logiset=logiset, wgts=wgts,grpby=grpby)

summary_mx_grpby_cols <- function (mx, logiset=NULL, wgts=NULL,grpby=NULL) {
	

		# check - that logiset dimensions are correct
	if (!is.null(logiset))	{
			if(nrow(mx) != length(logiset)) {
				param1Name <- as.character(sys.call())[2]
				stop(gettextf("Number of rows in %s != length of logiset", param1Name))
			}
	}	
	
	
	
	# subset
	if (!is.null(logiset)) mx <- mx[logiset, ,drop=FALSE]
	if (!is.null(logiset)) grpby <- subset(grpby,logiset)
	if (!is.null(logiset)) wgts <- subset(wgts,logiset)
	
	if (is.vector(wgts)) wgts<-matrix(rep(wgts, ncol(mx)), ncol=ncol(mx))
	if (is.vector(grpby)) grpby<-matrix(rep(grpby, ncol(mx)), ncol=ncol(mx))
	

	
	sm <- lapply(1:ncol(mx), function(i){ summary.grpby (mx[,i],grpby=grpby[,i],wgts=wgts[,i])})
	return(sm)

}




#' Execute summary on the columns of a matrix.
#' 
#' @param mx
#'  matrix
#' 
#' @param logiset
#'  logical vector indicating which rows to include, or NULL to include all.
#' 
#' @return
#'  summary of each column, returned as a row
#' 
#' @export
#' @examples
#' mx <- matrix(c(8,2,2,2,8,2,3,2,3,2,2,4,8,2,3,4,2,2,4,3),nrow=4,ncol=5,dimnames=list(NULL, LETTERS[1:5]))
#' logiset <- c(FALSE, TRUE, FALSE, TRUE)
#' summary_mx_cols(mx)
#' summary_mx_cols(mx, logiset=logiset)
summary_mx_cols <- function (mx, logiset=NULL) {
	
	# subset
	if (!is.null(logiset)) mx <- mx[logiset, ,drop=FALSE]
	#if (!is.null(logiset)) grpby <- grpby[logiset]
	
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

#' Frequency table, with option to group results.
#' The group-by variable must be time-invariant.
#'  
#' @param x
#'  vector of values which can be interpreted as factors 
#' @param grpby
#'  vector of elements to group by, or NULL to do no grouping
#'  Same length as the columns of x.
#' @param useNA
#'  whether to include extra NA levels in the table.
#'  one of c("no", "ifany", "always"). Defaults to "ifany".
#' 
#' @return
#'  a table. If grpby is specified this will be a table
#'  with columns that are the group by and rows the categories. 
#'  If grpby = NULL then a table with 1 column and rows as categories is returned.
#' 
#' @export
#' @examples
#' 
#' x <- rep(0,1075)
#' x <- c(8,8,2,1,1,8)
#' grpby <- c('M','M','F','F','F','F')
#'
#' table.grpby(x)
#' table.grpby(x, grpby)
table.grpby <- function (x, grpby = NULL, useNA = "ifany") {
	if (is.null(grpby)) {
		t(t(table(x, useNA = useNA, deparse.level = 0)))
	} else {	
	
		if(length(x) != length(grpby)) {
			stop(gettextf("Length of %s != length of %s", 
							as.character(sys.call())[2], as.character(sys.call())[3]))
		}
		
		table(x, grpby, useNA = useNA, deparse.level = 0)
	}
}

#' Frequency table, with option to group results.  
#' Extension of table.grpby() that can handle grpby as a matrix as well as a vector.  
#' The group-by variable may be time-invariant or time-variant.  
#'  
#' @param x
#'  vector of values which can be interpreted as factors 
#' @param grpby
#'  A vector or matrix of elements to group by, or NULL to do no grouping.
#'  vector must be same length as the columns of x or matrix must have the same number
#'  of rows as z.
#' If the group-by variable is  time-invariant grpby can be provided as a vector or as a 
#' matrix with every column the same.  If the group-by variable is time-variable then 
#' grpby should be a matrix with number of columns equal to number of years. 
#' @param useNA
#'  whether to include extra NA levels in the table.
#'  one of c("no", "ifany", "always"). Defaults to "ifany".
#' 
#' @return
#'  a table. If grpby is specified this will be a table
#'  with columns that are the group by and rows the categories. 
#'  If grpby = NULL then a table with 1 column and rows as categories is returned.
#' 
#' @export
table.grpby_BCASO <- function (x, grpby = NULL, wgts=NULL) {
	
	if (is.null(wgts)) {wgts <- rep(1,length(x)) }   
	
	if ((is.null(grpby))|(sum(is.na(grpby))==length(grpby))) {
		a <- wtd.table(x, weights=wgts)
		t(t(a$sum.of.weights))
		
		#t(t(table(x, useNA = useNA, deparse.level = 0)))
	} else {
		
		if(length(x) != length(grpby)) {
			stop(gettextf("Length of %s != length of %s", 
							as.character(sys.call())[2], as.character(sys.call())[3]))
		}
		
		a <- aggregate(wgts, by = list(grpby=grpby, x), FUN = sum)
		b <- t(tapply(a$x, list(a$grpby, a$Group.2), identity))
		b[which(is.na(b))] <- 0
		b
		
	}
}

#' Generates a frequency table, with option to group by, for each column of a matrix.
#' The group-by variable must be time-invariant.
#' 
#' @param mx
#'  matrix, or dataframe
#' 
#' @param grpby
#'  a vector of elements to group by, or NULL or unspecified to do no grouping.
#'  Same length as the columns of mx.
#' 
#' @param grpby.tag
#'  a character vector. If specified this value with be attached as the
#'  meta attribute "grpby.tag"
#'
#' @param logiset
#'  logical vector indicating which rows to include, or NULL to include all.
#'
#' @param useNA
#'  whether to include extra NA levels in the table.
#'  one of c("no", "ifany", "always"). Defaults to "ifany".
#' 
#' @return
#'  list. Each element of the list is a frequency table for a column in mx.
#'  If grpby is specified this will be a table with columns that are the group by and rows the categories. 
#'  If grpby = NULL then a table with 1 column and rows as categories is returned.
#'
#' @export 
#' @examples
#' mx <- matrix(c(8,2,2,2,8,2,3,2,3,2,2,4,8,2,3,4,2,2,4,3),nrow=4,ncol=5,dimnames=list(NULL, LETTERS[1:5]))
#' grpby <- c('M','F','F','M')
#' table_mx_cols(mx)
#' table_mx_cols(mx, grpby)
#' logiset <- c(FALSE, TRUE, FALSE, TRUE)
#' table_mx_cols(mx, grpby = grpby, logiset = logiset)
table_mx_cols <- function(mx, grpby = NULL, grpby.tag = NULL, logiset = NULL, useNA = "ifany") {

	# subset
	if (!is.null(logiset)) mx <- mx[logiset, ,drop=FALSE]
	if (!is.null(logiset)) grpby <- grpby[logiset]
	
	# if no column names, number them off
	if (is.null(dimnames(mx)[[COL]])) {
		dimnames(mx)[[COL]] <- seq(dim(mx)[COL])
	}
	
	# get freqs for each column of mx, return a list
	# in case dimensions of results of the table.grpby calls are different
	# each element of the list represents a column, 
	# each element is a matrix where columns are the group by
	# and rows the categories
	
	#use lapply instead of apply because apply simplifies
	#use lapply instead of apply so we don't have the split attributes
	results.by.col <- lapply(1:ncol(mx), function (i) {
				#i <- 1
				table.grpby(mx[,i], grpby, useNA = useNA)
			})
	
	# add names 
	names(results.by.col) <- dimnames(mx)[[COL]]
	
	varname <- attr(mx, "varname")
	if (is.null(varname)) {
		varname <- attr(mx, "meta")[["varname"]]
	}
	
	# return with meta
	structure(results.by.col, meta=c(varname=varname, grpby.tag = grpby.tag, set=attr(logiset,"desc")))
	
}


#' Generates a frequency table, with option to group by, and weight, for each column of a 
#' matrix.  The group-by variable may be time-invariant or time-variant.  
#' 
#' @param mx
#'  matrix, or dataframe
#' 
#' @param grpby
#'  a vector or matrix of elements to group by, or NULL or unspecified to do no grouping.
#'  Same length (or number of columns) as the columns of mx.
#' If  the group-by variable is time-invariant grpby can be provided as a vector or as a 
#' matrix with every column the same.  If the group-by variable is time-variable then grpby 
#' should be a matrix with number of columns equal to number of years. 
#' 
#' @param wgts
#'  a vector or matrix of weights, or NULL or unspecified to do no weighting.
#'  Same length (or number of columns) as the columns of mx.
#' 
#' @param grpby.tag
#'  a character vector. If specified this value with be attached as the
#'  meta attribute "grpby.tag"
#'
#' @param logiset
#'  logical vector or matrix indicating which rows to include, or NULL to include all.
#'
#' @param useNA
#'  whether to include extra NA levels in the table.
#'  one of c("no", "ifany", "always"). Defaults to "ifany".
#' 
#' @return
#'  list. Each element of the list is a frequency table for a column in mx.
#'  If grpby is specified this will be a table with columns that are the group by and rows the categories. 
#'  If grpby = NULL then a table with 1 column and rows as categories is returned.
#'
#' @export 
#' @examples
#' \dontrun{
#' mx <- matrix(c(8,2,2,2,8,2,3,2,3,2,2,4,8,2,3,4,2,2,4,3),nrow=4,ncol=5)
#' grpby <- c('M','F','F','M')
#' grpby<-matrix(c('M','F','F','M',  'M','M','F','M',  'M','M','M','M',  'F','F','F','F', 'F','F','F','M'),nrow=4,ncol=5)
#' wgts<-matrix(c(rep(c(1,2,1,1),4),1,3,1,1),nrow=4,ncol=5)
#' table_mx_cols(mx)
#' table_mx_cols(mx, grpby)
#' table_mx_cols(mx, grpby,wgts)
#' logiset <- c(FALSE, TRUE, FALSE, TRUE)
#' logiset <- c(0, 1, 0, 1)
#' logiset <- matrix(data=c(rep(c(FALSE, TRUE, FALSE, TRUE),2),rep(c(TRUE, FALSE, TRUE,FALSE),3)), nrow=4,ncol=5)
#' logiset <- matrix(data=c(rep(c(0, 1, 0, 1),2),rep(c(1, 0, 1,0),3)), nrow=4,ncol=5)
#' table_mx_cols_BCASO(mx, grpby = grpby, logiset = logiset)
#' table_mx_cols_BCASO(mx, grpby = grpby, wgts=wgts, logiset = logiset)}
table_mx_cols_MELC <- function(mx, grpby=NULL, wgts=NULL, grpby.tag=NULL, logiset=NULL, dict) {
	
	if (is.vector(wgts)) wgts <-matrix(rep(wgts, ncol(mx)), ncol=ncol(mx))
	if (is.null(wgts)) wgts <- matrix(rep(1, length(mx)), ncol=ncol(mx))
	if (is.vector(grpby)) grpby <-matrix(rep(grpby, ncol(mx)), ncol=ncol(mx))
	
	#save varname
	varname <- attr(mx, "varname")
	if (is.null(varname)) {
		varname <- attr(mx, "meta")[["varname"]]
	}
	
	if (!is.null(logiset)) {
		logiset[is.na(logiset)] <- 0
		
		# subset by logiset - if logiset is a vector
		if (!is.matrix(logiset)) {
			logiset <- as.logical(logiset)
			mx <- mx[logiset, ,drop=FALSE]
			if ( !is.null(grpby) ) grpby <- grpby[logiset, ,drop=FALSE]
			if ( !is.null(wgts) ) wgts <- wgts[logiset, ,drop=FALSE]
		}
		#subset by logiset - if logiset is a matrix
		if (is.matrix(logiset)) {
			
			#mx loses varname here
			mx2 <- as.vector(mx)

			if (length(mx2)!=length(logiset)) {
				#logiset can be matrix with one column,  in thsi case we have to repeat out
					#logiset so the entire dataset gets subset rather than just the first column
				logiset <- rep(logiset, ncol(mx))
				logiset <- matrix(logiset, byrow=F, nrow=nrow(mx))
			}
			if (length(mx)!=length(logiset)){
				stop("Check logiset subsetting in table_mx_cols_MELC()")
			}
			NA_index <- which(logiset==F)
			
			mx2[NA_index] <- NA
			mx <- matrix(mx2, byrow=F, nrow=nrow(mx))
			
			if (!is.null(grpby)) {
				grpby2 <- as.vector(grpby)
				grpby2[NA_index] <- NA
				grpby <- matrix(grpby2, byrow=F, nrow=nrow(grpby))	
			}
			if (!is.null(wgts))  { 
				wgts2 <- as.vector(wgts)
				wgts2[NA_index] <- NA
				wgts <- matrix(wgts2, byrow=F, nrow=nrow(wgts))
			}
			
		}
		
	}
	
	# if no column names, number them off
	if (is.null(dimnames(mx)[[COL]])) {
		dimnames(mx)[[COL]] <- seq(dim(mx)[COL])
	}
	
	if (!is.null(grpby.tag)) {
		if (grpby.tag=="") {
			grpby.tag <- NULL
		}
	}
	
	# get freqs for each column of mx, return a list
	# in case dimensions of results of the table.grpby calls are different
	# each element of the list represents a column, 
	# each element is a matrix where columns are the group by
	# and rows the categories
	
	#use lapply instead of apply because apply simplifies
	#use lapply instead of alply so we don't have the split attributes
	results.by.col <- lapply(1:ncol(mx), function (i) {
				#i <- 1
				#old:
				##table.grpby_BCASO(mx[,i], grpby[,i], wgts=wgts[,i])
				#new:
				result <- table.grpby_BCASO(mx[,i], grpby[,i], wgts=wgts[,i])
				if ((!is.null(grpby.tag))&(dict$dlookup_exists(grpby.tag)==1)) {
					colnames(result) <- c("Not in subgroup", "In subgroup")
				} 
				return(result)
			})
	
	# add names 
	names(results.by.col) <- dimnames(mx)[[COL]]
	
	# return with meta
	structure(results.by.col, meta=c(varname=varname, grpby.tag = grpby.tag, set=attr(logiset,"desc")))
	
}


#' Weighted frequency table
#'
#' @param x
#' 	vector of values.
#' 
#' @param wgts
#' 	vector of weights. If unspecified a default weight of 1 is used for each value of x
#' 
#' @return
#' 	weighted frequency table
#' 
#' @export 
#' @examples
#' \dontrun{
#' library(Hmisc)
#' x <- c(8,8,2,1,1,8)
#' wgts <- c(1,1,2,1,1,1)
#' wtdtable(x, wgts) 
#' 1 2 8 
#' 2 2 3
#' }
wtdtable <- function (x, wgts=rep(1,length(x))) {
	if(length(x) != length(wgts)) {
		param1Name <- as.character(sys.call())[2]
		param2Name <- as.character(sys.call())[3]
		stop(gettextf("Length of %s != length of weights %s", param1Name, param2Name))
	}
	
	wt <- wtd.table(x, weights=wgts)
	tbl <- wt$sum.of.weights
	names(tbl) <- wt$x
	
	# wtd.table does not count NAs so we have to do it here
	NAs <- sum(wgts[is.na(x)])
	if (NAs > 0) {
		#attach NA column to table
		expandedTbl <- as.array(c(tbl,NAs))
		names(expandedTbl) <- c(names(tbl), NA)
		tbl <- expandedTbl
	}
	
	#NB: cast to table because wtd.table doesn't do this
	#but table does and we want wtdtable to act like table
	#so when it is passed to the data.frame command it 
	#works properly
	as.table(tbl)
}

#' Produces a weighted frequency distribution for each column 
#' of the matrix mx and returns them altogether in one table.
#' Each column can have a different set of categories 
#' (ie: frequency "buckets")
#' 
#' @param logiset
#'  logical vector indicating which rows to include, or NULL to include all.
#' 
#' @param mx
#'  matrix
#' 
#' @param wgts
#'  weights. If unspecified, a default weight of 1 is used for each row
#' 
#' @param addVariableName
#'  if addVariableName = TRUE, then the columns will be given the name of mx
#' 
#' @return
#' weighted frequency distributions stacked in rows
#' 
#' @export 
#' @examples 
#' COL<-2
#' library(Hmisc)
#' mx <- matrix(c(8,2,2,2,8,2,3,2,3,2,2,4,8,2,3,4,2,2,4,3),nrow=4,ncol=5)
#' wgts = rep(1,nrow(mx))
#' addVariableName = FALSE
#' addVariableName = TRUE
#' logiset=rep(TRUE,nrow(mx))
#' logiset=c(TRUE,TRUE,TRUE,FALSE)
#' wtdtable_mx_cols(mx, logiset=logiset)
#' 
wtdtable_mx_cols <- function(mx, wgts = rep(1,nrow(mx)), addVariableName = FALSE, logiset=NULL) {
	
	# subset
	if (!is.null(logiset)) mx <- mx[logiset, ,drop=FALSE]
	if (!is.null(logiset)) wgts <- wgts[logiset]
	
	
	if(nrow(mx) != length(wgts)) {
		param1Name <- as.character(sys.call())[2]
		stop(gettextf("Number of rows in %s != length of wgts", param1Name))
	}
	
	# if no column names, number them off
	if (is.null(dimnames(mx)[[COL]])) {
		dimnames(mx)[[COL]] <- seq(dim(mx)[COL])
	}
	
	# get the total set of categories (ie: frequency buckets)
	cats <- as.numeric(names(table(mx)))
	
	# get freqs for each column of mx
	freqs <- apply(mx, COL, function (x) { wtdtable(x,wgts)})
	
	if (mode(freqs)=="list") {
		# if its a list it means that the set of cats between
		# columns is not consistent so we need to combine all 
		# the freqs together, joining into cats 
		freqs <- data.frame(
				lapply(freqs, 
						function (x)	{ 
							merge(cats, x, by = 1, all.x=TRUE)$Freq 
						}
				)
				, row.names = cats, check.names = FALSE
		)
	}
	
	# transponse  
	tfreqs <- t(freqs)
	
	# add variable name
	if (addVariableName) {
		firstParamName <- as.character(sys.call())[2] 
		names(dimnames(tfreqs))[2] <- firstParamName
	}
	
	tfreqs
}



#' Calculates the weighted mean for each column of the matrix
#' optionally subsetting and grouping by another (equal length) variable.
#' The group-by variable must be time-invariant.
#' 
#' @param mx
#'  matrix or dataframe to calculate column means of 
#'  
#' @param grpby
#'  elements to group by, or NULL or unspecified to do no grouping. 
#'  Same length as the columns of mx.
#' 
#' @param grpby.tag
#'  added to meta attribute of the result
#'
#' @param logiset
#'  logical vector indicating which observations to include, or NULL to include all.
#'
#' @param wgts
#'  elements to weight by, or NULL to do no weighting
#'  
#' @param na.rm
#'  logical. Should missing values be removed?  
#' 
#' @return 
#'  a matrix of means. Each row is the mean for a column in mx.
#'  if grpby is NULL, a single column of means. 
#'  if grpby is specified, a column of means for each group by category.
#' 
#' @export
#' @examples
#' mx <- matrix (c(1:10), ncol = 2) ; mx <- matrix (c(1:3, NA, 5:7, NA, 9:10), ncol = 2) 
#' logiset = NULL
#' grpby = NULL ; grpby.tag = NULL 
#' grpby <- c("A","A","A","B","B") ; grpby.tag = "AB" 
#' wgts = rep(1, nrow(mx))
#' na.rm = FALSE ; na.rm = TRUE
#' 
#' mean_mx_cols(mx, logiset=logiset, wgts=wgts, grpby=grpby, grpby.tag=grpby.tag, na.rm = na.rm)
mean_mx_cols <- function (mx, grpby=NULL, grpby.tag = NULL, logiset=NULL, wgts = NULL, na.rm = F) {
	
	if (is.null(wgts)) wgts <- rep(1, nrow(mx))  #can't make this default param for some reason need to set here
	
	# save before subsetting mx 
	varname <- attr(mx, "varname")
	
	# subset
	if (!is.null(logiset)) mx <- subset(mx, logiset)
	if (!is.null(logiset)) wgts <- subset(wgts, logiset)
	if (!is.null(logiset)) grpby <- subset(grpby, logiset)
	
	if (is.null(grpby)) {
		result <- apply(mx, COL, function(x) {
					#x <- mx[,60]
					if (!na.rm & any(is.na(x))) {
						NA
					} else { 
						non.nas <- !is.na(x)
						sum(x[non.nas] * wgts[non.nas]) / sum(wgts[non.nas])
					}
				})
		
		result <- t(t(result)) #turn into matrix
		
	} else {
		result <- t(apply(mx, COL, function (x) {
							#x <- mx[,6]
							if (!na.rm&any(is.na(x))) {
								rep(NA, length(unique(grpby)))
							} else if (na.rm&all(is.na(x))) {
								rep(NA, length(unique(grpby)))
							} else {
								non.nas <- !is.na(x)
								
								weightsGrouped <- aggregate(wgts * non.nas, by = list(grpby), FUN = sum, na.rm = na.rm)$x
								weightsGrouped[weightsGrouped == 0] <- NA
								aggregate(x * wgts, by = list(grpby), FUN = sum, na.rm = na.rm)$x / weightsGrouped
								#update - now will get NA if one group has all NAs
							}
							
						}))
		dimnames(result)[[COL]] <- sort(unique(grpby))	
	}
	
	structure(result, meta=c(varname=varname, grpby.tag = grpby.tag, set=attr(logiset,"desc")))
}





#' Calculates the weighted mean for each column of the matrix
#' Optionally subsetting and grouping by another (equal length/number of rows) variable,
#' which can be either a vector or a matrix.
#' Optionally having different weights for each column of the matrix (same length/number of rows))- wgts parameter 
#' which can also be either a vector or a matrix.
#'  The group-by variable may be time-invariant or time-variant.  
#' 
#' @param mx
#'  matrix or dataframe to calculate column means of 
#'  
#' @param grpby
#'  elements to group by, or NULL or unspecified to do no grouping. 
#'  Same length as the columns of mx.
#' If the group-by variable is time-invariant grpby can be provided as a vector or as a 
#' matrix with every column the same.  If the group-by variable is time-variable then 
#' grpby should be a matrix with number of columns equal to number of years. 
#' 
#' @param grpby.tag
#'  added to meta attribute of the result
#'
#' @param logiset
#'  logical vector indicating which observations to include, or NULL to include all.
#'
#' @param wgts
#'  elements to weight by, or NULL to do no weighting
#'  
#' @return 
#'  a matrix of means. Each row is the mean for a column in mx.
#'  if grpby is NULL, a single column of means. 
#'  if grpby is specified, a column of means for each group by category.
#' 
#' @export
#' @examples
#' mx <- matrix (c(1:10), ncol = 2) ; mx <- matrix (c(1:3, NA, 5:7, NA, 9:10), ncol = 2) 
#' logiset = NULL
#' grpby = NULL ; grpby.tag = NULL 
#' grpby <- c("A","A","A","B","B") ; grpby.tag = "AB" 
#' 
#' mx <- matrix (c(1:4, 5, 6:7,NA,NA,10), ncol = 2) 
#' wgts = rep(1, nrow(mx))
#' wgts =c(2,2,2,2,1)
#' grpby <- c(1,4,3,2,2) ;
#' 
#' wgts<-matrix(c(1,1,1,1,1,2,2,2,2,1), ncol = 2)
#' na.rm = FALSE ; na.rm = TRUE
mean_mx_cols_BCASO <- function (mx, grpby=NULL, grpby.tag=NULL, logiset=NULL, wgts=NULL, dict=NULL) {
		
	# 1. beginning check - that weight dimensions are correct
	if (!is.null(wgts)) {
		if (is.vector(wgts)) {
			if (nrow(mx) != length(wgts)) {
				param1Name <- as.character(sys.call())[2]
				stop(gettextf("Number of rows in %s != length of wgts", param1Name))
			}
		} else {
			if (nrow(mx) != nrow(wgts)) {
				param1Name <- as.character(sys.call())[2]
				stop(gettextf("Number of rows in %s != number of rows of wgts", param1Name))
			}
		}
	}      
	
	# 2. beginning check  - that grpby dimensions are correct   
	if (!is.null(grpby)) {
		if (is.vector(grpby)) {
			if(nrow(mx) != length(grpby)) {
				param1Name <- as.character(sys.call())[2]
				stop(gettextf("Number of rows in %s != length of grpby", param1Name))
			}
		} else {
			if (nrow(mx) != nrow(grpby)) {
				param1Name <- as.character(sys.call())[2]
				stop(gettextf("Number of rows in %s != number of rows of grpby", param1Name))
			}
		}	
	}
	
	#bulk of function starts   
	
	if (is.vector(wgts)) wgts <-matrix(rep(wgts, ncol(mx)), ncol=ncol(mx))
	if (is.vector(grpby)) grpby <-matrix(rep(grpby, ncol(mx)), ncol=ncol(mx))
	if (is.null(wgts)) wgts <- matrix(rep(1, length(mx)), ncol=ncol(mx))  #can't make this default param for some reason need to set here
	
	# save before subsetting mx
	varname <- attr(mx, "varname")
	
	# subset by logiset
	if (!is.null(logiset)) {
		if (is.vector(logiset)) {
			if (length(mx)!=length(logiset)) {
				stop("Check logiset subsetting in mean_mx_cols_BCASO()")
			}
			mx <- subset(mx, logiset)
			wgts <- subset(wgts, logiset)
			if (!is.null(grpby)) {
				grpby <- subset(grpby, logiset)
			}
		} else if (is.matrix(logiset)) {
			mx2 <- as.vector(mx)
			if (length(mx2)!=length(logiset)) {
				#logiset can be matrix with one column,  in thsi case we have to repeat out
				#logiset so the entire dataset gets subset rather than just the first column
				logiset <- rep(logiset, ncol(mx))
				logiset <- matrix(logiset, byrow=F, nrow=nrow(mx))
			}
			if (length(mx)!=length(logiset)) {
				stop("Check logiset subsetting in mean_mx_cols_BCASO()")
			}
			NA_index <- which(logiset==F)
			
			mx2[NA_index] <- NA
			mx <- matrix(mx2, byrow=F, nrow=nrow(mx))
			if (!is.null(grpby)) {
				grpby2 <- as.vector(grpby)
				grpby2[NA_index] <- NA
				grpby <- matrix(grpby2, byrow=F, nrow=nrow(grpby))
			}
		}
	}
	
	#concatenating all grpby columns into one vector
	if (!is.null(grpby)) {
		allgrpby=c(); j=1;while (j<=ncol(grpby)) {allgrpby=c(allgrpby,grpby[,j]);j=j+1}
	}
	
	if ( is.null(grpby) || sum(is.na(grpby)) == length(grpby) ) {
		
		result <- apply(matrix(1:ncol(mx),nrow=1), COL, function(i) {
					#i = 1
					x <- mx[,i]
					non.nas <-  !is.na(x)
					sum(x[non.nas] * wgts[non.nas,i]) / sum(wgts[non.nas,i])
				})
		
		result <- t(t(result))
	} else {
		#there is grouping
		result <- t(apply(matrix(1:ncol(mx),nrow=1), COL, function (i) {
			#i=14
			x <- mx[,i]
			non.nas <-  !is.na(x) 
			
			#if all numbers in a column of mx are NA, then return NA's for each grpby category,
			#otherwise continue the weigting procedure.
			#Using the weighting procedure on a column of NA's falls down as 
			#by=list(grpby[non.nas,i]) has length 0.
			
			if(all(is.na(x))) {
				
				z2<-t(rep("NA", length(unique(allgrpby))))
				return(z2)
			} else{      
				weightsGrouped <- aggregate(wgts[non.nas,i], by = list(grpby[non.nas,i]), FUN = sum)$x
				
				a <- aggregate(x[non.nas] * wgts[non.nas,i], by = list(grpby[non.nas,i]), FUN = sum)$x / weightsGrouped
				grp <- aggregate(wgts[non.nas,i], by = list(grpby[non.nas,i]), FUN = sum)$Group.1
				
				
				num_and_grp <- cbind(grp,a)
				#creating a matrix of NA's for those grpby's not present in the column
				#this enables each i'th step to return the same number of elements,
				#further helping to enable correct naming of the result later (by unique allgrpby)
				uall <- unique(allgrpby);
				uGOTsomeNOTNA <- unique(grpby[non.nas,i]);
				leftover <- uall[!(uall %in% uGOTsomeNOTNA)]
				nmat <- matrix(c(leftover,rep(NA,length(leftover))), ncol=2)
				colnames(nmat) <- c("grp","a")
				#combining the means for the i'th result with the matrix of NA's for those grpbys not present
				z <- rbind(nmat,num_and_grp)
				#ordering on the grpby, to maintain the correct ordering of grpby across all
				#ith steps - ensuring correct order in naming of the result later (by unique allgrpby)
				z2 <- z[order(z[,1]),]
				z2[,2]
			}
		}))
		if (sum(is(result[1,])=="character")>=1) {
			result <- matrix(as.numeric(result), ncol=ncol(result), nrow=nrow(result), byrow=F)
		}
		#if there is a column of NAs due to using a logiset then remove the last column which will only be NAs
		if (sum(is.na(result[,ncol(result)]))==nrow(result)) {
			result <- result[,-ncol(result)]
		}
		dimnames(result)[[COL]] <- sort(unique(allgrpby))
		
		if (is.null(dict)) {
			stop("Dict is NULL")
		} else 	if ((!is.null(grpby.tag))&(dict$dlookup_exists(grpby.tag)==1)) {
			colnames(result) <- c("Not in subgroup", "In subgroup")
		}
	}

	rownames(result) <- colnames(mx)
	
	structure(result, meta=c(varname=varname, grpby.tag = grpby.tag, set=attr(logiset,"desc"))) 
}


