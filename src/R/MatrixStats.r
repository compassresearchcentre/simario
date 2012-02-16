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

#' Mean across Z dimension of a 3D array.
#' Preserves meta atrribute.
#' 
#' @param xa
#'  an array with a Z dimension
#' @param CI
#'  if TRUE, lower and upper confidence intervals are also returned
#' @param NA.as.zero
#'  if TRUE (default), treat NAs as if they are zeros
#' 
#' @return
#'  a matrix of means across the Z dimension
#' 
#' @export
#' @examples 
#' \dontrun{
#' xa <- env.base$years1_5$runs$means$all$gpmorb
#' xa <- env.base$years6_13$runs$means$all$burt
#' xa <- env.base$years1_5$runs$means$all.by.SESBTH.2cat$gpmorb
#' xa <- years1_5$runs$means$all.by.gender$gptotvis
#' 
#' xa <- flatten.lolmx(env.scenario$modules$years6_13$runs$cfreqs[["cond"]])
#' 
#' xa <- env.base$years1_5$runs$means$all.by.ethnicity$gptotvis
#' xa <- env.base$years1_5$runs$means$all.by.gender$gptotvis
#' 
#' result <- mean.array.z(xa)
#' }
mean.array.z <- function (xa, CI = TRUE, NA.as.zero = T) {
	if (NA.as.zero) xa[is.na(xa)] <- 0
	
	result <- apply(xa, c(ROW,COL), mean)
	numRuns <- dim(xa)[ZDIM]
	
	# CIs only make sense if more than 1 run
	if (CI && numRuns > 1) {
		
		#calculate error of each row
		errRuns <- apply(xa,c(ROW,COL),err)
		
		#calculate left CI
		leftRuns <- result - errRuns
		
		#calculate right CI
		rightRuns <- result + errRuns
		
		#add left and right runs to the right hand side of result
		resultCI <- cbind(Mean=result,  
				Lower=leftRuns, Upper=rightRuns)
		#dimnames(result)[[2]] <- c("Mean", "Lower", "Upper")
		
		# reorder so that lower and upper is next to the mean of each grouping
		numGroups <- dim(xa)[COL]
		reordering <- as.vector(sapply(c(1:numGroups), function (x) { seq(from=x, length.out=3, by=numGroups)}))
		resultCI <- resultCI[, reordering]
		
		colnames(resultCI) <- paste(colnames(resultCI), rep(c("Mean", "Lower", "Upper"), numGroups))
		names(dimnames(resultCI)) <- names(dimnames(result))
		result <- resultCI
	}
	
	#keep meta attribute
	attr(result, "meta") <- attr(xa, "meta")
	
	result
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
#' prop.table.grpby(x, grpby)
prop.table.grpby <- function (x, grpby, na.rm=TRUE) {
	grpsum <- tapply(x, grpby, sum, na.rm=na.rm)
	#x / grpsum[grpby]
	structure(as.vector(x / grpsum[grpby]), .Names=names(x))
}

#' Execute quantile on the columns of a matrix.
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
#' quantile.mx(mx)
#' quantile.mx(mx, probs=seq(0, 1, 0.2), na.rm = TRUE)
#' quantile.mx(mx, new.names=c("Min", "20th", "40th", "60th","80th","Max"), probs=seq(0, 1, 0.2), na.rm = TRUE)
#' quantile.mx(mx, probs=seq(0, 1, 0.02), na.rm = TRUE)
#' }
quantile.mx <- function (mx, new.names=NULL, ...) {
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

#' Execute summary on the columns of a matrix.
#' 
#' @param mx
#'  matrix
#' 
#' @return
#'  summary of each column, returned as a row
#' 
#' @export
#' @examples
#' \dontrun{
#' mxc <- env.base$years6_13$outcomes[["cond"]]
#' mx <- env.base$years1_5$outcomes[["gptotvis"]]
#' summary.mx(mxc)
#' summary.mx(mx)
#' }
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
#' @export
#' @examples
#' \dontrun{
#' xlist <- env.base$years6_13$outcomes
#' indices <- names(env.base$years6_13$runs$means$all)
#' summary.mx.list(xlist, indices)
#' } 
summary.mx.list <- function (xlist, indices) {
	lapply.subset(xlist, indices, summary.mx)
}

#' Frequency table, with option to group results.
#'  
#' @param x
#'  vector of values which can be interpreted as factors 
#' @param grpby
#'  elements to group by, or NULL to do no grouping
#' @param useNA
#'  whether to include extra NA levels in the table.
#'  one of c("no", "ifany", "always"). Defaults to "ifany".
#' 
#' @return
#'  a table. If grpby is specified this will be a 2D table
#'  with columns that are the group by and rows the categories. 
#'  If grpby = NULL then a 2D table with 1 column and rows as categories.
#' 
#' @export
#' @examples
#' 
#' #x <- env.base$years1_5$outcomes$z1accomLvl1[,1]
#' #grpby <- env.base$years1_5$outcomes$z1gender
#' #grpby.tag <- "z1gender"
#' 
#' 
#' x <- rep(0,1075)
#' x <- c(8,8,2,1,1,8)
#' grpby <- c('M','M','F','F','F','F')
#' grpby.tag <- "gender"
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

#' Executes table.grpby on the cols of a matrix.
#' 
#' @param mx
#'  matrix
#' 
#' @param grpby
#'  a vector of elements to group by, or NULL or unspecified to do no grouping
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
#'  list, each element of the list represents a year, 
#'  each element is a 2D matrix where columns are the group by
#'  and rows the categories. If grpby = NULL then 
#'  a 1D matrix with columns as categories is returned.
#'
#' @export 
#' @examples
#' mx <- matrix(c(8,2,2,2,8,2,3,2,3,2,2,4,8,2,3,4,2,2,4,3),nrow=4,ncol=5,dimnames=list(NULL, LETTERS[1:5]))
#' grpby <- c('M','F','F','M')
#' table.grpby.mx.cols(mx, grpby)
#' logiset <- c(FALSE, TRUE, FALSE, TRUE)
#' table.grpby.mx.cols(mx, grpby = grpby, logiset = logiset)
#' 
#' \dontrun{
#' mx <- env.base$years1_5$outcomes$z1accomLvl1
#' grpby <- env.base$years1_5$outcomes$z1gender
#' r1 <- table.grpby.mx.cols(mx, grpby)
#'  
#' mx <- outcomes[[1]]
#' grpby=children$r1stchildethn ; grpby.tag="r1stchildethn"
#' r2 <- table.grpby.mx.cols(mx, grpby, grpby.tag)
#' 
#' mx <- env.base$modules$years1_5$outcomes$z1kidsLvl1
#' grpby = NULL
#' r3 <- table.grpby.mx.cols(mx)
#' }
table.grpby.mx.cols <- function(mx, grpby = NULL, grpby.tag = NULL, logiset = NULL, useNA = "ifany") {
	
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
	
	#use alply instead of apply because apply simplifies
	#results.by.col <- alply(mx, COL, function (x) {
	#			# x <- mx[,1]
	#			table.grpby(x,grpby,grpby.tag)
	#		})
	
	#use lapply instead of apply because apply simplifies
	#use lapply instead of alply so we don't have the split attributes
	results.by.col <- lapply(1:ncol(mx), function (i) {
				#i <- 1
				table.grpby(mx[,i], grpby, useNA = useNA)
			})
	
	# add names 
	names(results.by.col) <- dimnames(mx)[[COL]]
	
	# return with meta
	structure(results.by.col, meta=c(varname=attr(mx, "varname"), grpby.tag = grpby.tag, set=attr(logiset,"desc")))
	
}


#' Calculates the weighted mean for each column of the matrix
#' optionally subsetting first and grouping by another (equal length) variable.
#' 
#' @param mx
#'  matrix or dataframe to calculate column means of 
#'  
#' @param logiset
#'  logical vector indicating which observations to include, or NULL to include all.
#'
#' @param wgts
#'  elemets to weight by, or NULL to do no weighting
#'
#' @param grpby
#'  elements to group by, or NULL or unspecified to do no grouping
#' 
#' @param grpby.tag
#'  added to meta attribute of the result
#' 
#' @param na.rm
#'  logical. Should missing values be removed?  
#' 
#' @return 
#'  if grpby is NULL, vector of means for each column
#'  if grpby is specified, a matrix of means for each column grouped
#' 
#' @export
#' @examples
#' \dontrun{
#' 	mx <- children$o.gptotvis
#' 	wgts <- children$weight
#' 	by <- children$z1gender
#' 
#' wtdmeancols(children$o.gptotvis, children$weight, children$z1gender)
#' wtdmeancols(children$o.gptotvis, children$weight)
#' 
#' mx <- xframeset[[varname]]
#' wgts <- xframeset[[wgtsname]]
#' na.rm = T
#' wtdmeancols(mx)
#' wtdmeancols(mx,wgts, grpby)
#'
#' mx <- env.base$modules$years1_5$outcomes$gptotvis ; logiset = childsets$males
#' mx <- X[[1]]; logiset=lol.a$logiset; wgts = NULL; grpby = NULL; grpby.tag = NULL
#' 
#' }
#' mx <- matrix (c(1:10), ncol = 2)
#' mx <- matrix (c(1:3, NA, 5:7, NA, 9:10), ncol = 2) ; logiset=NULL
#' grpby = NULL
#' grpby <- c("A","A","A","B","B") ; grpby.tag = "AB" 
#' wgts = rep(1, nrow(mx))
#' na.rm = FALSE ; na.rm = TRUE
#' 
#' 
#' wtdmeancols(mx, logiset=logiset, wgts=wgts, grpby=grpby, grpby.tag=grpby.tag, na.rm = na.rm)
#' 
wtdmeancols <- function (mx, logiset=NULL, wgts = NULL, grpby=NULL, grpby.tag = NULL, na.rm = F) {
	
	if (is.null(wgts)) wgts <- rep(1, nrow(mx))  #can't make this default param for some reason need to set here
	
	# save before subsetting mx 
	varname <- attr(mx, "varname")
	
	# subset
	if (!is.null(logiset)) mx <- subset(mx, logiset)
	if (!is.null(logiset)) wgts <- subset(wgts, logiset)
	if (!is.null(logiset)) grpby <- subset(grpby, logiset)
	
	if (is.null(grpby)) {
		result <- apply(mx, COL, function(x) {
					#x <- mx[,2]
					non.nas <- if (na.rm) !is.na(x) else rep(T, length(x))
					sum(x[non.nas] * wgts[non.nas]) / sum(wgts[non.nas])
				})
		
		result <- t(t(result))
		
	} else {
		result <- t(apply(mx, COL, function (x) {
							#x <- mx[,1]
							non.nas <- if (na.rm) !is.na(x) else rep(T, length(x))
							
							weightsGrouped <- aggregate(wgts[non.nas], by = list(grpby[non.nas]), FUN = sum)$x
							
							aggregate(x[non.nas] * wgts[non.nas], by = list(grpby[non.nas]), FUN = sum)$x / weightsGrouped
							
						}))
		dimnames(result)[[COL]] <- sort(unique(grpby))	
	}
	
	structure(result, meta=c(varname=varname, grpby.tag = grpby.tag, set=attr(logiset,"desc")))
}

