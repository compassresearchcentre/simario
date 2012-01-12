# Calculations performed on lists and matrices of values.
# 
# Author: oman002
###############################################################################


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
#' 
#' @return
#'  a matrix of means across the Z dimension
#' 
#' @examples 
#' 
#' xa <- env.base$years1_5$runs$means$all$gpmorb
#' xa <- env.base$years6_13$runs$means$all$burt
#' xa <- env.base$years1_5$runs$means$all.by.SESBTH.2cat$gpmorb
#' xa <- years1_5$runs$means$all.by.gender$gptotvis
#' 
#' 
#' xa <- env.base$years1_5$runs$means$all.by.ethnicity$gptotvis
#' xa <- env.base$years1_5$runs$means$all.by.gender$gptotvis
#' 
#' mean.array.z(xa)
mean.array.z <- function (xa, CI = TRUE) {
	result <- apply(xa, c(1,2), mean)
	numRuns <- dim(xa)[3]
	
	# CIs only make sense if more than 1 run
	if (CI && numRuns > 1) {
		
		#calculate error of each row
		errRuns <- apply(xa,c(1,2),err)
		
		#calculate left CI
		leftRuns <- result - errRuns
		
		#calculate right CI
		rightRuns <- result + errRuns
		
		#add left and right runs to the right hand side of result
		resultCI <- cbind(Mean=result,  
				Lower=leftRuns, Upper=rightRuns)
		#dimnames(result)[[2]] <- c("Mean", "Lower", "Upper")
		
		# reorder so that lower and upper is next to the mean of each grouping
		numGroups <- dim(xa)[2]
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
#' test <- lapply(list(listmx1,listmx2,listmx3,listmx4, listmx5, listmx6, listmx7, listmx8, listmx9, listmx10), mean.list.mx)
#'  
#' mean.list.mx(listmx)
#' mean.list.mx(listmx1)
#' mean.list.mx(listmx2)
#' mean.list.mx(listmx3)
#' mean.list.mx(listmx4)     
#' mean.list.mx(listmx5)
#' mean.list.mx(listmx6)
#' mean.list.mx(listmx7)
#' mean.list.mx(listmx8)
#' mean.list.mx(listmx9)
#' mean.list.mx(listmx10)
mean.list.mx <- function(listmx) {
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
#' 
#' @return 
#'  vector of proportions, calculated according to group
#' 
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

propWtdtable <- function (variable, wgts) {
	#return proportions of variable weighted
	#eg: propWtdtable(people$sex, people$weightBase)
	prop.table(wtdtable(variable, wgts))
}

#' Execute quantile on the columns of a matrix.
#' 
#' @param mx
#'  matrix
#' @param new.names
#'  if specified, the names of the result will be 
#'  set to this vector
#' 
#' @return
#'  quantile of each column returned as a row
#' 
#' @examples 
#'  
#' mx <- years6_13$outcomes[["cond"]]
#' mx <- env.base$modules$years1_5$outcomes[["hadmtot"]]
#' mx <- env.base$modules$years6_13$outcomes[["cond"]]
#' mx <- env.base$modules$years1_5$outcomes[["gptotvis"]]
#' quantile.mx(mx)
#' quantile.mx(mx, probs=seq(0, 1, 0.2), na.rm = TRUE)
#' quantile.mx(mx, new.names=c("Min", "20th", "40th", "60th","80th","Max"), probs=seq(0, 1, 0.2), na.rm = TRUE)
#' quantile.mx(mx, probs=seq(0, 1, 0.02), na.rm = TRUE)
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

#' Frequency table, with option to group results.
#'  
#' @param grpby
#'  elements to group by, or NULL to do no grouping
#' 
#' @return
#'  a table. If grpby is specified this will be a 2D table
#'  with columns that are the group by and rows the categories. 
#'  If grpby = NULL then a 2D table with 1 column and rows as categories.
#' 
#' @examples
#' 
#' x <- env.base$years1_5$outcomes$z1accomLvl1[,1]
#' grpby <- env.base$years1_5$outcomes$z1gender
#' grpby.tag <- "z1gender"
#' 
#' 
#' x <- rep(0,1075)
#' x <- c(8,8,2,1,1,8)
#' grpby <- c('M','M','F','F','F','F')
#' grpby.tag <- "gender"
#' 
#' table.grpby(x)
#' table.grpby(x, grpby, grpby.tag)
table.grpby <- function (x, grpby = NULL) {
	if (is.null(grpby)) {
		t(t(table(x, useNA = "ifany", deparse.level = 0)))
	} else {
		
		if(length(x) != length(grpby)) {
			stop(gettextf("Length of %s != length of %s", 
							as.character(sys.call())[2], as.character(sys.call())[3]))
		}
		
		table(x, grpby, useNA = "ifany", deparse.level = 0)
	}
}

#' Executes table.grpby on the cols of a matrix.
#' 
#' @param mx
#'  matrix
#' 
#' @param grpby
#'  elements to group by, or NULL or unspecified to do no grouping
#' 
#' @param grpby.tag
#'  if specified this value with be attached as the
#'  meta attribute "grpby.tag"
#'  
#' @return
#'  list, each element of the list represents a year, 
#'  each element is a 2D matrix where columns are the group by
#'  and rows the categories. If grpby = NULL then 
#'  a 1D matrix with columns as categories is returned.
#' 
#' @examples
#' mx <- env.base$years1_5$outcomes$z1accomLvl1
#' grpby <- env.base$years1_5$outcomes$z1gender
#' r0 <- table.grpby.mx.cols(mx, grpby)
#'  
#' mx <- matrix(c(8,2,2,2,8,2,3,2,3,2,2,4,8,2,3,4,2,2,4,3),nrow=4,ncol=5,dimnames=list(NULL, LETTERS[1:5]))
#' grpby <- c('M','F','F','M')
#' r1 <- table.grpby.mx.cols(mx, grpby)
#' 
#' mx <- outcomes[[1]]
#' grpby=children$r1stchildethn ; grpby.tag="r1stchildethn"
#' r2 <- table.grpby.mx.cols(mx, grpby, grpby.tag)
#' 
#' mx <- env.base$modules$years1_5$outcomes$z1kidsLvl1
#' grpby = NULL
#' r3 <- table.grpby.mx.cols(mx)
table.grpby.mx.cols <- function(mx, grpby = NULL, grpby.tag = NULL) {
	
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
				table.grpby(mx[,i], grpby)
			})
	
	# add names 
	names(results.by.col) <- dimnames(mx)[[COL]]
	
	# return with meta
	structure(results.by.col, meta=c(varname=attr(mx, "varname"), grpby.tag = grpby.tag))
	
}

#' Weighted frequency table
#'
#' x <- c(8,8,2,1,1,8)
#' wgts <- c(1,1,2,1,1,1)
#' 
#' wtdtable(x, wgts)
#' 
#' 1 2 8 
#' 2 2 3
#' 
#' x <- env.base$years1_5$outcomes$z1accomLvl1[,1]
#' grpby <- env.base$years1_5$outcomes$z1gender
#' 
#' table(x, useNA = "ifany")
#' table(x,grpby) 
#' 
#' wtdtable(x, wgts)
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
#' @param mx
#'  matrix
#' 
#' @param wgts
#'  weights. If unspecified, a default weightof 1 is used for each row
#' 
#' @param addVariableName
#'  if addVariableName = TRUE, then the columns will be given the name of mx
#' 
#' @examples
#' 
#' wgts = rep(1,nrow(mx))
#' addVariableName = FALSE
#' 
#' mx <- matrix(c(8,2,2,2,8,2,3,2,3,2,2,4,8,2,3,4,2,2,4,3),nrow=4,ncol=5)
#' 
#' wtdtablecols(mx)
wtdtablecols <- function(mx, wgts = rep(1,nrow(mx)), addVariableName = FALSE) {
	
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

#' Executes wtdtablecols on each varname in xframe and return the results as a list.
#' Add a "meta" attributed that specifies the weighting variable name (ie: wgtsname).
#' 
#' @param xframe
#'  data frame
#' @param varnames
#'  variables to calculate wtdtablecols on
#' @param wgtsname
#'  weighting variable in xframe to use for weighting
#' 
#' @examples
#' 
#' xframe <- env.base$years1_5$outcomes
#' varnames <- names(env.base$years1_5$runs$cfreqs)
#'
#' varnames <- names(existingMatrices)
#'  
#' w <- wtdtablecols.list(xframe, varnames, wgtsname = "weightBase") 
wtdtablecols.list <- function (xframe, varnames, wgtsname="weightBase") {
	if (is.null(xframe[[wgtsname]])) {
		firstParamName <- as.character(sys.call())[2]
		stop(gettextf("Weighting variable '%s' does not exist in '%s'", wgtsname, firstParamName))
	}
	
	if (is.null(varnames)) {
		paramName <- as.character(sys.call())[3]
		stop(gettextf("Varnames '%s' are NULL.", paramName))
	}
	
	# get frequency tables for all varnames  
	results <- lapply.subset(xframe, varnames, wtdtablecols, wgts=xframe[[wgtsname]])
	
	# add meta with weighting
	results <- lapply(results, structure, meta=c(weighting=wgtsname))
	
	results
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
#' @examples
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
#' mx <- matrix (c(1:10), ncol = 2)
#' mx <- matrix (c(1:3, NA, 5:7, NA, 9:10), ncol = 2) ; logiset=NULL
#' grpby <- c("A","A","A","B","B") ; grpby.tag = "AB" 
#' grpby = NULL
#' wgts = rep(1, nrow(mx))
#' na.rm = T ; na.rm = F
#' 
#' mx <- env.base$modules$years1_5$outcomes$gptotvis ; logiset = childsets$males
#' mx <- X[[1]]; logiset=lol.a$logiset; wgts = NULL; grpby = NULL; grpby.tag = NULL
#' 
#' wtdmeancols(mx, logiset=logiset, wgts=wgts, grpby=grpby, grpby.tag=grpby.tag, na.rm = F)
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

#' Calculates the mean of each column (ie: wtdmeancols) of xlist[[varname]],
#' names the columns, and labels the result with a meta attribute.
#' 
#' If logiset is not null, then xlist is firstly subsetted and will
#' only include those obs that have indexed with a TRUE value in the logiset
#' logical vector.
#' 
#' If grpbycoding is not null, then grouping is applied before calculating
#' the mean.
#' 
#' @param mx
#'  matrix or dataframe to calculate column means of 
#'  
#' @param logiset
#'  logical vector indicating which observations to include, or NULL to include all.
#'  Can contain a "desc" attribute which describes the set.
#'
#' @param wgts
#'  elemets to weight by, or NULL to do no weighting
#'
#' @param grpby
#'  elements to group by, or NULL or unspecified to do no grouping
#' 
#' @param grpby.tag
#'  if specified this value with be attached as the
#'  meta attribute "grpby.tag"
#'  
#' @examples
#' mx <- env.base$modules$years1_5$outcomes$gptotvis ; mx <- xframe$gptotvis
#' logiset <- childsets$females
#' wgts <- NULL; wgts <- children$weightBase ; wgts <- rep(1, nrow(mx))
#' grpby = NULL; grpby.tag = NULL
#' grpby <- children$r1stchildethn ;  grpby.tag <- "r1stchildethn"
#' grpby <- children$z1gender ;  grpby.tag <- "z1gender"
#' 
#' mx <- X[[1]]; logiset=lol.a$logiset; wgts = NULL; grpyby = NULL; grpby.tag = NULL
#' 
#' wtdmeancols.lbl(mx, logiset=logiset, grpby=grpby, grpby.tag=grpby.tag)
#' wtdmeancols.lbl(mx, logiset, wgts, grpby, grpby.tag)
#' 
wtdmeancols.lbl <- function (mx, logiset = NULL, wgts = NULL, grpby = NULL, grpby.tag = NULL) {
	
	if (is.null(wgts)) wgts <- rep(1, nrow(mx))  #can't make this default param for some reason need to set here
	
	grpdesc <- NULL
	result <- wtdmeancols(mx, logiset, wgts, grpby)
	
	if (!is.null(grpby)) {
		# set column names
		grpdesc <- dict[[grpby.tag]]
		dimnames(result)[[COL]] <- names(codings[[grpby.tag]])
		names(dimnames(result))[[COL]] <- grpdesc
	}
	
	
	# return with meta
	structure(result, meta=c(varname=attr(mx, "varname"), grpby.tag = grpby.tag, set=attr(logiset,"desc")))
}

