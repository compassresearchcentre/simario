# Matrix and array util functions.
# 
# Author: oman002
###############################################################################


a <- function (mylist) {
	## alias for toArray
	toArray(mylist)
}


#' Convert a list of matrices into an array, with each matrix in the z dimension.
#' 
#' @param listmx
#'  list of matrices. Each must matrix the same dimensions.
#' @return
#'  array with dimnames and meta from listmx
#' @examples
#'  listmx <- list(A=structure(matrix(c(1:6), nrow=2, dimnames=(list(1:2, 1:3))), meta=c("grpby.tag"="r1stchildethn")), B=structure( matrix(c(21:26), nrow=2, dimnames=(list(1:2, 1:3))) , meta=c("grpby.tag"="r1stchildethn")))
#'  as.array.listmx(listmx)
as.array.listmx <- function(listmx) {
	
	rows <- nrow(listmx[[1]]) ; rnames <- rownames(listmx[[1]])
	cols <- ncol(listmx[[1]]) ; cnames <- colnames(listmx[[1]])
	zdim <- length(listmx) ; znames <- names(listmx)
	structure( 
		array(	unlist(listmx), 
				dim = c(rows,cols,zdim),
				dimnames = list(rnames, cnames, znames)
		)
		, meta=attr(listmx[[1]], "meta"))
	
}

#' Align a list of matrices/vectors by row and column name, so all matrices have the 
#' same set of named rows and named cols. 
#' NAs are used to pad missing rows and cols. 
#' 
#' @param listmx
#'  a list of different sized matrices
#' 
#' @return
#'  the list of the matrices aligned to the maximal set of rows and cols.
#'  rows and cols are in numerical sort order.
#' 
#' @examples 
#' 
#' # list of vectors with same names
#' listmx2 <- list(A = c("0"=720L, "1"=353L), B = c("0"=722L, "1"=355L))
#' 
#' #' list of vectors with diff names
#' listmxa <- list(A = c("0"=720L, "1"=353L), B = c("1"=722L, "2"=355L))
#'  
#' # list of 2 4x1 matrices with same number of rows but different row names, and no col names 
#' listmx7 <- list(matrix(c(10,20,30,40), dimnames = list(c(1:4), NULL)), matrix(c(20,30,40,50), dimnames = list(c(2:5), NULL)))
#'
#' # list of 3x2 matrix and 4x2 matrix  
#' listmx8 <- list(matrix(c(10,20,30,40,50,60), nrow = 3, ncol = 2, dimnames = list(c("1","10","100"), c("b","a"))), matrix(c(20,30,40,50,60,70,80,90), nrow = 4, ncol = 2, dimnames = list(c("10","2","100","200"), c("b","c"))))
#' 
#' #' list of 5x1 table and 4x1 table
#' listmx9 <-  list(structure(c(1L, 15L, 3L, 27L, 1029L), .Dim = c(5L, 1L), .Dimnames = structure(list(    c("1", "2", "3", "4", "5"), NULL), .Names = c("", "")), class = "table"),    structure(c(11L, 1L, 18L, 1045L), .Dim = c(4L, 1L), .Dimnames = structure(list(        c("2", "3", "4", "5"), NULL), .Names = c("", "")), class = "table"))
#' 
#' #' list with a matrix with NA for a rowname
#' listmxa <- list(matrix(c(10,20), ncol = 1, dimnames = list(c("1","10"), NULL)), matrix(c(200,1000), ncol = 1, dimnames = list(c("2",NA), NULL)) )
#'
#' #' list of 2 matrices with meta
#' listmx10 <- list(structure(matrix(100, dimnames = list(1, NULL)), meta=c("grpby.tag"="r1stchildethn")), matrix(c(200,300), dimnames = list(c(1,2), NULL))) 
#' 
#' listmx11 <- list(numeric(0), structure(0.5, .Dim = c(1,1), .Dimnames = list(NULL, "Cat 1")))
#' listmx11 <- list(structure(numeric(0), .Dim=c(1,0)), structure(0.5, .Dim = c(1,1), .Dimnames = list(NULL, "Cat 1")))
#' listmx11 <- list(structure(0, .Dim=c(1,1)), structure(0.5, .Dim = c(1,1), .Dimnames = list(NULL, "Cat 1")))
#' 
#' listmx <- listmxa
#' listmx <- listmx2 
#' listmx <- listmx7
#' listmx <- listmx8
#' listmx <- listmx9
#' listmx <- listmxa
#' listmx <- listmx10
#' listmx <- listmx11
#' 
#' align.by.name.list.mx (listmx)
align.by.name.list.mx <- function(listmx) {
	
	listmx.is.vector <- sapply(listmx, is.vector)
	listmx.mx <- lapply(listmx, as.matrix)
	
	dims <-	sapply(listmx.mx, dim)
	all.colnames <- lapply(listmx.mx, colnames)
	all.rownames <- lapply(listmx.mx, rownames)
	
	nulls.colnames <- sapply(all.colnames, is.null)
	nulls.rownames <- sapply(all.rownames, is.null)
	
	if(any(nulls.colnames)) {
		# missing colnames, make sure all col dimensions are the same
		if (!all(dims[COL, ] == dims[COL, 1])) {
			stop(gettext("colnames not specified for a matrix and different number of columns"))
		}
	}
	
	if(any(nulls.rownames)) {
		# missing rownames, make sure all row dimensions are the same
		if (!all(dims[ROW, ] == dims[ROW, 1])) {
			stop(gettext("rownames not specified for a matrix and different number of rows"))
		}
	}
	
	# if rownames and colnames all the same just return listmx
	if (	all(sapply(all.colnames, identical, y=all.colnames[[1]])) && 
			all(sapply(all.rownames, identical, y=all.rownames[[1]]))) {
		return(listmx)			
	}
	
	# create the new matrix template, i.e: an ordered union of columns from all matrices
	# and rows from all matrices
	
	# NB: doesn't handle (yet) case where there is a NULL name but not all are NULL
	# NB: if NULL name then all matrices will be of the same dimension
	
	# if numeric names, sort numerically
	mt.rownames <- if (all(nulls.rownames)) NULL else nsort(unique(unlist(all.rownames)))
	mt.colnames <- if (all(nulls.colnames)) NULL else nsort(unique(unlist(all.colnames)))
	mt.dimnames <- list(mt.rownames, mt.colnames)
	
	# map existing matrix's columns and rows to new matrix template
	maps <- lapply(listmx.mx, function(mx) {
				#mx <- listmx.mx[[1]]
				
				#newcols specifies the source col for each col in matrix template
				#newcols specifies the source row for each row in matrix template
				newrows <- if (is.null(mt.rownames)) c(1:dim(mx)[ROW]) else match (mt.rownames, rownames(mx))
				newcols <- if (is.null(mt.colnames)) c(1:dim(mx)[COL]) else match (mt.colnames, colnames(mx)) 
				list(newrows=newrows, newcols=newcols)
			})
	
	
	result <- mapply(function(mx, map) {
				# mx <- listmx.mx[[1]] ; map <- maps[[1]]
				redim.mx(mx, map$newrows, map$newcols, mt.dimnames)
			}, listmx.mx, maps, SIMPLIFY = FALSE)
	
	# turn back into vectors if that's what we started with
	result[listmx.is.vector] <- lapply(result[listmx.is.vector], function(x) {
				#x <- result[[1]]
				vx <- as.vector(x)
				names(vx) <- rownames(x)
				vx
			})
	
	result
	
}

#' Append two lists of matrices by adding each element of listmx.src into 
#' the corresponding element of listmx.dest in the specified dimension.
#' 
#' @param listmx.dest
#' 	list of vector/matrix/arrays
#' 
#' @param listmx.src
#'  list of vector/matrix/arrays
#' 
#' @param into.dim
#'  the dimension in which to add each element of listmx.src.
#' 
#'  If into.dim = ZDIM (i.e: 3) then listmx.src elements will be added as an additional
#'  z matrix into each matrix/array element of listmx.dest. Existing elements of listmx.dest
#'  will be in the first z dim(s). 
#' 
#'  If into.dom = COL (i.e: 2) then listmx.src elements will be added as the 2nd
#'  col into each vector/matrix element of listmx.dest. Existing elements of listmx.dest
#'  will be in the first column(s).
#' 
#'  If into.dom = ROW (i.e: 1) then listmx.src elements will be added as the 2nd
#'  row into each vector/matrix element of listmx.dest. Existing elements of listmx.dest
#'  will be in the first row(s).  
#'  
#' @examples
#'
#' listmx.dest <- mxlist
#' listmx.src <- results
#' append.list.mx(listmx.dest, listmx.src)
#'
#'  
#' listmx.dest <- list(a=c(1:3), b=c(1:2))
#' listmx.src <- list(a=c(11:13), b=c(11:12))
#' 
#' into.dim=ROW
#' result <- append.list.mx(listmx.dest, listmx.src, into.dim)
#' result <- append.list.mx(result, listmx.src, into.dim)
#' 
#' listmx.dest <- me.base
#' listmx.src <- me.scenario
#' into.dim=ROW 
#' 
#' listmx.dest <- structure(list(means = structure(c(5, 5, 9, 11, 15, 50), .Names = c("0%","20%", "40%", "60%", "80%", "100%")), errs = structure(c(0, 0,0, 0, 0, 0), .Names = c("0%", "20%", "40%", "60%", "80%", "100%"))))
#' listmx.src <- structure(list(means = structure(c(5, 5, 9, 11, 15, 50.5), .Names = c("0%","20%", "40%", "60%", "80%", "100%")), errs = structure(c(0, 0,0, 0, 0, 44.4717165766114), .Names = c("0%", "20%", "40%", "60%","80%", "100%"))))
#' append.list.mx(listmx.dest, listmx.src, into.dim)
append.list.mx <- function(listmx.dest, listmx.src, into.dim=ZDIM) {
	
	
	if (into.dim==ROW) {
		# abind doesn't maintain row seperation so use rbind instead
		# eg: rbind(listmx.dest[[1]], listmx.src[[1]])
		# eg: rbind(listmx.dest[[2]], listmx.src[[2]])
		combined <- mapply(rbind, listmx.dest, listmx.src, SIMPLIFY = FALSE)
	} else {
		# add sourcem in the specifide dimension to each element in listmx.dest
		# eg: abind(listmx.dest[[1]], listmx.src[[1]], along=ZDIM)
		combined <- mapply(abind, listmx.dest, listmx.src, MoreArgs=list(along=into.dim), SIMPLIFY = FALSE)
	}
	
	# add meta attribute back because abind kills it
	copyMeta.list(combined, listmx.src)
	
}

library(abind)
arrZAdd <- function(m, arrName) {
	#add the 2d matrix m to the 3d array specified by the string arrName
	#in the next z dimension slot
	#eg: arrZAdd(matrix(1:12,3,4), "arr3d")
	# m <- matrix(1:12,3,4)
	# dimnames(m) <- list(letters=c("A","B","C"), numbers=c("1st","2nd","3rd","4th"))
	# arrZAdd(m, "arr3d")
	
	if (length(dim(m)) != 2) {
		firstParamName <- as.character(sys.call())[2]
		stop(gettextf("'%s' must have 2 dimensions\n",firstParamName))
	}
	
	#create new NULL variable if arrName doesn't exist
	if (!exists(arrName)) {
		assign(arrName, NULL, envir = .GlobalEnv)
	}
	
	#get current value of arrName
	arr <- eval(parse(text=arrName))
	
	#bind m to the 3rd dimension of arr
	arr <- abind(arr,m,along=3)
	
	#add back names of dimension because they get lost in abind
	names(dimnames(arr)) <- names(dimnames(m))
	
	#keep meta attribute
	attr(arr, "meta") <- attr(m, "meta")	
	
	#save to arrName
	assign(arrName, arr, envir = .GlobalEnv)
}

#' Converts a list of same length vectors to a matrix.
#' Unlike as.matrix.default, it unlists first and
#' uses names from the first list element in the result.
#' 
#' @param byrow
#'  if TRUE, each list element becomes a row in the matrix
#'  else, each list element becomes a column in the matrix
#' 
#' @examples
#'  xlist <- runs.mean.cfreq$gpmorb
#' 
#'  xlist <- list(a=c("1st"=1,"2nd"=2, "3rd"=3), b=c(11:13))
#' 
#'  as.matrix.default(xlist)
#'  as.matrix(xlist)
#'  as.matrixFromList(xlist)
#'  as.matrixFromList(xlist, byrow = FALSE)
as.matrixFromList <- function (xlist, byrow = TRUE) {
	if (byrow) {
		matrix(unlist(xlist), 
				nrow = length(xlist),
				byrow = TRUE,
				dimnames=list(names(xlist), names(xlist[[1]])))
	} else {
		matrix(unlist(xlist), 
				nrow = length(xlist[[1]]),
				byrow = FALSE,
				dimnames=list(names(xlist[[1]]), names(xlist)))	
	}				
}

cAdd <- function(vec, arrName, colName) {
	#add the vector to the 2d array specified by the string arrName
	#in the next column slot
	#eg: cAdd(c(1,2,3,4,5), "arr2d", "col1")
	# vec <- c(1,2,3)
	# names(vec) <- c("a","b","c")
	# cAdd(vec, "arr2d", "col1")
	
	#create new NULL variable if arrName doesn't exist
	if (!exists(arrName)) {
		assign(arrName, NULL, envir = .GlobalEnv)
	}
	
	#get current value of arrName
	arr <- eval(parse(text=arrName))
	
	#bind vec to the arr with column name = colName
	arr <- cbind(arr, `colnames<-`(cbind(vec), colName))
	
	#keep meta attribute
	attr(arr, "meta") <- attr(vec, "meta")	
	
	#save to arrName
	assign(arrName, arr, envir = .GlobalEnv)
}

collapseZdim <- function (xarray) {
	#collapse the Z dimension of a 3D array into cols in a 2D array
	#eg: xarray <- runs.all.mean$all.base$o.gptotvis
	#eg: xarray <- runs.all.mean$all.by.gender.base$o.gptotvis
	#eg: collapseZdim(runs.all.mean$all.base$o.gptotvis)
	#eg: collapseZdim(runs.all.mean$all.by.gender.base$o.gptotvis)
	
	result <- apply(xarray, ZDIM, function(x) {x})
	
	#copy names, dimnames, and meta attribute to the result
	rows <- dim(xarray)[ROW]
	cols <- dim(xarray)[COL]
	zdims <- dim(xarray)[ZDIM]
	dim(result) <- c(rows, cols*zdims)
	dimnames(result)[[ROW]] <- dimnames(xarray)[[ROW]] 
	dimnames(result)[[COL]] <- rep(dimnames(xarray)[[COL]], zdims)
	names(dimnames(result)) <- names(dimnames(xarray))[-ZDIM] 
	attr(result, "meta") <- attr(xarray, "meta")
	
	result
}

#' Push row/col headers (ie: names of the rownames and colnames) into the rownames and colnames.
#' 
#' @param mx
#'  matrix
#' 
#' @examples
#'  mx <- matrix(1:4, nrow=2, ncol=2, dimnames=list(gender=c("M","F"), SES=c("Low","High")))
#'  mx <- matrix(1:4, nrow=2, ncol=2, dimnames=list(c("M","F"), SES=c("Low","High")))
#'  names(dimnames(mx))[[ROW]] <- NA
#'  dimnames.prepend.header(mx)
dimnames.prepend.header <- function(mx) {
	
	dx <- dimnames(mx)[ROW]
	header <- if(is.na(names(dx))) "" else names(dx)
	new.row.names <- paste(header, unlist(dx))
	
	dx <- dimnames(mx)[COL]
	header <- if(is.na(names(dx))) "" else names(dx)
	new.col.names <- paste(header, unlist(dx))
	
	structure(mx, dimnames = list(new.row.names, new.col.names))
}

#' Takes a list of lists of matrices and converts to array, by:
#'  flattens each matrix in a single row,
#'  aligns then single rows within each list, then combines the rows into a single matrix per list
#'  then aligns each single matrix and combines them in the z dimension of an array 
#' Preserves names and meta attribute.
#' 
#' @param lol.mx
#'  a list of lists of matrices, eg:
#'  List of 2
#'  $ year1:List of 2
#'  ..$ 1: 'table' int [1:2, 1:3] 1 2 3 4 5 6
#'  ..$ 2: 'table' int [1:2, 1:3] 21 22 23 24 25 26
#'  $ year2:List of 2
#'  ..$ 1: 'table' int [1:2, 1:3] 31 32 33 34 35 36
#'  ..$ 2: 'table' int [1:2, 1:3] 41 42 43 44 45 46
#' 
#'  NB: matrices can have different dimensions and are aligned first within a list, and then between the lists.
#'  @seealso align.by.name.list.mx
#' 
#' @examples
#'  
#' lol.mx <- .$runs$freqs$all.by.ethnicity$z1msmokeLvl1
#' lol.mx <- structure(list(year1 = structure(list(`1` = structure(c(1:6), .Dim = 2:3, .Dimnames = structure(list(0:1,1:3), .Names = c("", "")), class = "table", meta = c(grpby.tag="r1stchildethn")), '2'= structure(c(21:26), .Dim = 2:3, .Dimnames = structure(list(0:1,1:3), .Names = c("", "")), class = "table", meta = c(grpby.tag="r1stchildethn"))), .Names = 1:2), year2 = structure(list(`1` = structure(31:36, .Dim = 2:3, .Dimnames = structure(list(0:1,1:3), .Names = c("", "")), class = "table", meta = c(grpby.tag="r1stchildethn")),     `2` = structure(c(41:46), .Dim = 2:3, .Dimnames = structure(list(0:1,1:3), .Names = c("", "")), class = "table", meta = c(grpby.tag="r1stchildethn"))), .Names = c("1", "2"))))
#' 
#' flatten.lolmx(lol.mx)
#' 
#' , , year1
#' 
#'   1 0 1 1 2 0 2 1 3 0 3 1
#' 1   1   2   3   4   5   6
#' 2  21  22  23  24  25  26
#' 
#' , , year2
#' 
#'   1 0 1 1 2 0 2 1 3 0 3 1
#' 1  31  32  33  34  35  36
#' 2  41  42  43  44  45  46
#'
flatten.lolmx <- function(lol.mx) {
	
	# flatten each matrix into a single row then align and return each list of matrices as a single matrix
	lol.mx.flat <- lapply(lol.mx, flatten.listmx)
	
	# align matrices
	lol.mx.flat.aligned <- align.by.name.list.mx(lol.mx.flat)
	
	# combine into an array, with each matrix in the z dimension
	lol.mx.flat.aligned.array <- as.array.listmx (lol.mx.flat.aligned)
	
	# return with meta from first list of matrices
	structure(lol.mx.flat.aligned.array, meta=attr(lol.mx[[1]], "meta"))
	
}

#' Takes a list of matrices, flattens them into a single row, aligns them by column name
#' and then returns a single matrix.
#' 
#' @param listmx
#'  list of matrices to flatten. Can have different dimensions but must have similar names
#'  for alignment (@seealso listmx.flat.aligned)
#' @return 
#'  a single matrix with 
#'    colnames = the flattened row/col names of listmx[[1]]],
#'    meta = meta of listmx[[1]]
#'    rownames = names of listmx 
#' 
#' @examples
#'  listmx <- list(A=structure(matrix(c(1:6), nrow=2, dimnames=(list(1:2, 1:3))), meta=c("grpby.tag"="r1stchildethn")), B=structure( matrix(c(21:26), nrow=2, dimnames=(list(1:2, 1:3))) , meta=c("grpby.tag"="r1stchildethn")))
flatten.listmx <- function(listmx) {
	
	# flatten each matrix into a single row 
	listmx.flat  <- lapply(listmx, flatten.mx) 
	
	# align each row matrix 
	listmx.flat.aligned <- align.by.name.list.mx(listmx.flat)
	
	# collapse into a single matrix of all rows
	# keeps colnames and meta from first row matrix, use rownames from names of listmx.flat.aligned
	structure(
			matrix( unlist(listmx.flat.aligned), nrow=length(listmx.flat.aligned), byrow = T,
					dimnames=list(names(listmx.flat.aligned), colnames(listmx.flat.aligned[[1]])) )
			, meta=attr(listmx.flat.aligned[[1]], "meta") 
	)
	
}

#' Flatten a vector or matrix (r,c) into a single row matrix (1,c) 
#' creating new column names from a combination of old rownames 
#' and colnames. 
#' 
#' A single row matrix, unlike a vector, has the advantage that 
#' it can have a row name.
#'  
#' eg: A1, A2,
#'     B1, B2
#' 
#' @param row.names.first
#'  if TRUE, row.names will appear first in the names of the resultant columns, eg: becomes A1,B1,A2,B2
#'  else row.names will appear last, eg: 1A, 1B, 2A, 2B
#' 
#' @param mx
#'  matrix
#' 
#' @return
#'  a single row matrix (1,c)
#' 
#' @examples
#' 
#' # 2 x 3 matrix
#' mx <- matrix(c(1:6), nrow = 2, ncol = 3, dimnames=list(c("A", "B"), c("1", "2", "3")))
#' 
#' # 1 columned matrix (2 x 1) with no col names
#' mx <- matrix(c(10,20), nrow = 2, ncol = 1, dimnames=list(c("0", "1"), NULL))
#'
#' # 1 columned matrix (3 x 1) with no dim names
#' mx <- matrix(c(1,2,3), nrow = 3, ncol = 1)
#'
#' # 1 columned matrix (2 x 1) with no col names and NA in row name
#' mx <- matrix(c(10,20), nrow = 2, ncol = 1, dimnames=list(c("0", NA), NULL))
#'
#' # 1 columned matrix (1 x 1) with no col names and NA in row name
#' mx <- matrix(c(10), nrow = 1, ncol = 1, dimnames=list(c(NA), NULL))
#' 
#' # vector
#' mx <- c("a"=1,"b"=2,"c"=3)
#' 
#' flatten.mx(mx)
flatten.mx <- function (mx, row.names.first = FALSE) {
	# if already vector, return as single row matrix
	if (is.vector(mx)) {
		return ( matrix(mx, nrow=1, dimnames= list(NULL, names(mx))) )
	}
	
	# flatten into vector
	mx.flat <- structure(mx, .Dim=c(1,length(mx)))
	#mx.flat <- as.vector(mx)
	
	# create new column names, as a combination of the old row and col names
	rnames <- if (is.null(rownames(mx))) c("") else rownames(mx)
	cnames <- if (is.null(colnames(mx))) c("") else colnames(mx)
	cnames <- if (row.names.first || ncol(mx) == 1) {
				trim(paste(rep(rnames,length(cnames)), sapply(cnames, rep, length(rnames))))
			} else {
				trim(paste(sapply(cnames, rep, length(rnames)), rep(rnames,length(cnames))))
			}
	
	#replace "NA" with NA
	cnames[cnames == "NA"] <- NA
	
	if (any(is.na(cnames)) || !cnames[1] == c("")) {
		colnames(mx.flat) <- cnames
	}
	
	mx.flat
}


#' Create a matrix of NA with specified col/row names/lengths.
#' 
#' @param rows row names, or a numeric scalar for the number of rows
#' @param cols columns names, or a numeric scalar for the number of cols
#' 
#' @examples
#' cols <- 2 ; rows <- 5
#' cols <- names(codings$smoke); rows <- 5
#' cols <- names(codings$smoke); rows <- paste("Year", 1:5)
#' 
#' namedMatrix(rows, cols)
namedMatrix <- function (rows, cols) {
	nrows <- if (is.numeric.scalar(rows)) rows else length(rows)
	ncols <- if (is.numeric.scalar(cols)) cols else length(cols)
	rownames <- if (is.numeric.scalar(rows)) NULL else rows
	colnames <- if (is.numeric.scalar(cols)) NULL else cols
	matrix(nrow=nrows, ncol=ncols,
			dimnames=list(rownames,colnames))
}


#' Takes any number of 2D matrices, each with the same number of cols but with 
#' any set of named rows. Every col 1 of the matrices are combined into a new 
#' matrix, as are every col 2,3 etc.
merge.list.mx.by.cols <- function(...) {
	merge.list.mx.by.rows(lapply(listmx, t))
}

#' Takes any number of 2D matrices, each with the same number of rows but with 
#' any set of named cols. Every row 1 of the matrices are combined into a new 
#' matrix, as are every row 2,3 etc.
#'
#' @examples
#'  	xlistm <- list(matrix(c(1,5,2,6,3,7,4,8),nrow=2,dimnames=list(c(),c("a","b","c","d"))),matrix(c(11,15,14,18,20,21),nrow=2,dimnames=list(c(),c("a","d","e"))),matrix(c(13,17,30,31,40,41),nrow=2,dimnames=list(c(),c("c","e","f"))))
#'		xlistm
#'		[[1]]
#'		a b c d
#'		[1,] 1 2 3 4
#'		[2,] 5 6 7 8
#'
#'		[[2]]
#'		a  d  e
#'		[1,] 11 14 20
#'		[2,] 15 18 21
#'
#'		[[3]]
#'		c  e  f
#'		[1,] 13 30 40
#'		[2,] 17 31 41
#'		
#' (matrices = 3, cols = x*, rows = 2)
#' 
#' Cols in the matrices are joined on column name to produce a list of y matrices, with z cols, and a unique merged
#' set of rows, eg: matrices = 2, cols = 3, rows = x*, eg:
#' > merge.list.mx.by.rows(xlistm)
#' [[1]]
#'       a  b  c  d  e  f
#' [1,]  1  2  3  4 NA NA
#' [2,] 11 NA NA 14 20 NA
#' [3,] NA NA 13 NA 30 40
#' 
#' [[2]]
#'       a  b  c  d  e  f
#' [1,]  5  6  7  8 NA NA
#' [2,] 15 NA NA 18 21 NA
#' [3,] NA NA 17 NA 31 41
#' 
#' Another example:	
#' xlistm <- runs.all.cfreq$gpmorb
#' xlistm <- list(first=matrix(1:6, nrow=2, dimnames=list(c("means", "errs"),c("C","A","B"))), second=matrix(11:16, nrow=2, dimnames=list(c("means", "errs"),c("C","A","B"))))
#' merge.list.mx.by.rows(xlistm)
merge.list.mx.by.rows <- function(...) {
	xlistm <- if (nargs() > 1) list(...) else (...)

	# if colnames are all the same, then use them as is
	colnames1 <- colnames(xlistm[[1]])
	if (all(sapply(xlistm, function(mx) {
						#mx <- xlistm[[1]]
						identical(colnames(mx), colnames1)
			}))) {
		colset <- colnames1
	} else {
		# else get the unique set of colnames across each element in list xlistm
		colset <- nsort ( unique( as.vector( unlist(sapply(xlistm, colnames))) ) )
	}
	
	# for each row j in xlistm[[1]]
	result <- lapply(seq(nrow(xlistm[[1]])), function (j) {
				#j = 1
				#merge corresponding row j in each element of xlistm into colset
				newm <- sapply(xlistm, 
						function (mx)	{
							#mx <- xlistm[[1]]
							mx[j,][match(colset, colnames(mx))]
							#merge(colset, data.frame(colnames(mx), mx[j,]), by = 1, all.x=TRUE)$mx 
						}
				)
				
				colnames(newm) <- names(xlistm)
				rownames(newm) <- colset
				t(newm)
			})
	names(result) <- rownames(xlistm[[1]])
	result
}



#' Re-dimension a matrix.
#' Takes the matrix mx and transforms it into a length(newrows) x length(newcols) matrix
#' which has the rows from mx specified in newrows and the cols from mx specified in newcols.
#' 
#' @param mx
#'  source matrix
#' 
#' @param newrows
#'  a vector specifing the rows from the source matrix that will appear
#'  in the new matrix. The new matrix will have length(newrows) rows.
#'  The position a source row will have in the new matrix is their index in newrows. 
#'  A NA in this vector will create a row of NAs at the position in the new matrix.
#'  eg: 
#'  c(2,NA,1) will create a new matrix of 3 rows. 
#'  Row 1 of the new matrix will be row 2 of the source matrix
#'  Row 2 of the new matrix will be NAs.
#'  Row 3 of the new matrix will be row 1 of the source matrix.
#'  
#' @param newcols
#'  a vector specifing the cols from the source matrix that will appear
#'  in the new matrix. Works in the same manner as newrows but for columns.
#' 
#' @param dim.names
#'  A dimnames attribute for the matrix: NULL or a list of length 2 giving the row and column names respectively.
#'  An empty list is treated as NULL, and a list of length one as row names. 
#'  The list can be named, and the list names will be used as names for the dimensions.
#' 
#' @return 
#'  a new matrix of length(newrows) rows and length(newcols) cols with the
#'  source rows and cols as specified by newrows and newcols.
#'  If the source matrix is a table then will also return a table.
#' 
#' @examples
#' mx <- matrix(c(10,20,30,40,50,60), nrow = 3, ncol = 2, dimnames = list(c("a","b","c"), c(2:1)))
#' mx <- as.table(mx)
#' > mx
#'    2  1
#' a 10 40
#' b 20 50
#' c 30 60
#'
#' newcols <- c(2,1,NA)
#' newrows <- c(1,2,3,NA,NA,NA)
#' dim.names <- list(c(LETTERS[1:6]),1:3)
#'  
#' > redim.mx (mx, newrows, newcols, dim.names)
#'    1  2  3
#' A 40 10 NA
#' B 50 20 NA
#' C 60 30 NA
#' D NA NA NA
#' E NA NA NA
#' F NA NA NA
#'
#' mx <- listmx[[1]]
#'
#' # mx as a table 
#' mx <- structure(c(1L, 15L, 3L, 27L, 1029L), .Dim = c(5L, 1L), .Dimnames = structure(list(c("1", "2", "3", "4", "5"), NULL), .Names = c("", "")), class = "table")
#' newrows = 1:5
#' newcols = 1
#' dim.names = list(c("1", "2", "3", "4", "5"), NULL)
#' redim.mx(mx, newrows , newcols, dim.names)
#' 
#' # mx with meta
#' mx <- structure(matrix(100, dimnames = list(1, NULL)), meta=c("grpby.tag"="r1stchildethn"))
#' newrows = 1
#' newcols = 1
#' dim.names = list(1, NULL)
#' redim.mx(mx, newrows , newcols, dim.names)
redim.mx <- function(mx, newrows, newcols, dim.names) {
	numrows <- length(newrows)
	numcols <- length(newcols)
	rowindices <- rep(newrows, numcols)
	colindices <- rep(newcols, each=numrows)
	
	# indices into mx. 
	# the position of each index within mxindices, 
	# will be the new position in the final matrix, 
	# eg: the index at mxindices[1] specifies a value
	# that will end up in row 1 col 1 of the final matrix, 
	# the index at mxindices[2] specifies a value
	# that will end up in row 2 col 1 of the final matrix,
	# etc.
	mxindices <- rowindices + (colindices-1)*dim(mx)[1]
	newdata <- as.vector(mx)[mxindices]
	
	result <- matrix(newdata, nrow = numrows, ncol = numcols, dimnames = dim.names)
	
	if (is.table(mx)) {
		result <- as.table(result)
		dimnames(result) <- dim.names # because if colname is NULL don't what it to be "A"
	}
	
	# add back meta attribute
	structure(result, meta = attr(mx, "meta"))
}


#' Select only the specified row from each matrix in a list,
#' and return a list of row vectors.
#' 
#' @param mxlist
#'  list of matrices
#' @param na.rm
#'  if TRUE, then do not return any selected row that contains a NA
#' 
#' @examples 
#'  mxlist <- list(mx1=matrix(1:10, nrow=2), mx2=matrix(c(1,2,NA,4), nrow=2))
#'  mxlist <- list(mx1=matrix(1:10, nrow=2)) 
#'  rownum <- 1
#'  select.row.list.mx(mxlist, rownum, na.rm = F)
#'  select.row.list.mx(mxlist, rownum, na.rm = T)
select.row.list.mx <- function(mxlist, rownum, na.rm = T) {
	
	result <- lapply(mxlist, function (mx) {
				# mx <- mxlist[[1]]
				# mx <- mxlist[[2]]
				selected.row <- mx[rownum, ]
				if (na.rm && any(is.na(selected.row))) NA else selected.row	
			})
	
	if (na.rm) result <- result[!is.na(result)]
	result
	
}


toArray <- function (mylist) {
	## convert list of vectors to an array
	t(array(unlist(mylist), dim=c(length(mylist[[1]]),length(mylist))))
}


