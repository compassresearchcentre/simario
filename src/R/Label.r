# Functions to do with results labelling, ie: setting the name or colname of objects
# 
# Author: oman002
###############################################################################

#' set the columns names of each x in xlist to
#' each char vector c in colnameslist
#' 
#' @param xlist
#'  list
#' @param colnameslist
#'  the list of colnames
#' 
#' @return 
#'  a list with named column
#' 
#' @export 
#' @examples
#' xlist <- list(matrix(1:8,ncol=2), matrix(1:9,ncol=3))
#' colnameslist <- list(c("a","b"),c("c","d","e"))
#' labelColFromVec(xlist, colnameslist)
labelColFromVec <- function (xlist, colnameslist) {
	labelCol <- function(x, colnames) {dimnames(x)[[COL]] <- colnames;x}
	mapply(labelCol,
			xlist, colnameslist, SIMPLIFY = FALSE)
}	


#' names each object's column title with the 
#' object's name in the named list
#' 
#' @param xnamedlist
#'  list
#' 
#' @return 
#'  a named list with named column
#' 
#' @export 
labelColTitleFromList <- function(xnamedlist) {
	labelCol <- function(x,xname) { names(dimnames(x))[COL] <- c(xname);x }
	mapply(labelCol, xnamedlist, names(xnamedlist), SIMPLIFY = FALSE)
}


#' Set the colnames on each object in a list. By default
#' will only set if no exiting colnames (see onlyIfNull param).
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
#' @return 
#' a list with named colnames
#' 
#' @export 
#' @examples
#' \dontrun{
#' xlist <- env.base$years1_5$run_results_collated$means
#' xlist <- run_results_collated$means
#' x <- xlist$gptotvis
#' labelCols.list(xlist, "Mean")
#' }
labelCols.list <- function(xlist, xlabels = names(xlist), onlyIfNull = TRUE) {
	
	if (length(xlist) == 0) return(xlist)
	
	mapply(function(x, label) {
				if (!onlyIfNull || is.null(colnames(x))) {
					colnames(x) <- label
				}
				x
			},
			xlist, xlabels, SIMPLIFY = FALSE)
}


#' names the "along" dimension with "title"
#' eg: labelTitle(y, ROW, "Year") 
#' 
#' @param xm
#'  
#' @param along
#'  
#' @param title
#'  
#' 
#' @return 
#' NULL
#' 
labelTitle <- function (xm, along, title) {
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


