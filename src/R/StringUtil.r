# String util functions
# 
# Author: oman002
###############################################################################

#' Adds a trailing slash, if required, to character vector
#' 
#' @param x
#'  character vector
#' @return
#'  x with a trailing slash, if required
#' 
#' @export
#' @examples
#' x <- "d:/workspace"
#' x <- "d:/workspace/"
#' add_trailing_slash("d:/workspace")
#' add_trailing_slash("d:/workspace/")
add_trailing_slash <- function(x) {
	if(grepl("/$", x)) {
		x
	} else {
		paste(x, "/", sep="")
	}
}


#' TRUE for elements that only contain alpha characters, ie: have no numeric component.
#' 
#' @param x
#'  character vector
#' 
#' @return 
#'  logical vector
#' 
#' @export
#' @examples 
#' 
#' x <- c("1", "1 (%)", "per cent")
#' is.alpha.only(x)
is.alpha.only <- function(x) {
	grepl("^\\D*$", x)
}


#' Remove any alpha from character vector, leaving only numbers.
#' 
#' @param x
#'  character vector
#' 
#' @return 
#' character vector without alpha
#' 
#' @export
#' @examples 
#' 
#' x <- c("1 (%)", "-2 (%)", "2.102 (%)", "10 (%)")
#' strip.alpha(x)
strip.alpha <- function(x) {
	# strip any characters that are not . 0-9 or -
	gsub("[^\\.0-9-]", "", x)
}

#' Remove any numbers from character vector, leaving only alpha.
#' 
#' @param x
#'  character vector
#'  
#' @return 
#' character vector without numbers
#' 
#' @export
#' @examples 
#' 
#' x <- c("1 (%)", "-2 (%)", "2.102 (%)", "10 (%)")
#' strip.numeric(x)
strip.numeric <- function(x) {
	# strip any characters that are . 0-9 or -
	gsub("[\\.0-9-]", "", x)
}

#' Remove any alpha from row and col names, leaving only numbers.
#' 
#' @param mx
#'  matrix 
#' 
#' @return 
#'  matrix after modify the row and col names
#' 
#' @export
#' @examples 
#' 
#' mx <- matrix(c(1:4), nrow=2, ncol=2, dimnames=list(c("0 (%)", "1 (%)"), c("0 (%)", "1 (%)")))
#' strip.alpha.mx(mx)
strip.alpha.mx <- function(mx) {
	
	rownames(mx) <- strip.alpha(rownames(mx))
	colnames(mx) <- strip.alpha(colnames(mx))
	mx
}


#' Remove leading and trailing spaces from a string
#' 
#' @param string
#'  character vector
#' 
#' @return 
#'  character vector after remove spaces
#' 
#' @export
#' @examples 
#' x <- c(" 1 (%) ", " -2 (%) ", "2.102 (%) ", " 10 (%)")
#' trim(x)
trim <- function (string) {
	#
	gsub("^\\s+|\\s+$", "", string)
}


