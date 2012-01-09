# String util functions
# 
# Author: oman002
###############################################################################

#' Remove any alpha from character vector, leaving only numbers.
#' 
#' @param x
#'  character vector
#' @examples 
#' 
#' x <- c("1 (%)", "-2 (%)", "2.102 (%)", "10 (%)")
#' strip.alpha(x)
strip.alpha <- function(x) {
	# strip any characters that are not . 0-9 or -
	gsub("[^\\.0-9-]", "", x)
}

#' Remove any alpha from row and col names, leaving only numbers.
#' 
#' @param mx
#'  matrix 
#' @examples 
#' 
#' mx <- matrix(c(1:4), nrow=2, ncol=2, dimnames=list(c("0 (%)", "1 (%)"), c("0 (%)", "1 (%)")))
#' strip.alpha.mx(mx)
strip.alpha.mx <- function(mx) {
	
	rownames(mx) <- strip.alpha(rownames(mx))
	colnames(mx) <- strip.alpha(colnames(mx))
	mx
}


trim <- function (string) {
	#remove leading and trailing spaces from a string
	gsub("^\\s+|\\s+$", "", string)
}




