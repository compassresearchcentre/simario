# Simulation support functions.
#
# Requires support.r
# 
# Author: Oliver Mannion
###############################################################################

#' Loads and merges a CSV/XLS file with the supplied values (keys). ie:
#' returns a dataframe (excluding key_column_name) for the supplied 
#' values that exist in key_column_name of the file 
#' 
#' @param filedir
#'  file directory, with or without trailing slash
#' 
#' @param filename
#'  file name. File type is determined from the file extension, eg: ".csv", ".xls", ".xlsx" 
#' 
#' @param  key_column_name 
#'  a column in the propensity files to merge on, and select
#'  those values that appear in selected_keys
#' 
#' @param selected_keys
#'  a vector of selected keys that are to be retained in the propensities
#' 
#' @return 
#' a dataframe
#'
#' @export 
#' @examples
#' \dontrun{
#' selected_keys <- children$A0
#' key_column_name <- "A0"
#' filedir <- "D:/workspace.sim/MELC/CHDS/propensityFiles/"
#' loadMergedFile(key_column_name, selected_keys, propensityfiledir)
#' }
loadMergedFile <- function(filedir, filename, key_column_name, selected_keys) {
	dataframe <- read_file(filedir, filename)
	mergeAndRemoveKeyColumn(dataframe, key_column_name, selected_keys)
}

#' Takes a result row and returns the means and error amounts as separate vectors in a matrix or list.
#' 
#' @param result.row
#'  a result row, ie: a vector with values named Mean and Lower eg:
#' 
#'>  envs$`Scenario 1`$years1_5$run_results_collated$means$kids["Total",]
#'     Mean    Lower    Upper 
#' 10.99488 10.62256 11.36721 
#' 
#'  if there are no values named Mean, then it will be assumed that all values
#'  are Means and that Lower is 0.
#' 
#' @param simplify
#'  if TRUE (default), returns a matrix instead of a list. 
#' 
#' @return
#'  a matrix/list of means and errs. The first row/means vector is the means from the result row, and the
#'  second row/errs vector is the difference between each mean and it's lower value.
#' 
#' @examples
#' \dontrun{
#' 
#' result.row <- envs$`Scenario 1`$years1_5$run_results_collated$means$kids["Total",]
#' \dontrun{
#' > result.row
#'     Mean    Lower    Upper 
#' 10.99488 10.62256 11.36721
#'  
#' > result.as.means.and.errs(result.row)
#' $means
#' 
#' 10.99488 
#' 
#' $errs
#' 
#' 0.3723213 
#'
#' }
#' result.row <- c("0%"=5,"20%"=5,"40%"=9,"60%"=11,"80%"=15,"100%"=50)
#' result.row <- structure(c(5, 5, 5, 5, 5, 5, 9, 9, 9, 11, 11, 11, 15, 15, 15,50.5, 6.02828342338857, 94.9717165766114), .Names = c("0% Mean","0% Lower", "0% Upper", "20% Mean", "20% Lower", "20% Upper","40% Mean", "40% Lower", "40% Upper", "60% Mean", "60% Lower","60% Upper", "80% Mean", "80% Lower", "80% Upper", "100% Mean","100% Lower", "100% Upper"))
#' 
#' result.row <- env.base$modules$years1_5$run_results_collated$quantiles$kids["All Years",]
#' result.row <- env.scenario$modules$years1_5$run_results_collated$quantiles$kids["All Years",]
#' result.row <- env.scenario$modules$years1_5$run_results_collated$means$kids["All Years",]
#' result.row <- na.omit(env.scenario$modules$years6_13$run_results_collated$histogram[["cond"]]["All Years",])
#' 
#' result.as.means.and.errs(result.row.scenario)
#' 
#' result.as.means.and.errs(result.row)
#' }
result.as.means.and.errs <- function(result.row, simplify = T) {
	ind.means <- grep("Mean", names(result.row))
	ind.lowers <- grep("Lower", names(result.row))
	
	assert(length(ind.means) == length(ind.lowers))
	
	has_CIs <- length(ind.lowers) > 0
	if(!has_CIs) {
		result.row.means <- result.row
		result.row.err <- structure(rep(0, length(result.row.means)), .Names = names(result.row.means))
	} else {
		result.row.means <- result.row[ind.means]
		names(result.row.means) <- trim(gsub("Mean", "", names(result.row.means)))
		
		result.row.err <- result.row.means - result.row[ind.lowers]
	}
	
	if (simplify) {
		rbind(means=result.row.means, errs=result.row.err)
	} else {
		list(means=result.row.means, errs=result.row.err)
	}
}	

#' Produce a proportioned table for x, using
#' the specified coding as names and 
#' setting the "meta" attribute to "varname"
#' of coding.
#' 
#' @param x
#'  vector of values
#' 
#' @param coding
#'  a coding variable. names(coding) is the labels
#'  attr(coding, "varname") is a named element in xlist
#' 
#' @return 
#'  a table (proportions) with names specified by coding
#' 
#' @export 
#' @examples
#' \dontrun{
#' table.catvar(children$SESBTH, codings$SESBTH)
#' x <- simframe$z1singleLvl1 ; coding <- codings$z1singleLvl1
#' table.catvar(simframe$z1singleLvl1, codings$z1singleLvl1)
#' x <- binary.levels.combine(simframe.master$SESBTHLvl1, simframe.master$SESBTHLvl2, simframe.master$SESBTHLvl3)
#' coding <- dict.MELC$codings$SESBTH}
table.catvar <- function (x, coding) {
	
	varname <- attr(coding, "varname")
	
	tbl <- prop.table(table(x)) * 100
	
	# match names into codings
	codings.indices <- match(names(tbl), coding)
	names(tbl) <- paste(names(coding)[codings.indices], "(%)")
	
	attr(tbl, "meta") <- c("varname" = varname)
	
	tbl
	
}


#' This is the version for table.catvar() with confidence interval.
#' Produce a proportioned table for x with confidence interval, using
#' the specified coding as names and setting the "meta" attribute to "varname"
#' of coding.
#' 
#' @param x
#'  vector of values
#' 
#' @param coding
#'  a coding variable. names(coding) is the labels
#'  attr(coding, "varname") is a named element in xlist
#' 
#' @return 
#'  a table (proportions) with names specified by coding
#' 
#' @export 
#' @examples
#' \dontrun{
#' table.catvar.with.CI(children$SESBTH, codings$SESBTH)
#' x <- simframe$z1singleLvl1 ; coding <- codings$z1singleLvl1
#' table.catvar.with.CI(simframe$z1singleLvl1, codings$z1singleLvl1)
#' x <- binary.levels.combine(simframe.master$SESBTHLvl1, simframe.master$SESBTHLvl2, simframe.master$SESBTHLvl3)
#' coding <- dict.MELC$codings$SESBTH}

table.catvar.with.CI <- function (x, coding) {
	
	varname <- attr(coding, "varname")
	
	tbl <- prop.table(table(x)) * 100
	#calculate CIs for each group proportion
	n <- length(x)
	p <- tbl/100
	se <- sqrt(p*(1-p)/n)
	z <- qnorm(.975)
	upper.limit <- p + z*se
	lower.limit <- p - z*se
	tbl2 <- c(tbl, lower.limit*100, upper.limit*100)
	tbl.names <- names(tbl2) 
	tbl2 <- tbl2[order(tbl.names)]
	
	if (table(tbl.names)[1]>1) {
		CI=TRUE
		tbl3 <- matrix(tbl2, ncol=3, nrow=length(tbl2)/3, byrow=TRUE)
		suffixes <- c("Mean (%)", "Lower", "Upper")
		codings.indices <- match((tbl.names[order(tbl.names)]), coding)
		colnames(tbl3) <- suffixes
		id <- 3*(1:(length(tbl2)/3)) - 2
		codings.indices <- match((tbl.names[order(tbl.names)]), coding)
		rownames(tbl3) <- names(coding)[codings.indices][id]
		attr(tbl3, "meta") <- c("varname" = varname)
		return(tbl3)
	} else {
		suffixes <- c("", "", "")
		# match names into codings
		codings.indices <- match((tbl.names[order(tbl.names)]), coding)
		names(tbl2) <- paste(names(coding)[codings.indices], "(%)", suffixes)
		attr(tbl2, "meta") <- c("varname" = varname)
		return(tbl2)
	}

}

#' Display a vector of continuous values in a table using the
#' breaks supplied.
#' Attachs a meta attribute with varname
#' 
#' @param x
#'  vector of continous values
#' 
#' @param breaks
#' a numeric vector of two or more cut points
#' NB: note that the cut point value is not included in the bin 
#' (ie: include.lowest = FALSE)
#' Therefore the very first cut point must be less than min(x)
#' 
#' @param varname
#'  added as a tag on the meta attribute
#' 
#' @return 
#'  a table (proportions) with names specified by breaks
#' 
#' @examples
#' \dontrun{
#' x <- env.scenario$simframe$bwkg
#' breaks <- binbreaks$bwkg
#' 
#' table.contvar(env.scenario$simframe$bwkg, binbreaks$bwkg, "bwkg")
#' }
table.contvar <- function (x, breaks, varname) {
	tbl <- prop.table(table(bin(x, breaks, breaklast=NULL), useNA='ifany')) * 100
	attr(tbl, "meta") <- c("varname" = varname)
	tbl
}


#' This is the version for table.contvar() with confidence interval.
#' Display a vector of continuous values in a table with confidence interval 
#' using the breaks supplied.
#' Attachs a meta attribute with varname
#' 
#' @param x
#'  vector of continous values
#' 
#' @param breaks
#' a numeric vector of two or more cut points
#' NB: note that the cut point value is not included in the bin 
#' (ie: include.lowest = FALSE)
#' Therefore the very first cut point must be less than min(x)
#' 
#' @param varname
#'  added as a tag on the meta attribute
#' 
#' @return 
#'  a table (proportions) with names specified by breaks
#' 
#' @examples
#' \dontrun{
#' x <- env.scenario$simframe$bwkg
#' breaks <- binbreaks$bwkg
#' 
#' table.contvar.with.CI(env.scenario$simframe$bwkg, binbreaks$bwkg, "bwkg")
#' }
table.contvar.with.CI <- function (x, breaks, varname) {
	tbl <- prop.table(table(bin(x, breaks, breaklast=NULL), useNA='ifany')) * 100
	#calculate CIs for each group proportion
	n <- length(x)
	p <- tbl/100
	se <- sqrt(p*(1-p)/n)
	z <- qnorm(.975)
	upper.limit <- p + z*se
	lower.limit <- p - z*se
	tbl2 <- c(tbl, lower.limit*100, upper.limit*100)
	tbl.names <- names(tbl2) 
	
	un.ordered.names <- tbl.names
	ord <- match(un.ordered.names, names(breaks[-1]))
	ordered.names <- un.ordered.names[order(ord)]
	
	tbl2 <- tbl2[order(ord)]
	
	tbl2[tbl2<0] <- 0
	if (table(tbl.names)[1]>1) {
		CI=TRUE
		tbl3 <- matrix(tbl2, ncol=3, nrow=length(tbl2)/3, byrow=TRUE)
		suffixes <- c("Mean (%)", "Lower", "Upper")
		colnames(tbl3) <- suffixes
		id <- 3*(1:(length(tbl2)/3)) - 2
		rownames(tbl3) <- tbl.names[order(ord)][id]
		attr(tbl3, "meta") <- c("varname" = varname)
		return(tbl3)
		
	} else {
		suffixes <- c("", "", "")
		names(tbl2) <- paste(tbl.names[order(ord)], "(%)", suffixes)
		attr(tbl2, "meta") <- c("varname" = varname)
		tbl3 <- t(tbl2)
		return(tbl3)
		
	}
}



cat("Loaded simulate\n")
