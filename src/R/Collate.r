# Functions related to collation of run stats.
# 
# Author: oman002
###############################################################################


#' Prepare frequency runs for display by:
#'  flattening into a 3D array
#'  calculate percentages
#'  remove zero category
#'  label columns
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
#' @param dict
#'  Dictionary object instance
#' @param asPercentages
#'  convert values to percentages of their grouping (if any)
#' @param removeZeroCategory 
#'  remove zero category
#' @return
#'  labelled and percentaged array, with z-dim = runs
#' 
#' @examples
#' \dontrun{
#' varname = "z1singleLvl1" ; varname = "gptotvis"
#' lol.mx <- env.base$modules[[1]]$runstats$freqs$all[[1]]
#' lol.mx <- env.base$modules$years1_5$runstats$freqs$all$z1singleLvl1
#' lol.mx <- env.base$modules$years1_5$runstats$freqs$all.by.ethnicity$z1singleLvl1
#' lol.mx <- env.base$modules$years1_5$runstats$cfreqs$gptotvis
#'
#' lol.mx <- env.base$modules$years1_5$runstats$freqs$all$sptype
#'  
#' lol.mx <- env.scenario$modules$years1_5$runstats$freqs$all$z1singleLvl1 
#' lol.mx <- env.scenario$modules$years1_5$runstats$freqs$all.by.ethnicity$z1singleLvl1
#' lol.mx <- env.scenario$modules$years1_5$runstats$cfreqs$houtptot
#' lol.mx <- env.scenario$modules$years6_13$runstats$cfreqs[["cond"]]
#' 
#' dict <- dict.MELC
#' dict <- dict_demo
#' 
#' asPercentages = T; removeZeroCategory = T; CI = F
#' removeZeroCategory = F
#' finialise.lolmx(lol.mx)
#' finialise.lolmx(lol.mx, dict, asPercentages, removeZeroCategory, CI)
#' }
finialise.lolmx <- function(lol.mx, dict, asPercentages = T, removeZeroCategory = T) {
	# flatten into 3D array
	lol.mx.array <- flatten.lolmx(lol.mx)
	
	# get percentages
	if (asPercentages) {
		#calculating numgrps. If no groupings, then across the whole row
		grpby.tag = attr(lol.mx.array, "meta")["grpby.tag"]
		numgrps <- if(is.null(grpby.tag) || is.na(grpby.tag)) 1 else length(dict$codings[[grpby.tag]]) 
		
		lol.mx.array <- prop.table.grpby.array.zdim(lol.mx.array, numgrps) * 100
	}
	
	# label cols, remove 0
	lol.mx.array.lbl <- labelFlattenedArrayCols(lol.mx.array, dict, removeZeroCategory = removeZeroCategory)
	
	# add (%) to column names
	if (asPercentages) {
		colnames(lol.mx.array.lbl) <- paste(colnames(lol.mx.array.lbl), "(%)")
	}
	
	# add year as a row label
	names(dimnames(lol.mx.array.lbl)) <- c("Year","")
	
	lol.mx.array.lbl
}


#' Label columns of a 3D array with the codings of the specified varname.
#' 
#' @param x
#'  vector/array with a column for each category, ordered
#' @param dict
#'  a Dictionary proto object
#' @param varname
#'  categorical variable name. The codings for this variable are applied
#'  as column names.
#' @return
#'  x with column names that use the codings of varname, and a column label 
#'  that is the decsription of varname.
#' 
#' @examples
#' \dontrun{
#' x <- structure(matrix(1:2, nrow=1, dimnames=list(1, c("0","1"))), meta=c("grpby.tag"="z1gender"))
#' x <- structure(matrix(1:6, nrow=1, dimnames=list(1, c("0 Mean","0 Lower","0 Upper","1 Mean","1 Lower","1 Upper"))), meta=c("grpby.tag"="z1gender"))
#' 
#' x <- env.base$modules$years1_5$runstats.collated$means$all.by.gender$kids
#' x <- env.scenario$modules$years1_5$runstats.collated$means$all.by.gender$kids
#' x <- env.base$modules$years1_5$runstats.collated$means$all$kids
#' x <- env.base$modules$years1_5$runstats.collated$means$all.by.gender$gptotvis
#' x <- runstat_f
#' varname=attr(x, "meta")["grpby.tag"]
#' dict <- dict.MELC
#' dict <- dict_demo
#' labelColumnCodes(x, dict, varname)
#' }
labelColumnCodes <- function(x, dict, varname) {
	
	if (is.null(varname) || is.na(varname)) {
		return(x)
	}
	
	# match codings into colnames stripped of alpha
	cnames <- dimnames(x)[[COL]]
	cnames_numeric <- strip.alpha(cnames)
	cnames_alpha <- strip.numeric(cnames)
	catcodings <- dict$codings[[varname]]
	
	codings_indices <- match(cnames_numeric, catcodings)
	cnames_numeric_desc <- names(catcodings)[codings_indices]
	
	# combine desc with existing alpha, NB: assume alpha is at the end
	dimnames(x)[[COL]] <- paste(cnames_numeric_desc, cnames_alpha, sep = "")	
	
	# add varname desc
	desc <- dict$descriptions[[varname]]
	names(dimnames(x))[[COL]] <- desc
	
	x
}

#' Label columns of a 3D array that has flattened codes for colnames.
#' 
#' @param xa
#'  3D array with flattened codes for colnames.
#'  A flattened code is in the form "0 1",
#'  where the first value is a grping code and the second a varname code.
#'  If grpby.tag is NULL or NA, then the flattened code will be in the form "0", 
#'  i.e: no grping codes only varname codes.
#' @param dict
#'  a Dictionary proto object
#' @param varname
#'  category variable name of this 3D array
#' @param grpby.tag
#'  grouping of flattened codes if any, else NULL or NA  
#' @param removeZeroCategory 
#'  remove zero category
#' @return
#'  3D array with relabled columns and dropped zero category (if requested)
#' 
#' @examples
#' \dontrun{
#' xa <- lol.mx.array
#' 
#' lol.mx <- env.base$modules$years1_5$runstats$freqs$all$z1singleLvl1  
#' lol.mx <- env.scenario$modules$years1_5$runstats$freqs$all$z1singleLvl1 
#' lol.mx <- env.base$modules$years1_5$runstats$freqs$all.by.ethnicity$z1singleLvl1
#' lol.mx <- env.base$modules$years1_5$runstats$cfreqs$gptotvis
#' xa <- flatten.lolmx(lol.mx) 
#' dict <- dict.MELC
#' removeZeroCategory = F
#' 
#' xa <- r$all.by.gender$householdsize
#' 
#' labelFlattenedArrayCols(xa, dict=dict, removeZeroCategory=removeZeroCategory)
#' }
labelFlattenedArrayCols <- function(xa, dict, varname=attr(xa, "meta")["varname"], 
		grpby.tag=attr(xa, "meta")["grpby.tag"], removeZeroCategory = T) {
	
	# identify 0 category columns
	cnames <- colnames(xa)
	if (is.null(grpby.tag) || is.na(grpby.tag)) {
		zerocols <- which(cnames == "0")
	} else {
		zerocols <- grep("\\s0", cnames)
	}
	
	colnames(xa) <- dict$cmatchFlattened(cnames, varname, grpby.tag)
	
	# remove 0 category, if requested
	if (removeZeroCategory && length(zerocols)) {
		structure(xa[,-zerocols,,drop=FALSE], meta=c(attr(xa, "meta")))
	} else {
		xa
	}
}

#' Calculate the proportions within groups that 
#' exist in the rows of the matrices in the ZDIM of xa
#' 
#' @param xa
#'  and 3D array, with matrices in the ZDIM.
#' @param numgrps
#'  the num of groups. Each row in the ZDIM is divided into this number of groups.
#'  Proportions are then calculated within these groups. If numgrps = 1, then 
#'  there is only 1 group and proportions are calculated across the whole row.
#' @return
#'  xa as proportions
#' @examples
#' \dontrun{
#' lol.mx <- env.base$modules$years1_5$runstats$freqs$all$z1singleLvl1 
#' lol.mx <- env.base$modules$years1_5$runstats$freqs$all.by.ethnicity$z1singleLvl1
#' lol.mx <- env.base$modules$years1_5$runstats$cfreqs$gptotvis
#' 
#' lol.mx <- env.scenario$modules$years1_5$runstats$freqs$all$sptype
#' lol.mx <- env.scenario$modules$years1_5$runstats$cfreqs$houtptot
#' 
#' xa <- flatten.lolmx(lol.mx)
#' 
#' xa <- array(c(1:5, rep(0,5)), dim=c(5,1,2), dimnames=list(LETTERS[1:5],c("Col")))
#' xa <- array(c(1:5, 9,8,7,6,5), dim=c(5,2,1), dimnames=list(LETTERS[1:5], c("Col 1", "Col 2")))
#' 
#' xa <- lol.mx.array
#' numgrps <- 1
#' prop.table.grpby.array.zdim(xa, numgrps)
#' }
prop.table.grpby.array.zdim <- function (xa, numgrps) {
	
	grpsize <- ncol(xa) / numgrps
	grpby <- rep(1:numgrps, each=grpsize)
	
	# get proportions by grp
	xa.prop <- apply(xa, c(ROW,ZDIM), prop.table.grpby, grpby=grpby)
	
	if (ncol(xa)==1) {
		# apply simplifies if we only have one column, so reapply dimensions
		xa.prop.t <- structure(xa.prop, .Dim=dim(xa))
	} else {
		# transpose ZDIM
		xa.prop.t <- aperm(xa.prop, perm=c(2,1,3))
	}
	
	structure(xa.prop.t, meta = attr(xa, "meta"), dimnames=dimnames(xa))
	
}

confreqs_collator <- function (runs, dict) { 
	runs_f <- finialise.lolmx(runs, dict = dict, removeZeroCategory = FALSE)
	mean.array.z(runs_f, CI=F)  
}

freqs_collator <- function(runs, dict) { 
	runs_f <- finialise.lolmx(runs, dict = dict)
	mean.array.z(runs_f, CI=F) 
}

histogram_collator <- function(runs, dict) { 
	runs_f <- finialise.lolmx(runs, dict = dict, asPercentages = F, removeZeroCategory = FALSE) 
	mean.array.z(runs_f) 
}

means_collator <- function(runs, dict) {
	#runs <- all_run_results_zipped$means_by_sex[[1]]
	runs_array <- as.arrayListmx(runs)
	runs_array_mean <- mean.array.z(runs_array) 
	runs_array_mean_labelled <- labelColumnCodes(runs_array_mean, dict, attr(runs_array_mean, "meta")["grpby.tag"])
	if (is.null(colnames(runs_array_mean_labelled))) colnames(runs_array_mean_labelled) <- "Mean"
	runs_array_mean_labelled
}

basic_collator <- function(runs) {
	runs_array <- as.arrayListmx(runs)
	mean.array.z(runs_array)
}
