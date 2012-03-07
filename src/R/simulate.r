# Simulation support functions.
#
# Requires support.r
# 
# Author: Oliver Mannion
###############################################################################

#' Creates a set of run outputs. 
#' 
#' @param freqvars
#' 			frequency variable names, or NULL
#' @param cfreqvars
#' 			continuous frequency variable names, or NULL
#' @param meanvars
#' 			mean variable names, or NULL
#' @param freqs.args
#' 			frequency variable args, or NULL
#' @param means.args
#' 			mean variable args, or NULL
#' @return
#'  	list(freq = freq, cfreq = cfreq, mean.sets = mean.sets, mean.grouped = mean.grouped)
#' 		each element of the list is a list of empty catvar or convar elements, either grouped
#' 		into sets (mean.grouped and mean.sets) or listed straight.
#' 
#' @examples 
#' \dontrun{
#' catvars <- NULL
#' convars <- NULL
#' means.args <- NULL
#' mean.grouped.spec <- NULL
#'  
#' catvars <- c("msmoke", "fsmoke", "single", "kids", "householdsize", "welfare", "mhrswrk", "fhrswrk", "accom", "homeown", "bedrooms",	"chpar", "chres")
#' convars <- c("gptotvis", "hadmtot", "houtptot", "gpresp", "gpmorb", "gpprev")
#' 
#' freqs.args <- list( by.ethnicity = list(grpbycoding=codings$r1stchildethn) )
#' means.args <- list(	all = list(), males = list(logiset=childsets$males),	females = list(logiset=childsets$females),pacific = list(logiset=childsets$pacific),	maori = list(logiset=childsets$maori))
#' 
#' freqvars <- catvars
#' runstats <- createRunOutputs(catvars, convars, means.args, mean.grouped.spec)
#' }
createRunOutputs <- function(freqvars, cfreqvars, meanvars, freqs.args, means.args) {
	# Frequency tables for categorical variables
	freqslist <- namedList(freqvars)
	
	# Frequency tables for continuous variables
	cfreqs <- namedList(cfreqvars)
	
	# Mean tables for continuous variables
	meanslist <- namedList(meanvars)
	
	freqs <- lapply(freqs.args, function(x) freqslist)
	attr(freqs, "args.list") <- freqs.args
	
	means <- lapply(means.args, function(x) meanslist)
	attr(means, "args.list") <- means.args
	
	list(freqs = freqs, 
			cfreqs = cfreqs, 
			means = means, 
			summaries = meanslist,
			quantiles = meanslist
	)
	
}

#' Prepare run runstats.collated for display by:
#'  flattening into a 3D array
#'  calculate percentages
#'  remove zero category
#'  label columns
#'  and calculate mean across all matrices
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
#' @param CI
#'  if TRUE, will produce confidence intervals on the means
#' @return
#'  a matrix of means
#' 
#' @examples
#' \dontrun{
#' varname = "z1singleLvl1" ; varname = "gptotvis"
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
#' 
#' asPercentages = T; removeZeroCategory = T; CI = F
#' removeZeroCategory = F
#' finialise.lolmx(lol.mx)
#' finialise.lolmx(lol.mx, dict, asPercentages, removeZeroCategory, CI)
#' }
finialise.lolmx <- function(lol.mx, dict, asPercentages = T, removeZeroCategory = T, CI = F) {
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
	
	# combine into a single matrix by taking mean across z dimension
	result <- mean.array.z(lol.mx.array.lbl, CI = CI)
	
	# add year as a row label
	names(dimnames(result)) <- c("Year","")
	
	result
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
#' varname=attr(x, "meta")["grpby.tag"]
#' dict <- dict.MELC
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

#' Loads and merges a CSV/XLS file with the supplied values (keys). ie:
#' returns a dataframe (excluding key_column_name) for the supplied 
#' values that exist in key_column_name of the file 
#' 
#' @param filedir
#'  file directory, with or without trailing slash
#' @param filename
#'  file name. File type is determined from the file extension, eg: ".csv", ".xls", ".xlsx" 
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


#' Takes a result row and returns the means and error amounts as separate vectors in a matrix or list.
#' 
#' @param result.row
#'  a result row, ie: a vector with values named Mean and Lower eg:
#' 
#'>  envs$`Scenario 1`$years1_5$runstats.collated$means$all$kids["Total",]
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
#' result.row <- envs$`Scenario 1`$years1_5$runstats.collated$means$all$kids["Total",]
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
#' result.row <- env.base$modules$years1_5$runstats.collated$quantiles$kids["All Years",]
#' result.row <- env.scenario$modules$years1_5$runstats.collated$quantiles$kids["All Years",]
#' result.row <- env.scenario$modules$years1_5$runstats.collated$means$all$kids["All Years",]
#' result.row <- na.omit(env.scenario$modules$years6_13$runstats.collated$histo[["cond"]]["All Years",])
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
#' @param coding
#'  a coding variable. names(coding) is the labels
#'  attr(coding, "varname") is a named element in xlist
#' @return 
#'  a table (proportions) with names specified by coding 
#' @examples
#' \dontrun{
#' table.catvar(children$SESBTH, codings$SESBTH)
#' x <- simframe$z1singleLvl1 ; coding <- codings$z1singleLvl1
#' table.catvar(simframe$z1singleLvl1, codings$z1singleLvl1)
#' }
table.catvar <- function (x, coding) {
	
	varname <- attr(coding, "varname")
	
	tbl <- prop.table(table(x)) * 100
	
	# match names into codings
	codings.indices <- match(names(tbl), coding)
	names(tbl) <- paste(names(coding)[codings.indices], "(%)")
	
	attr(tbl, "meta") <- c("varname" = varname)
	
	tbl
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

cat("Loaded simulate\n")
