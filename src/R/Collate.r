# Functions related to collation of run results.
# 
# Author: oman002
###############################################################################

#' Collate frequencies. Performs the following:
#' 
#' \itemize{
#'   \item Takes mean without confidence intervals using \code{\link{collator_mutiple_lists_mx}}  
#'   \item Labels the result using the dictionary 
#'   \item Converts frequencies to percentages
#' }
#'   
#' @param runs
#'  a list of lists of matrices, one inner list per run.
#'  Each inner list may have any number of matrices,
#'  and each matrix may have a different sets of rows or columns.
#'  The matrices will be flattened into rows.
#' 
#' @param dict
#'  Dictionary object. Used to label columns.
#'  
#' @param row.dim.label
#'  name of the entire row dimension
#' 
#' @param col.dim.label
#'  name of the entire col dimension
#'
#' @seealso \code{\link{collator_mutiple_lists_mx}}
#' @export 
#' @examples
#' run1_mx1 = matrix(1:2, nrow=1, dimnames=list(1, c("F","M")))
#' run1_mx2 = matrix(1:4, nrow=2, dimnames=list(1:2, c("F","M")), byrow = TRUE)
#' run1 = structure(list(run1_mx1, run1_mx2), meta=c(varname="disability_state", grpby.tag="sex"))
#' run2_mx1 = matrix(11:12, nrow=1, dimnames=list(1, c("F","M")))
#' run2_mx2 = matrix(11:14, nrow=2, dimnames=list(3:4, c("F","M")), byrow = TRUE)
#' run2 = structure(list(run2_mx1, run2_mx2), meta=c(varname="disability_state", grpby.tag="sex")) 
#' 
#' runs <- list(run1=run1,run2=run2) 
#' dict <- dict_example
#' collator_freqs(runs, dict)
#' collator_freqs(runs, dict, numbers=TRUE)
collator_freqs <- function (runs, dict, row.dim.label="Year", col.dim.label="", numbers=FALSE, CI=FALSE) {
	runs_mx <- collator_mutiple_lists_mx(runs, CI)
	
	num.runs <- length(runs)
	
	if ((CI==FALSE|(num.runs==1))) {
		runs_mx <- label_flattened_mx(runs_mx, dict, row.dim.label, col.dim.label)
		if (numbers==FALSE) {
			result <- percentages_flattened_mx(runs_mx, dict, CI, num.runs=num.runs)
		} else {
			result <- runs_mx
		}
	} else if ((CI==TRUE)&&(num.runs>1)) {
		runs_mx <- label_flattened_mx_grping.and.CIs(runs_mx, dict, row.dim.label, col.dim.label)
		if (numbers==FALSE) {
			resultCI <- percentages_flattened_mx(runs_mx, dict, CI, num.runs=num.runs)
		} else {
			resultCI <- runs_mx
		}
		#label CI components
		run1_array <- as_array_list_mx(runs[[1]])
		numGroups <- dim(run1_array)[COL]
		colnames(resultCI) <- paste(colnames(resultCI), rep(c("Mean", "Lower", "Upper"), numGroups))
		result <- resultCI
	}
	return(result)
}


#' Collate frequencies and removes the zero category. Performs the following:
#' 
#' \itemize{
#'   \item Takes mean without confidence intervals using \code{\link{collator_mutiple_lists_mx}} 
#'   \item Labels the result using the dictionary 
#'   \item Converts frequencies to percentages
#'   \item Removes the zero category
#' }
#'
#' @param runs
#'  a list of lists of matrices, one inner list per run.
#'  Each inner list may have any number of matrices,
#'  and each matrix may have a different sets of rows or columns.
#'  The matrices will be flattened into rows.
#' 
#' @param dict
#'  Dictionary object. Used to label columns.
#'  
#' @param row.dim.label
#'  name of the entire row dimension
#' 
#' @param col.dim.label
#'  name of the entire col dimension
#'
#' @seealso \code{\link{collator_mutiple_lists_mx}}
#' @export 
#' @examples
#' \dontrun{
#' runs <- all_run_results_zipped$freqs[[1]]
#' runs <- all_run_results_zipped$freqs_by_sex[[1]]
#' collator_freqs_remove_zero_cat(runs, dict_example)
#' }
	
collator_freqs_remove_zero_cat <- function(runs, dict, row.dim.label="Year", col.dim.label="", CI=FALSE) {
	runs_mx <- collator_mutiple_lists_mx(runs, CI)
	grpby.tag <- attr(runs_mx, "meta")["grpby.tag"]
	
	zero_cat_cols <- identify_zero_category_cols(runs_mx)
	
	#the above code give incorrect categories with 0s for the outcome 
	#we only want to remove thosew columns that are 0 for the outcome, not also for the grouping variable
	#(which is what the above code does)
	if (!is.null(grpby.tag)) {
		if (!is.na(grpby.tag)) {
			if (grpby.tag!="") {
				zero_cat_cols <- identify_zero_category_cols_bygrp(runs_mx)	
			}
		}
	}

	numZ <- length(runs) #number of runs
	
	if ((CI==FALSE)|(numZ==1)) {
		runs_mx <- label_flattened_mx(runs_mx, dict, row.dim.label, col.dim.label)
		runs_mx <- percentages_flattened_mx(runs_mx, dict, CI, numZ)
		result <- remove.cols(runs_mx, zero_cat_cols)
	} else if ((CI==TRUE)&&(numZ>1)) {
		runs_mx <- label_flattened_mx_grping.and.CIs(runs_mx, dict, row.dim.label, col.dim.label)
		runs_mx <- percentages_flattened_mx(runs_mx, dict, CI, numZ)
		
		resultCI <- remove.cols(runs_mx, zero_cat_cols)
		#label CI components
		run1_array <- as_array_list_mx(runs[[1]])
		numGroups <- dim(run1_array)[COL]
		colnames(resultCI) <- paste(colnames(resultCI), rep(c("Mean", "Lower", "Upper"), numGroups))
		names(dimnames(resultCI)) <- names(dimnames(resultCI))
		result <- resultCI
	}
	
	return(result)
}

#' Collates frequencies for use in histogram output with confidence intervals. 
#' Performs the following:
#' 
#' \itemize{
#'   \item Takes mean with confidence intervals using \code{\link{collator_mutiple_lists_mx}}
#'   \item Labels the result using the dictionary 
#' }
#' 
#' @param runs
#'  a list of lists of matrices, one inner list per run.
#'  Each inner list may have any number of matrices,
#'  and each matrix may have a different sets of rows or columns.
#'  The matrices will be flattened into rows.
#' 
#' @param dict
#'  Dictionary object. Used to label columns.
#'  
#' @param row.dim.label
#'  name of the entire row dimension
#' 
#' @param col.dim.label
#'  name of the entire col dimension
#' 
#' @seealso \code{\link{collator_mutiple_lists_mx}}
#' @export 
collator_histogram <- function(runs, dict, row.dim.label="Year", col.dim.label="") {
	runs_mx <- collator_mutiple_lists_mx(runs)
	
	label_flattened_mx(runs_mx, dict, row.dim.label, col.dim.label) 
}

#' Collate means over multiple runs. Performs the following:
#' 
#' \itemize{
#'   \item Takes mean with confidence intervals using \code{\link{collator_list_mx}}
#'   \item Labels the result using the dictionary 
#' }
#' 
#' @param runs
#'  a list of matrices, one matrix per run.
#' @param dict
#'  Dictionary object. Used to label columns.
#'
#' @seealso \code{\link{collator_list_mx}}
#' @export 
#' @examples
#' run1 = structure(matrix(1:6, nrow=3, dimnames=list(1:3, c("F","M"))), meta=c(varname="earnings", grpby.tag="sex"))
#' run2 = structure(matrix(11:16, nrow=3, dimnames=list(1:3, c("F","M"))), meta=c(varname="earnings", grpby.tag="sex"))
#' 
#' runs <- list(run1=run1,run2=run2) 
#' dict <- dict_example
#' collator_means(runs, dict)
collator_means <- function(runs, dict, ...) {
	runs_mx <- collator_list_mx(runs, ...)
	
	runs_mx_labelled <- labelColumnCodes(runs_mx, dict, attr(runs_mx, "meta")["grpby.tag"])
	if (is.null(colnames(runs_mx_labelled))) colnames(runs_mx_labelled) <- "Mean"
	runs_mx_labelled
}

#' Collate and average a list of matrices.
#' 
#' @param runs
#'  a list of matrices, one matrix per run. Each matrix must have
#'  the same dimensions.
#' 
#' @param CI
#'  if TRUE and length(runs) > 1, lower and upper confidence intervals 
#'  are returned in additional columns
#' 
#' @return
#'  a matrix with the averaged values of runs.
#'
#' @export 
#' @examples
#' run1 = structure(matrix(1:6, nrow=3, dimnames=list(1:3, c("F","M"))), meta=c(varname="earnings", grpby.tag="sex"))
#' run2 = structure(matrix(11:16, nrow=3, dimnames=list(1:3, c("F","M"))), meta=c(varname="earnings", grpby.tag="sex"))
#' runs <- list(run1=run1,run2=run2) 
#' collator_list_mx(runs)
collator_list_mx <- function(runs, CI=TRUE, ...) {
	runs_array <- as_array_list_mx(runs)
	mean_array_z_pctile_CIs(runs_array, CI=CI, ...)
}

#' Collate and average mutiple lists of matrices.
#' 
#' @param runs
#'  a list of lists of matrices, one inner list per run.
#'  Each inner list may have any number of matrices,
#'  and each matrix may have a different sets of rows or columns.
#'  The matrices will be flattened into rows.
#' 
#' @param CI
#'  if TRUE and length(runs) > 1, lower and upper confidence intervals 
#'  are returned in additional columns
#'   
#' @return
#'  a matrix with the averaged values of runs.
#'
#' @export 
#' @keywords internal
#' @examples
#' run1_mx1 = matrix(1:2, nrow=1, dimnames=list(1, c("F","M")))
#' run1_mx2 = matrix(1:4, nrow=2, dimnames=list(1:2, c("F","M")), byrow = TRUE)
#' run1 = structure(list(run1_mx1, run1_mx2), meta=c(varname="disability_state", grpby.tag="sex"))
#' run2_mx1 = matrix(11:12, nrow=1, dimnames=list(1, c("F","M")))
#' run2_mx2 = matrix(11:14, nrow=2, dimnames=list(3:4, c("F","M")), byrow = TRUE)
#' run2 = structure(list(run2_mx1, run2_mx2), meta=c(varname="disability_state", grpby.tag="sex")) 
#' 
#' runs <- list(run1=run1,run2=run2) 
#' collator_mutiple_lists_mx(runs, CI=FALSE)
collator_mutiple_lists_mx <- function(runs, CI=TRUE) {
	runs_array <- flatten_mxlists_to_array(runs)
	mean_array_z_pctile_CIs(runs_array, CI=CI)
}

#' Identify and return the indices of columns that 
#' are for the zero category. Zero category column
#' names begin with a "0" or, for flatten column 
#' names, contain " 0". 
#' 
#' @param mx
#'  matrix with column names
#' @return
#'  vector of zero column positions
#'
#' @export 
#' @examples
#'  mx <- matrix(1:3, nrow=1, dimnames=list(NULL, c("1","2","3")))
#'  mx <- matrix(1:3, nrow=1, dimnames=list(NULL, c("0","1","2")))
#'  mx <- matrix(1:4, nrow=1, dimnames=list(NULL, c("1 0","1 1","2 0", "2 1")))
#'  identify_zero_category_cols(mx)
identify_zero_category_cols <- function (mx) {
	grep("\\s0|^0", colnames(mx))
}

identify_zero_category_cols_bygrp <- function (mx) {
	#names of the outcome variable (as opposed to the grouping variable come 2nd
	col.names <- colnames(mx)
	#identify the position of the last space in each name
	space.ids <- str_locate_all(col.names, " ")
	relevant.name.pos <- lapply(space.ids, function(x) {x[1,1] + 1}) #position of relevant name
	rel.name.pos.vec <- rep(NA, length(relevant.name.pos))
	for (i in 1:length(rel.name.pos.vec)) {
		rel.name.pos.vec[i]<-relevant.name.pos[[i]]
	}
	rel.names <- str_sub(col.names, rel.name.pos.vec, rel.name.pos.vec)
	grep("\\s0|^0", rel.names)
}


#' Calculated percentages within groups of a flattened matrix.
#'
#' @param mx.flattened
#'  a flattened matrix, ie: a matrix that has rows that contain
#'  groups of values. Percentages are then calculated within these groups.
#'  The number of groups is determined by using the meta "grpby.tag"
#'  attribute to lookup the group by codings in \code{dict}.
#'  
#' @param dict
#'  Dictionary object. Used to determine number of groups.
#' 
#' @seealso \code{\link{prop.table.mx.grped.rows}}
#' @export  
#' @examples
#' \dontrun{
#' mx.flattened <- structure(matrix(c(1,2,1,3,1,4,2,2,2,3,2,4,1,2,3,4), nrow=2, byrow = TRUE, dimnames=list(NULL, c("Female 1", "Female 2", "Female 3", "Female 4", "Male 1", "Male 2", "Male 3", "Male 4"))), meta=c(varname="disability_state", grpby.tag="sexLvl1"))
#' dict <- dict_demo
#' percentages_flattened_mx(mx.flattened, dict)
#'
#' mx.flattened <- structure(matrix(c(1,2,1,3,1,4,2,2,2,3,2,4,1,2,3,4), nrow=2, byrow = TRUE, dimnames=list(NULL, c("65-69 1", "65-69 2", "65-69  3", "65-69  4", "70-74  1", "70-74 2", "70-74 3", "70-74 4"))), meta=c(varname="disability_state", grpby.tag="age_grp_output"))
#' percentages_flattened_mx(mx.flattened, dict)
#' }
#' mx.flattened <- structure(matrix(c(1,2,1,3,1,4,2,2,2,3,2,4,1,2,3,4), nrow=2, byrow = TRUE, dimnames=list(NULL, c("Female 1", "Female 2", "Female 3", "Female 4", "Male 1", "Male 2", "Male 3", "Male 4"))), meta=c(varname="disability_state", grpby.tag="sex"))
#' dict <- dict_example
#' percentages_flattened_mx(mx.flattened, dict)

percentages_flattened_mx <- function(mx.flattened, dict, CI=FALSE, num.runs) {
	grpby.tag <- attr(mx.flattened, "meta")["grpby.tag"]
	
	groupnameprefixes <- if(is.null(grpby.tag) || is.na(grpby.tag)) NULL else names(dict$codings[[grpby.tag]])

	result <- prop.table.mx.grped.rows(mx.flattened, groupnameprefixes, CI, num.runs) * 100
	colnames(result) <- paste(colnames(result), "(%)")
	return(result)
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
#' x <- env.base$modules$years1_5$run_results_collated$means_by_gender$kids
#' x <- env.scenario$modules$years1_5$run_results_collated$means_by_gender$kids
#' x <- env.base$modules$years1_5$run_results_collated$means$kids
#' x <- env.base$modules$years1_5$run_results_collated$means_by_gender$gptotvis
#' x <- runstat_f
#' varname=attr(x, "meta")["grpby.tag"]
#' dict <- dict.MELC
#' dict <- dict_example
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

#' Label a flattened matrix. Labels flattened columns according to dictionary codings, 
#' and applies the specified row and col dimension label (if any).
#' 
#' @param mx.flattened
#'  a flattened matrix, ie: a matrix that has rows that contain
#'  groups of values.  
#'  
#'  A flattened matrix has a meta attribute that specifies the grouping
#'  used (if any) and the varname that identifies the codings to apply, eg:
#'  meta=c(grpby.tag="sex", varname="disability_state") 
#' 
#'  If grpby.tag is NULL or NA, then the flattened code will be in the form "0", 
#'  i.e: no grping codes only varname codes. These will be converted in the output
#'  to the corresponding varname category name. 
#' 
#'  If grpby.tag is specified then the flattened matrix will have flattened codes 
#'  for column names. A flattened code is in the form "0 1", where the first value 
#'  is a grping code and the second a varname code. These will be converted
#'  in the output to the corresponding group and varname category names. 
#' 
#' @param dict
#'  dictionary object
#' 
#' @param row.dim.label
#'  name of the entire row dimension
#' 
#' @param col.dim.label
#'  name of the entire col dimension
#' 
#' @export
#' @examples 
#' \dontrun{
#' mx.flattened <- structure(matrix(c(1,2,1,3,1,4,2,2,2,3,2,4), nrow=2, byrow = TRUE, dimnames=list(NULL, c("F 1", "F 2", "F 3", "M 1", "M 2", "M 3"))), meta=c(grpby.tag="sex", varname="disability_state"))
#' dict <- dict_demo
#' label_flattened_mx(mx.flattened, dict, row.dim.label="Year")
#' }
label_flattened_mx <- function(mx.flattened, dict, row.dim.label="", col.dim.label="") {
	varname <- attr(mx.flattened, "meta")["varname"]
	grpby.tag <- attr(mx.flattened, "meta")["grpby.tag"]
	
	#label
	colnames(mx.flattened) <- dict$cmatchFlattened(colnames(mx.flattened), varname, grpby.tag)
	names(dimnames(mx.flattened)) <- c(row.dim.label,col.dim.label)
	
	structure(mx.flattened, grpingNames=  attr(colnames(mx.flattened), "grpingNames"))

}


label_flattened_mx_grping.and.CIs <- function(mx.flattened, dict, row.dim.label="", col.dim.label="") {
	varname <- attr(mx.flattened, "meta")["varname"]
	grpby.tag <- attr(mx.flattened, "meta")["grpby.tag"]
	
	#colnames start off as:
	# e.g.
	#  [1] "1 0 Mean"  "1 0 Lower" "1 0 Upper" "1 1 Mean"  "1 1 Lower" "1 1 Upper"
	#[7] "2 0 Mean"  "2 0 Lower" "2 0 Upper" "2 1 Mean"  "2 1 Lower" "2 1 Upper"
	#[13] "3 0 Mean"  "3 0 Lower" "3 0 Upper" "3 1 Mean"  "3 1 Lower" "3 1 Upper"
	
	# Need to remove the Mean, Lower, and Upper parts
	col.names <- colnames(mx.flattened)
	#identify the position of the last space in each name
	space.ids <- str_locate_all(col.names, " ")
	num.spaces <- nrow(space.ids[[1]])
	pos.last.space <- lapply(space.ids, function(x) {x[num.spaces,2]})
	pos.last.space.vec <- rep(NA, length(col.names))
	for (i in 1:length(pos.last.space.vec)) {
		pos.last.space.vec[i]<-pos.last.space[[i]]
	}
	sub.col.names <- str_sub(col.names, 1, pos.last.space.vec-1)
	
	if (!is.null(grpby.tag)) {
		if (!is.na(grpby.tag)) {
			if (grpby.tag=="") {
				grpby.tag <- NA
			}
		}
	}

	colnames(mx.flattened) <- dict$cmatchFlattened(sub.col.names, varname, grpby.tag)
	names(dimnames(mx.flattened)) <- c(row.dim.label,col.dim.label)
	
	result <- structure(mx.flattened, grpingNames=attr(colnames(mx.flattened), "grpingNames"))
	return(result)
}

#' Calculates the proportions within row groupings of a flattened matrix. 
#' 
#' @param mx.grped.rows
#'  a matrix with grped rows, ie: within each row there are groups of 
#'  columns that form a set. Proportions are then calculated within these groups.
#' @param groupnameprefixes
#'	 names of the groups. If NULL then no groups.
#' @return
#'  the original matrix but with its values converted to proportions.
#'  Preserves names and any "meta" attribute of \code{mx.grped.rows)}.
#' 
#' @export
#' @examples
#' 
#' mx.grped.rows  <- structure(matrix(c(1,2,1,3,1,4,2,2,2,3,2,4,1,2,3,4), nrow=2, byrow = TRUE, dimnames=list(NULL, c("Female 1", "Female 2", "Female 3", "Female 4", "Male 1", "Male 2", "Male 3", "Male 4"))), meta=c(varname="disability_state", grpby.tag="sex"))
#' groupnameprefixes<-c("Female","Male")
#' 
#' mx.grped.rows <- matrix(c(1,2,1,3,1,4,2,2,2,3,2,4), nrow=2, byrow = TRUE)
#' groupnameprefixes<-NULL
#' 
#' mx.grped.rows <- structure(matrix(c(1,2,1,3,1,4,2,2,2,3,2,4,1,2,3,4), nrow=2, byrow = TRUE, dimnames=list(NULL, c("65-69 No disability", "65-69 Mild disability", "65-69  Moderate disability", "65-69  Severe disability", "70-74  No disability", "70-74 Mild disability", "70-74 Moderate disability", "70-74 Severe disability"))), meta=c(varname="disability_state", grpby.tag="age_grp_output"))
#' groupnameprefixes<-c("65-69", "70-74", "75-79", "80-84", "85+" )
#' 
#' mx.grped.rows <- structure(matrix(c(1,2,1,3,1,4,2,2,2,3,2,4,1,2,3,4), nrow=2, byrow = TRUE, dimnames=list(NULL, c("Male No disability", "Male Mild disability", "Male Moderate disability", "Male Severe disability", "Female No disability", "Female  Mild disability", "Female Moderate disability", "Female  Severe disability"))), meta=c(varname="disability_state", grpby.tag="sexLvl1"))
#' groupnameprefixes<-c("Male","Female")
#' 
#' mx.grped.rows <- matrix(c(1:4), nrow=1)
#' groupnameprefixes<-NULL
#' 
#' prop.table.mx.grped.rows(mx.grped.rows, groupnameprefixes)
prop.table.mx.grped.rows <- function (mx.grped.rows, groupnameprefixes, CI=FALSE, num.runs) {

	grpingNames <- attr(mx.grped.rows,"grpingNames")
	
	if (is.null(groupnameprefixes) || is.null(grpingNames)) {
		grpby <- rep(1, ncol(mx.grped.rows))
	} else {
		grpby <- match(grpingNames,groupnameprefixes)
		assert(!is.na(grpby))	
	}
	# get proportions by grp
	mx.grped.rows.prop <- apply(mx.grped.rows, ROW, prop.table.grpby, grpby=grpby, CI=CI, num.runs=num.runs)
	
	mx.grped.rows.prop.t <- t(mx.grped.rows.prop)
	
	result <- structure(mx.grped.rows.prop.t, meta = attr(mx.grped.rows, "meta"), dimnames=dimnames(mx.grped.rows))
	
	return(result)
}

