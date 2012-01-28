#' Categorical adjusment
#' 
#' Before or during the simulation the user may wish to specify the proportion of category values 
#' desired for a simframe variable
#' Eg: a user may wish the proportion of home owners in year 2 to be 0.4, 0.6
#' Desired proportions can be specified in a categorical adjustment matrix in which rows = iterations
#' In the above example instead of simulating the home ownership variable in year 2, it will be set to 
#' the desired proportions 0.4, 0.6 
#' A desired proportion of NA will leave the variable unchanged
#' If propensities are supplied they will be used to select which micro-units to adjust, otherwise the 
#' selection will be random
#' Propensities are specified via the global list variable propens.all


#' Create empty categorical variable adjustment matrices for specified number of iterations.
#' Initial matrix values are NA (i.e: no adjustment).
#'
#' @param cat.varnames
#'  names of level vars to create adjustment matrices for
#' @param dict 
#'  Dictionary object. Used to lookup codings for cat.varnames
#' @param rows row names, or a numeric scalar for the number of rows
#'  number of iterations to create
#' 
#' @examples
#' dict <- dict.MELC
#' cat.varnames <- c("z1homeownLvl1", "catpregsmk2") 
#' rows = 5
#' 
#' dict <- dict_demo
#' cat.varnames  <- c("sex","age_grp", "alive")
#' rows = 100
#' 
#' createAdjustmentMatrices(cat.varnames, dict, rows)
createAdjustmentMatrices <- function(cat.varnames, dict, rows) {
	
	cat.adjustments <- lapply(cat.varnames, function (varname) {
				coding <- dict$codings[[varname]]
				if (is.null(coding)) stop(gettextf("No codings for %s", varname))
				
				createAdjustmentMatrix(varname, coding, rows)
			})
	names(cat.adjustments) <- cat.varnames			
	
	cat.adjustments 
}

#' Creates an empty adjustment matrix of NAs. An adjustment matrix contains cells
#' for each categorical value across a supplied number of rows. Each row 
#' represents an iteration.
#' 
#' @param varname
#'  variable name. This can be a standard variable name, eg: "catpregsmk2"
#'  or a level variable name, eg: "z1homeownLvl1". 
#' @param coding
#'  codings for the variable, eg: c('0'='Own home','1'='Not owned')
#' @param rows row names, or a numeric scalar for the number of rows in 
#'  which case the rows will be labelled "Year 1", "Year 2"... up to rows.
#' @param is_level_var
#'  is this a binary level variable? i.e: are the values for this variable
#'  stored in multiple binary variables, eg: SESBTHLvl1, SESBTHLvl2, SESBTHLvl3?
#'  Defaults to testing whether varname ends in LvlX.
#' 
#' @return
#'  a matrix of NAs with columns specified by codings, and rows specified by rows
#'  and a "varnames" attribute which specifies the variables that need to be
#'  adjusted. If varname is a level variable then this will contain all the
#'  individual binary level varnames, eg: "z1homeownLvl0", "z1homeownLvl1"
#'  otherwise it will just varname.
#'
#' @export 
#' @examples
#' varname = "z1homeownLvl1"; coding <- c('Own home'=0,'Not owned'=1)
#' varname = "catpregsmk2"; coding <- c('0'=0, '1-5'=3, '6-10'=8, '11-20'=16, '>20'=27)
#' rows = 5
#' createAdjustmentMatrix(varname, coding, rows)
createAdjustmentMatrix <- function(varname, coding, rows, is_level_var = grepl("Lvl.$", varname)) {
	
	if (is_numeric_scalar(rows)) {
		rows <- paste("Year", seq(rows))
	}
	
	if (is_level_var) {
		varnames <- paste(strip_lvl_suffix(varname), "Lvl", coding, sep="")	
	} else {
		varnames <- varname
	}
	
	structure(namedMatrix(rows, paste(names(coding),"(%)")), varnames=varnames)
}

#' strip_lvl_suffix(varname)
strip_lvl_suffix <- function(varname) {
	gsub("Lvl.$", "", varname)
}
