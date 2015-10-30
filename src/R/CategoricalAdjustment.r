#' Categorical adjusment
#' 
#' Before or during the simulation the user may wish to specify the proportion of category values desired for a simframe variable. Eg: a user may wish the proportion of home owners in year 2 to be 0.4, 0.6. Desired proportions can be specified in a categorical adjustment matrix in which rows = iterations. In the above example instead of simulating the home ownership variable in year 2, it will be set to the desired proportions 0.4, 0.6. A desired proportion of NA will leave the variable unchanged. If propensities are supplied they will be used to select which micro-units to adjust, otherwise the selection will be random Propensities are specified via the global list variable propensities.
#'
#' Create empty categorical variable adjustment matrices for specified number of iterations.
#' Initial matrix values are NA (i.e: no adjustment).
#'
#' @param cat.varnames
#'  names of vars to create adjustment matrices for. This can be a name of
#'  a single variable, eg: "catpregsmk2" or the name of a multi-level binary 
#'  variable that eg: "z1accomLvl1". A multi-level binary variable will be
#'  part of a set eg: c("z1accomLvl0", "z1accomLvl1") of variables. Only
#'  one of the multi-level binary variables in the set need be specified.
#'  The others will be determined from the dictionary codings. 
#' @param dict 
#'  Dictionary object. Used to name the columns of the adjustment matrices
#'  and also to determine the set of variables when a multi-level binary
#'  variable is supplied via cat.varnames.
#' @param rows row names, or a numeric scalar for the number of rows
#'  number of iterations to create
#'
#' @return 
#' A list of empty categorical variable adjustment matrices 
#' 
#' @export
#' @examples
#' \dontrun{
#' dict <- dict.MELC
#' cat.varnames <- c("z1homeownLvl1", "catpregsmk2") 
#' rows = 5
#' 
#' dict <- dict_demo
#' cat.varnames  <- c("sex","age_grp", "alive")
#' rows = 100
#' 
#' createAdjustmentMatrices(cat.varnames, dict, rows)
#' }
createAdjustmentMatrices <- function(cat.varnames, dict, rows) {
  
  cat.adjustments <- lapply(cat.varnames, function (varname) {
    coding <- dict$codings[[varname]]
    if (is.null(coding)) stop(gettextf("No codings for %s", varname))
    
    createAdjustmentMatrix(varname, coding, rows)
  })
  
  names(cat.adjustments) <- sapply(cat.varnames, function (varname) {
    if (is_level_var(varname)) strip_lvl_suffix(varname) else varname
  })
  
  cat.adjustments 
}

#' Creates an empty adjustment matrix of NAs. 
#' An adjustment matrix contains cells for each categorical value across a supplied number of rows. Each row  represents an iteration.
#' 
#' @param varname
#'  variable name. This can be a standard variable name, eg: "catpregsmk2"
#'  or a level variable name, eg: "z1homeownLvl1".
#'  
#' @param coding
#'  codings for the variable, eg: c('0'='Own home','1'='Not owned')
#' 
#' @param rows 
#' row names, or a numeric scalar for the number of rows in 
#'  which case the rows will be labelled "Year 1", "Year 2"... up to rows.
#' 
#' @param is_a_level_var
#'  is this a multi-level binary variable? i.e: are the values for this variable
#'  stored in multiple binary variables, eg: SESBTHLvl1, SESBTHLvl2, SESBTHLvl3?
#'  Defaults to testing whether varname ends in LvlX.
#' 
#' @param cont.binbreaks
#' binbreaks for continuous variable. It is the scale to convert a continuous 
#' variable to categorical variable when we do adjustment on it.
#' 
#' @param catToContModels
#' It is the models that convert the adjustment on categorical variable (we convert 
#' from a continuous variable according to binbreaks) back to the correspongding 
#' continuous variable. 
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
#' \dontrun{
#' varname = "z1homeownLvl1"; coding <- c('Own home'=0,'Not owned'=1)
#' varname = "catpregsmk2"; coding <- c('0'=0, '1-5'=3, '6-10'=8, '11-20'=16, '>20'=27)
#' rows = 5
#' createAdjustmentMatrix(varname, coding, rows)
#' }
createAdjustmentMatrix <- function(varname, coding=cont.binbreaks[-1], rows, is_a_level_var = is_level_var(varname), cont.binbreaks=NULL, catToContModels=NULL) {
  
  if (is_numeric_scalar(rows)) {
    rows <- paste("Year", seq(rows))
  }
  
  if (is_a_level_var) {
    varnames <- paste(strip_lvl_suffix(varname), "Lvl", coding, sep="")	
  } else {
    varnames <- varname
  }
  
  structure(namedMatrix(rows, paste(names(coding),"(%)")), varnames=varnames, cont.binbreaks=cont.binbreaks, catToContModel=catToContModels)
}


#' Create a propensity array from a dataframe, for a variable with only 2 categories 
#' across multiple iterations.
#' 
#' @param df
#'  dataframe containing 1 column per iteration.
#'  This column represents the propensity to change from the 1st category to the 2nd category.
#'  
#' @return propensity array with
#' rows - the values for each individual micro-unit
#' cols - propensity to change from the 1st category to the 2nd category.
#' z dim - iterations/years
#' 
#' @export
#' @examples
#' \dontrun{
#' df <- data.frame(year1 = 1:10/100, year2 = 11:20/100)
#' create2CategoryPropensityArray(df)
#' }

create2CategoryPropensityArray <- function(df) {
  #convert dataframe to array with
  #rows = obs, cols = "Level 1", z = vars 
  array(as.matrix(df), dim=c(nrow(df), 1, ncol(df)), 
        dimnames=list(rownames(df), "1st to 2nd category propensity", colnames(df))  )
}


#' Create a propensity array from a dataframe, for only a single iteration.
#' 
#' @param df
#'  dataframe containing n columns where n is one less than the number of categories.
#'  This column represents the propensity to change from the 1st category to the 2nd category.
#' @param iteration_name
#'  used to label the single iteration in the z dim  
#'
#' @return propensity array with
#' rows - the values for each individual micro-unit
#' cols - propensity to change from the 1st category to the 2nd category.
#' z dim - iterations/years. Only 1 iteration.
#' 
#' @export
#' @examples
#' \dontrun{
#' df <- data.frame(year1_cat1 = 1:10/100, year1_cat2 = 11:20/100)
#' iteration_name <- "Year 1" 
#' createSingleIterationPropensityArray(df, iteration_name)
#' }

createSingleIterationPropensityArray <- function(df, iteration_name) {
  #convert dataframe to array with
  #rows = obs, cols = cols, z = "At Birth"
  array(as.matrix(df), dim=c(nrow(df), ncol(df), 1), 
        dimnames=list(rownames(df), colnames(df), iteration_name)  )
}



#' Check if each element of a character vector has the trailing "LvlX" (if any) 
#' where X is any character
#' 
#' @param varname
#' character vector to check
#' 
#' @return 
#' a vector of logical value
#' 
#' @export 
#' @examples
#' \dontrun{
#' varname <- c("fooLvl1", "bar")
#' is_level_var(varname)
#' }

is_level_var <- function(varname) {
  grepl("Lvl.$", varname)
}



#' Remove trailing "LvlX" (if any) where X is any character
#' 
#' @param varname
#'  character vector to strip
#' 
#' @return 
#' a vector of characters without "LvlX"
#' 
#' @export 
#' @examples
#' \dontrun{
#' varname <- c("fooLvl1", "bar")
#' strip_lvl_suffix(varname)
#' }

strip_lvl_suffix <- function(varname) {
  gsub("Lvl.$", "", varname)
}



#' Evaluate logiset expression attribute for the variable.
#' 
#' @param desired_props
#' a vector that is the proportions requested by the user.
#' The vector is the length of the number of distinct values of the variable
#' being modified.
#' 
#' @param simframe
#' the simframe to evaluate
#' 
#' @param varname
#' variable name.
#'  
#' @return
#' a vector of logical value
#'
#' @export 
#' @examples
#' \dontrun{
#' desired_props <- structure(1, logiset="alive & residential")
#' simframe <- env.base$simframe
#' evaluateLogisetExprAttribute(desired_props, simframe)
#' }
evaluateLogisetExprAttribute <- function(desired_props, simframe, varname="") { 
  
  logiset_expr <-attr(desired_props, "logisetexpr")
  cat("Evaluating logiset expression: \"",logiset_expr,"\" for variable ",varname, "\n", sep="")
  if (is.null(logiset_expr )) {
    logiset<-NULL
  } else {
    logiset<- eval(parse(text=logiset_expr), envir = simframe)
  }
  logiset
}



#' Set the subgroup expression to all cat.adjustments if the subgroup expression exists. 
#' Otherwise, remove the subgroup expression.
#' 
#' @param subgroupExpression
#'  the subgroup expression that is requested by the user. 
#'  It specify the subgroup which is going to be adjusted.
#'
#' @return 
#' NULL
#'
#' @export 
#' @examples
#' \dontrun{
#' subgroupExpression <- "mhrswrk < 20"
#' setGlobalSubgroupFilterExpression(subgroupExpression)
#' attr(env.scenario$cat.adjustments[[1]], "logisetexpr")
#' }
setGlobalSubgroupFilterExpression <- function(subgroupExpression) {
  if (is.null(subgroupExpression) || subgroupExpression == "") {
    return(removeGlobalSubgroupFilterExpression())
  }
  
  cat("Setting global subgroup expression \"",subgroupExpression,"\"\n", sep="")
  
  for (i in 1:length(env.scenario$cat.adjustments)) {
    attr(env.scenario$cat.adjustments[[i]], "logisetexpr") <- subgroupExpression
  }
}




#' Clear the subgroup expression for all cat.adjustments.
#' 
#' @return
#' NULL
#'
#' @export 
removeGlobalSubgroupFilterExpression <- function() {
  cat("Clearing global subgroup expression\n")
  
  for (i in 1:length(env.scenario$cat.adjustments)) {
    attr(env.scenario$cat.adjustments[[i]], "logisetexpr") <- NULL
  }
}
