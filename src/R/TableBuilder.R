#' Returns a dataset for use in a table by the Table Builder GUI.
#' 
#' Currently cannot do freqs for final outcomes (can only do freqs for those vars with binbreaks)
#' Also currently can only use a true categorical variable as a groupby variable - this coould be changed.
#' 
#' If the user defined a logiset/subgroup expression inthe scenario weightings screen and then
#' goes to tableBuilder() and sets a grpbyName, then the results they get will be on the entire 
#' dataset, not just on their subgroup they defined earlier.  The user can't define a logiset
#' expression in tableBuilder - the logisetexpr parameter is there so it can be used to show the 
#' user, in the scenario weightings screen, the distributions of the variable of interest for 
#' their subgroup only so they can better choose the proportions for their subgroup scenario.   
#' 
#' @param envName 
#'  the environment to use - Base, Scenario etc.
#' 
#' @param statistic
#'  the summary measure to use in producing the dataset - frequencies, means, quintiles
#' 
#' @param variableName
#'  the variable to use in producing the dataset
#' 
#' @param grpbyName
#'  a subgroup by which to examine the variable
#' 
#' @param CI
#'  logical indicating whether 95% confidence intervals should be generated
#' 
#' @param logisetexpr
#'  a character expression which defines the logiset variable
#' 
#' @param dict
#'  Dictionary object.
#' 
#' @param not.in.logiset
#'  logical.  If TRUE, then the results will be calculated on those not in the logiset rather than those in the logiset.
#' 
#' @return 
#'  a summary table for the entire or subgroup of the variable of interest.
#'  
#' @export
#' @examples
#' \dontrun{
#' dict <- dict_demo
#' test <- tableBuilder(envName="Scenario", "frequencies", "disability_state", "",CI=TRUE, logisetexpr=NULL)
#' test <- tableBuilder(envName="Base", statistic="means", variableName="age", grpbyName="", CI=TRUE, logisetexpr=NULL)
#' test <- tableBuilder("Base", "quintiles", "IQ", "",CI=TRUE, logisetexpr=NULL)
#' test <- tableBuilder("Base", "frequencies", "disability_state", grpbyName="alive",CI=FALSE, logisetexpr=NULL)
#' test <- tableBuilder(envName="Base", statistic="means", variableName="earnings", grpbyName="qualification", CI=TRUE, logisetexpr=NULL)
#' test <- tableBuilder(envName="Base", statistic="means", variableName="earnings", grpbyName="qualification", CI=F, logisetexpr=NULL)
#' test <- tableBuilder(envName="Base", statistic="quintiles", variableName="earnings", grpbyName="", CI=FALSE, logisetexpr="alive==TRUE", not.in.logiset=TRUE)
#' test <- tableBuilder(envName="Base", statistic="frequencies", variableName="alive", grpbyName="sex", CI=FALSE, dict=dict_demo)
#' test <- tableBuilder(envName="Base", statistic="frequencies", variableName="disability_state", grpbyName="qualification", CI=FALSE)
#' tableBuilder(envName="Base", statistic="means", variableName="earnings", grpbyName="sex", CI=FALSE, dict=dict_demo)}
tableBuilder <- function(envName, statistic, variableName, grpbyName="", CI=TRUE, logisetexpr=NULL, dict=dict_demo, not.in.logiset=FALSE) {
  
  
  if (!is.null(logisetexpr)) {
    if (logisetexpr == "") {  
      logisetexpr <- NULL
    }
  }
  
  #select env base on envName, eg: base (env.base) or scenario (env.scenario)
  if (envName=="Base") {
    env <- env.base
  } else {		
    env <- env.scenario
  }
  
  #do we need to have the simframe as an input or do we need to do an environment thing?
  catvars <- getOutcomeVars(env$simframe, "categorical")
  contvars <- c(getOutcomeVars(env$simframe, "continuous"), "age")
  presimvars <- names(env$presim.stats)
  
  run_results <- env$modules[[1]]$run_results
  #NUM_ITERATIONS <- ncol(env$modules[[1]]$outcomes[[1]])
  
  run_tables <- lapply(run_results, function(single_run) {
    ##single_run <- run_results[[1]]
    mx <- single_run$outcomes[[variableName]]
    if (is.null(mx)) {
      #variable is a time-invariant "presim" variable
      mx <- env$simframe[[variableName]]
      if (is.null(mx)) {
        #variable is a time-invariant "Lvl" variable and is only present in the simframe with its Lvl suffix
        which.vars <- str_locate_all(names(env$simframe), variableName)
        lvl.vars <- which(lapply(which.vars, length)>0)
        mx <- binary.levels.combine(env$simframe[[lvl.vars[1]]], env$simframe[[lvl.vars[2]]], env$simframe[[lvl.vars[3]]])
        #above code OK cos all the time-invariant Lvl vars only have three categories
        #but would be better to change code to be more generic
      }
      mx <- matrix(rep(mx, NUM_ITERATIONS), ncol=NUM_ITERATIONS, byrow=FALSE)
    }
    mx_wtotals <- structure(mx, varname=attr(mx,"varname"))
    
    if ( (variableName%in%contvars) & (tolower(statistic)=="frequencies") ) {
      if (is.null(binbreaks[[variableName]])) {
        stop(gettextf("Cannot produce frequencies for %s because it has no binbreaks",variableName))
      }
      # frequency for a continuous variable 
      mx <- apply(mx, COL, function(x) {
        #x <- mx[,1]
        bin(x, binbreaks[[variableName]])
      })
      mx_wtotals <- apply(mx_wtotals, COL, function(x) {bin(x, binbreaks[[variableName]])})
    }
    
    attr(mx, "varname") <- variableName
    attr(mx_wtotals, "varname") <- variableName
    
    grpbymx <- single_run$outcomes[[grpbyName]]
    
    if (is.null(grpbymx)) {
      if (grpbyName=="") {
        #if no grouping create a matrix of NAs
        grpbymx <- matrix(nrow=nrow(mx), ncol=NUM_ITERATIONS)
      } else {
        #if not outcome, use base variable from children
        grpbymx <- env$simframe[[grpbyName]]
        if (is.null(grpbymx)) {
          #variable is a time-invariant "Lvl" variable and is only present in the simframe with its Lvl suffix
          which.vars <- str_locate_all(names(env$simframe), grpbyName)
          lvl.vars <- which(lapply(which.vars, length)>0)
          grpbymx <- binary.levels.combine(env$simframe[[lvl.vars[1]]], env$simframe[[lvl.vars[2]]], env$simframe[[lvl.vars[3]]])
        }
      }
    }
    if (length(grpbymx)==nrow(mx)) {
      #grpby is a vector or a 1 column matrix
      grpbymx_wtotals <- matrix(rep(grpbymx, ncol(mx)+1), ncol=ncol(mx)+1)
      attr(grpbymx_wtotals, "varname") <- grpbyName
    } else {
      #grpbymx is a matrix with 18 columns
      grpbymx_wtotals <- structure(grpbymx, varname=attr(grpbymx,"varname"))
      if (is.null(attr(grpbymx_wtotals, "varname"))) {
        attr(grpbymx_wtotals, "varname") <- grpbyName
      }
    }
    
    #define logiset variable if logiset expression was specified
    outcomes <- single_run$outcomes
    if (!is.null(logisetexpr)) {
      prepended.exprs <- prepend.paths(logisetexpr)
      logiset.expr <- unlist(prepended.exprs["sg.expr"])
      names(logiset.expr) <- ""
      simframe <- env$simframe
      eval(parse(text=logiset.expr)) #creates sg.var which is actually a logiset.var
      logiset <- sg.var
      logiset <- as.matrix(logiset)
      
      if (ncol(logiset)>1) {
        logiset_wtotals <- structure(logiset, varname=attr(logiset,"varname"))
      } else {
        logiset_wtotals <- logiset
      }
      
      if (not.in.logiset==TRUE) {
        logiset <- !logiset
        logiset_wtotals <- !logiset_wtotals
      }
    } else if (is.null(logisetexpr)) {
      logiset <- NULL
      logiset_wtotals <- NULL
    }
    logiset <<- logiset
    logiset_wtotals <<- logiset_wtotals
    
    #select statistic generating function based on statistic parameter
    if (tolower(statistic)=="frequencies") {
      if (all(is.na(grpbymx))) {
        grpby <- NULL
      } else {
        grpby <- grpbymx
      }
      table_mx_cols_MELC(mx, grpby=grpby, grpby.tag=grpbyName, logiset=logiset, dict=dict)
      
    } else if (tolower(statistic)=="means") {
      if (grpbyName=="") {
        grpby <- NULL
      } else {
        grpby <- grpbymx_wtotals
      }
      mean_mx_cols_BCASO(mx_wtotals, grpby=grpby, grpby.tag=grpbyName, logiset=logiset_wtotals, dict=dict)
      
    } else if (tolower(statistic)=="quintiles") {
      quantile_mx_cols_BCASO(mx_wtotals, grpby=grpbymx_wtotals, grpby.tag=grpbyName, new.names=c("Min", "10th", "25th", "50th", "75th","90th","Max"), probs=c(0,.1,.25,.5,.75,.9,1), logiset=logiset_wtotals, na.rm=TRUE, dict=dict)
    }
  })
  
  #select collator based on statistic
  if (tolower(statistic)=="frequencies") {
    if (variableName%in%catvars) {
      
      result <- collator_freqs2(run_tables, dict=dict, CI=CI, binbreaks=binbreaks[[variableName]])
      
    } else if (variableName%in%contvars) {
      result <- collator_freqs2(run_tables, dict=dict, CI=CI, binbreaks=binbreaks[[variableName]])
      
    } else {
      stop(gettextf("Unknown variable %s. Cannot not find in catvars or contvars.", variableName))
    }
    
  } else if (tolower(statistic)=="means") {
    result <- collator_means(run_tables, dict=dict, CI=CI, NA.as.zero=FALSE)
    attr(result, "meta")[["varname"]] <- variableName
    
    
  } else if (tolower(statistic)=="quintiles") {
    result <- collator_list_mx(run_tables, CI=CI, NA.as.zero=FALSE)
    
  }
  
  if (variableName %in% presimvars) {
    if (grpbyName=="" | grpbyName %in% presimvars) {
      #take first non NA year
      NA.year <- apply(result, 1, function(x) { all(is.na(x)) })
      first.non.na.yr <- which(NA.year==FALSE)[1]
      result <- result[first.non.na.yr,]
      if ((CI==TRUE)&(length(run_tables)>1)) {
        #take every third name
        nms <- names(result)[(1:(length(result)/3))*3-2]
        #remove "Mean" from it
        nms.noMean <- rep(NA, length(nms))
        for (i in 1:length(nms)) {
          nms.noMean[i] <- str_sub(nms[i], 1, regexpr("Mean", nms)[[i]]-2)
        }
        result <- matrix(result, byrow=TRUE, ncol=3)
        rownames(result) <- nms.noMean
        colnames(result) <- c("Mean", "Lower", "Upper")
      }
    }
  }
  
  return(result)
}
