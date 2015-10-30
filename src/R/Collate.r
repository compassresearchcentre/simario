# Functions related to collation of run results.
# 
# Author: oman002
###############################################################################

#' Collate frequencies. 
#'
#'Performs the following: 
#' \itemize{
#'   \item Takes mean without confidence intervals using \code{\link{collator_mutiple_lists_mx}}  
#'   \item Labels the result using the dictionary 
#'   \item Converts frequencies to percentages
#'   \item Labels the output
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
#' @param numbers
#'  If TRUE, it produces frequency table. Otherwise, it will return 
#'  percentage table.
#' 
#' @param CI
#'  if TRUE and length(runs) > 1, lower and upper confidence intervals 
#'  are returned in additional columns
#' 
#' @return
#'  a matrix of collated result for each iteration
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
    runs_mx <- label_flattened_mx_grping.and.CIs(runs_mx, dict, row.dim.label, col.dim.label, CI=CI, num.runs = num.runs)
    if (numbers==FALSE) {
      resultCI <- percentages_flattened_mx(runs_mx, dict, CI, num.runs=num.runs)
    } else {
      resultCI <- runs_mx
    }
    #label CI components
    run1_array <- as_array_list_mx(runs[[1]])
    numGroups <- dim(run1_array)[COL] #number of group-by groups
    colnames(resultCI) <- paste(colnames(resultCI), rep(c("Mean", "Lower", "Upper"), numGroups))
    result <- resultCI
  }
  return(result)
}

#'A new version of collator_freqs() that calls collator_mutiple_lists_mx2() (and then
#' mean_array_pctile_CIs2()) instead of then 'non-2' versions.  This means that 
#' percenatages will be averaged over runs instead of frequencies and the confidence
#' intervals will be calculated on percentages rather than frequencies.  See documentation
#' on mean_array_pctile_CIS2() more more details.  
#' In contrast to the original collator_freqs() this function does not take a numbers 
#' argument.  It will only produce percentages and not raw numbers or frequencies.
#' Another difference between this function and the original collator_freqs() is that
#' this function takes cat.adjustment and binbreaks arguments.  These are sent to  
#' collator_mutiple_lists_mx2().
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
#' @param CI
#'  if TRUE and length(runs) > 1, lower and upper confidence intervals 
#'  are returned in additional columns
#' 
#' @param cat.adjustments
#' The cat.adjustments list containing the cat.adjustments for multiple variables.
#' Within the function the specific cat.adjustments for the variable of interest are 
#' extracted from the list in further embedded functions.  Either cat.adjustments or 
#' binbreaks are needed if frequencies of a continuous variable are being requested.  
#' 
#' 
#' @param binbreaks 
#' The binbreaks for the specific outcome variable.  Either binbreaks or cat.adjustments 
#' may be provided to the function.
#' 
#' @return 
#' a matrix of collated result for each iteration
#' 
#' @export
collator_freqs2 <- function (runs, dict, row.dim.label="Year", col.dim.label="", CI=FALSE, cat.adjustments=NULL, binbreaks=NULL) {
  #check rownames are present for each iteration for each run
  #(will cause error in collator_mutiple_lists_mx2 otherwise)
  #and if not present make the rownames be the same as for iterations where they are present
  runs <- lapply(runs, check.row.names)
  
  runs_mx <- collator_mutiple_lists_mx2(runs, CI, cat.adjustments, dict, binbreaks)
  
  num.runs <- length(runs)
  
  if ((CI==FALSE|(num.runs==1))) {
    #runs_mx <- label_flattened_mx(runs_mx, dict, row.dim.label, col.dim.label)
    runs_mx <- label_flattened_mx_grping.and.CIs(runs_mx, dict, row.dim.label, col.dim.label, CI=FALSE, num.runs=num.runs, binbreaks=binbreaks)
    result <- runs_mx
  } else if ((CI==TRUE)&&(num.runs>1)) {
    runs_mx <- label_flattened_mx_grping.and.CIs(runs_mx, dict, row.dim.label, col.dim.label, CI=CI, num.runs=num.runs, binbreaks=binbreaks)
    resultCI <- runs_mx
    
    #label CI components
    run1_array <- as_array_list_mx(runs[[1]])
    numGroups <- dim(run1_array)[COL] #number of group-by groups
    colnames(resultCI) <- paste(colnames(resultCI), rep(c("Mean", "Lower", "Upper"), numGroups))
    result <- resultCI
  }
  result <- result*100
  return(result)
}


collator_freqs2_NPRESCH <- function (runs, dict, row.dim.label="Year", col.dim.label="", CI=FALSE, cat.adjustments=NULL, binbreaks=NULL) {
  #workaround to make work for NPRESCH - put values into the matrices for years 1 - 3 (at the
  #moment they are 0s
  num.runs <- length(runs)
  for (k in 1:num.runs) {
    runs[[k]][[1]] <- runs[[1]][[6]]
    runs[[k]][[2]] <- runs[[1]][[6]]
    runs[[k]][[3]] <- runs[[1]][[6]]
    runs[[k]][[4]] <- runs[[1]][[6]]
    runs[[k]][[5]] <- runs[[1]][[6]]
  }
  
  runs_mx <- collator_mutiple_lists_mx2(runs, CI, cat.adjustments, dict, binbreaks)
  
  if ((CI==FALSE|(num.runs==1))) {
    #runs_mx <- label_flattened_mx(runs_mx, dict, row.dim.label, col.dim.label)
    runs_mx <- label_flattened_mx_grping.and.CIs(runs_mx, dict, row.dim.label, col.dim.label, CI=FALSE, num.runs=num.runs, binbreaks=binbreaks)
    result <- runs_mx
  } else if ((CI==TRUE)&&(num.runs>1)) {
    runs_mx <- label_flattened_mx_grping.and.CIs(runs_mx, dict, row.dim.label, col.dim.label, CI=CI, num.runs=num.runs, binbreaks=binbreaks)
    resultCI <- runs_mx
    
    #label CI components
    run1_array <- as_array_list_mx(runs[[1]])
    numGroups <- dim(run1_array)[COL] #number of group-by groups
    colnames(resultCI) <- paste(colnames(resultCI), rep(c("Mean", "Lower", "Upper"), numGroups))
    result <- resultCI
  }
  result <- result*100
  
  varname1 <- attr(result, "meta")["varname"]
  if (is.null(varname1)) {
    varname1 <- attr(result, "varname")
  }
  
  meta.attributes <- attr(result, "meta")
  
  result <- result[6,]
  #this makes it lose it varname attributes - need to put back on
  #reattach meta attributes
  result <- structure(result, meta=meta.attributes)
  #also put as 'normal' attribute in case meta.attributes are NULL
  attr(result, "varname") <- as.character(varname1)
  
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
#' @param CI
#'  if TRUE and length(runs) > 1, lower and upper confidence intervals 
#'  are returned in additional columns
#' 
#' @return 
#'  a matrix of collated result for each iteration
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
  runs_mx <- collator_mutiple_lists_mx(runs=runs, CI=CI)
  grpby.tag <- attr(runs_mx, "meta")["grpby.tag"]
  
  zero_cat_cols <- identify_zero_category_cols(runs_mx)
  
  #the above code give incorrect categories with 0s for the outcome 
  #we only want to remove those columns that are 0 for the outcome, not also for the grouping variable
  #(which is what the above code does)
  if (!is.null(grpby.tag)) {
    if (!is.na(grpby.tag)) {
      if (grpby.tag!="") {
        zero_cat_cols <- identify_zero_category_cols_bygrp(runs_mx)
        if (length(zero_cat_cols)==0) {
          zero_cat_cols <- identify_zero_category_cols(runs_mx)
        }
      }
    }
  }
  
  numZ <- length(runs) #number of runs
  
  colnames(runs_mx)[is.na(colnames(runs_mx))] = "NA"
  
  #browser()
  
  if ((CI==FALSE)|(numZ==1)) {
    #runs_mx <- label_flattened_mx(runs_mx, dict, row.dim.label, col.dim.label)
    runs_mx <- label_flattened_mx_grping.and.CIs(runs_mx, dict, row.dim.label, col.dim.label, CI=FALSE, num.runs=numZ)
    runs_mx <- percentages_flattened_mx(runs_mx, dict, CI, numZ)
    result <- remove.cols(runs_mx, zero_cat_cols)
    
  } else if ((CI==TRUE)&&(numZ>1)) {
    runs_mx <- label_flattened_mx_grping.and.CIs(runs_mx, dict, row.dim.label, col.dim.label, CI=CI, num.runs=numZ)
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

#' Collate frequencies and removes the zero category. 
#'
#'A new version of collator_freqs() that calls collator_mutiple_lists_mx2() instead of 
#' the 'non-2' version.  This means that percenatages will be averaged over runs instead 
#' of frequencies and the confidence intervals will be calculated on percentages rather 
#' than frequencies.  See documentation on mean_array_pctile_CIS2() more more details.  
#' Another difference between this function and the original collator_freqs() is that
#' this functiont takes cat.adjustments and binbreaks arguments. 
#' 
#' \itemize{
#'   \item Takes mean without confidence intervals using 
#'                                               \code{\link{collator_mutiple_lists_mx2}} 
#'   \item Removes the zero category
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
#' @param CI
#'  if TRUE and length(runs) > 1, lower and upper confidence intervals 
#'  are returned in additional columns
#' 
#' @param cat.adjustments
#' The cat.adjustments list containing the cat.adjustments for multiple variables.
#' Within the function the specific cat.adjustments for the variable of interest are 
#' extracted from the list in further embedded functions.  Either cat.adjustments or 
#' binbreaks are needed if frequencies of a continuous variable are being requested.  
#' 
#' @param binbreaks 
#' The binbreaks for the specific outcome variable.  Either binbreaks or cat.adjustments 
#' may be provided to the function.
#' 
#' @return
#' a matrix of collated result for each iteration
#' 
#' @export
collator_freqs_remove_zero_cat2 <- function(runs, dict, row.dim.label="Year", col.dim.label="", CI=FALSE, cat.adjustments=NULL, binbreaks=NULL) {
  runs_mx <- collator_mutiple_lists_mx2(runs=runs, CI=CI, dict=dict, cat.adjustments=cat.adjustments, binbreaks=binbreaks)
  grpby.tag <- attr(runs_mx, "meta")["grpby.tag"]
  
  zero_cat_cols <- identify_zero_category_cols(runs_mx)
  
  #the above code give incorrect categories with 0s for the outcome 
  #we only want to remove those columns that are 0 for the outcome, not also for the grouping variable
  #(which is what the above code does)
  if (!is.null(grpby.tag)) {
    if (!is.na(grpby.tag)) {
      if (grpby.tag!="") {
        zero_cat_cols <- identify_zero_category_cols_bygrp(runs_mx)
        if (length(zero_cat_cols)==0) {
          zero_cat_cols <- identify_zero_category_cols(runs_mx)
        }
      }
    }
  }
  
  numZ <- length(runs) #number of runs
  
  if ((CI==FALSE)|(numZ==1)) {
    runs_mx <- label_flattened_mx_grping.and.CIs(runs_mx, dict, row.dim.label, col.dim.label, CI=CI, num.runs=numZ)
    #runs_mx <- label_flattened_mx(runs_mx, dict, row.dim.label, col.dim.label)
    #runs_mx <- percentages_flattened_mx(runs_mx, dict, CI, numZ)
    result <- remove.cols(runs_mx, zero_cat_cols)*100
    
  } else if ((CI==TRUE)&&(numZ>1)) {
    runs_mx <- label_flattened_mx_grping.and.CIs(runs_mx, dict, row.dim.label, col.dim.label, CI=CI, num.runs=numZ)
    #runs_mx <- percentages_flattened_mx(runs_mx, dict, CI, numZ)
    resultCI <- remove.cols(runs_mx, zero_cat_cols)
    
    #label CI components
    run1_array <- as_array_list_mx(runs[[1]])
    numGroups <- dim(run1_array)[COL]
    colnames(resultCI) <- paste(colnames(resultCI), rep(c("Mean", "Lower", "Upper"), numGroups))
    names(dimnames(resultCI)) <- names(dimnames(resultCI))
    result <- resultCI*100
  }
  
  return(result)
}


collator_freqs_remove_zero_cat3 <- function(runs, dict, row.dim.label="Year", col.dim.label="", CI=FALSE, cat.adjustments=NULL, binbreaks=NULL) {
  #this function works when there are iterations at which a variable is not simulated (vector of NAs instead)
  
  #store colnames for later
  colnames <- colnames(runs[[1]][[1]])
  rownames <- rownames(runs[[1]][[1]])
  
  rownames.length <- unlist(lapply(runs[[1]], function(x) { length(rownames(x)) }))
  rownames.id <- which(rownames.length==max(rownames.length))[1]
  rownames <- rownames(runs[[1]][[rownames.id]])
  
  n.col <- max(unlist(lapply(runs[[1]], ncol)))
  n.row <- max(unlist(lapply(runs[[1]], nrow)))
  
  
  #identify for which iterations the variable was not simulated (will manifest as a matrix of 0s)
  #sum the matrix for each iteration
  numobs <- lapply(runs[[1]], function(x) {sum(unlist(x))})
  numobs <- unlist(numobs)
  #create index for iterations where sum=0 (all NAs for the variable at that iteration)
  id0 <- which(numobs==0)
  #index for those iterations where the sum is not 0
  idnot0 <- which(numobs!=0)
  #make the iterations where there was no values have a matrix like the first iteration where there 
  #were values (just as a workaround, later they will be changed back to NAs)
  for (k in 1:length(runs)) {
    for (j in id0) {
      ##if (is.null(runs[[k]][[idnot0[1]]])) {
      runs[[k]][[j]] <- matrix(1:(n.col*n.row), ncol=n.col, nrow=n.row) 
      #lose column and row names here, which are used in identify_zero_category_cols(), 
      #identify_zero_category_cols_bygrp() and label_flattened_mx_grping.and.CIs().
      #Put back on
      colnames(runs[[k]][[j]]) <- colnames
      if (!is.null(rownames)) {
        rownames(runs[[k]][[j]]) <- rownames
      } else {
        rownames(runs[[k]][[j]]) <- as.character(1:n.row) #not sure if this would always be the case - check
      }
      ##} else {
      ##runs[[k]][[j]] <- runs[[k]][[idnot0[1]]]
      ##}
    }
  }
  
  runs_mx <- collator_mutiple_lists_mx2(runs=runs, CI=CI, dict=dict, cat.adjustments=cat.adjustments, binbreaks=binbreaks)
  grpby.tag <- attr(runs_mx, "meta")["grpby.tag"]
  
  zero_cat_cols <- identify_zero_category_cols(runs_mx)
  
  #the above code gives incorrect categories with 0s for the outcome 
  #we only want to remove those columns that are 0 for the outcome, not also for the grouping variable
  #(which is what the above code does)
  if (!is.null(grpby.tag)) {
    if (!is.na(grpby.tag)) {
      if (grpby.tag!="") {
        #there is a subgroup
        zero_cat_cols <- identify_zero_category_cols_bygrp(runs_mx)
        if (length(zero_cat_cols)==0) {
          zero_cat_cols <- identify_zero_category_cols(runs_mx)
        }
      }
    }
  }
  
  numZ <- length(runs) #number of runs
  
  if ((CI==FALSE)|(numZ==1)) {
    runs_mx <- label_flattened_mx_grping.and.CIs(runs_mx, dict, row.dim.label, col.dim.label, CI=CI, num.runs=numZ)
    #runs_mx <- label_flattened_mx(runs_mx, dict, row.dim.label, col.dim.label)
    #runs_mx <- percentages_flattened_mx(runs_mx, dict, CI, numZ)
    result <- remove.cols(runs_mx, zero_cat_cols)*100
    
  } else if ((CI==TRUE)&&(numZ>1)) {
    runs_mx <- label_flattened_mx_grping.and.CIs(runs_mx, dict, row.dim.label, col.dim.label, CI=CI, num.runs=numZ)
    #runs_mx <- percentages_flattened_mx(runs_mx, dict, CI, numZ)
    resultCI <- remove.cols(runs_mx, zero_cat_cols)
    
    #label CI components
    run1_array <- as_array_list_mx(runs[[1]])
    numGroups <- dim(run1_array)[COL]
    colnames(resultCI) <- paste(colnames(resultCI), rep(c("Mean", "Lower", "Upper"), numGroups))
    names(dimnames(resultCI)) <- names(dimnames(resultCI))
    result <- resultCI*100
  }
  
  #change result at the end  - back to NAs for the first 3 years of conduct
  if (is.matrix(result)) {
    result[id0,] <- NA	
  } else {
    #assume result is a vector
    result[id0] <- NA
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
#' @return
#' NULL
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
#' @param ...
#'  Additional arguments passed to \code{\link{collator_list_mx}} 
#' 
#' @return
#'  a matrix with the averaged values of runs.
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
  #runs_mx <- collator_list_mx(runs)
  runs_mx <- collator_list_mx(runs, ...)
  
  grpby.tag <- attr(runs_mx, "meta")["grpby.tag"]
  if (!is.null(grpby.tag)) {
    if (!is.na(grpby.tag)) {
      if (grpby.tag=="") {
        grpby.tag<-NA
      }
    }
  }
  
  #if there are spaces in the groupby tag it means that this table is one grouped by
  #by the subgroup expression specific by the user
  spaces.in.grpby.tag <- str_locate_all(grpby.tag, " ")[[1]]
  if (length(spaces.in.grpby.tag)>0) {
    colnames(runs_mx) <- colnames(runs_mx)
    #fix labelling for means_by_subgroup later
    runs_mx_labelled <- runs_mx
  } else {
    runs_mx_labelled <- labelColumnCodes(runs_mx, dict, grpby.tag)
  }
  
  if (is.null(colnames(runs_mx_labelled))) colnames(runs_mx_labelled) <- "Mean"
  result <- runs_mx_labelled
  
  return(result)
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
#' @param ...
#'  Additional arguments passed to \code{\link{mean_array_z_pctile_CIs}} 
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

#'A new version of collator_mutiple_lists_mx() that calls 
#' mean_array_pctile_CIs2()) instead of then 'non-2' version.  This means that 
#' percenatages will be averaged over runs instead of frequencies and the confidence
#' intervals will be calculated on percentages rather than frequencies.  See documentation
#' on mean_array_pctile_CIS2() more more details.  
#' Another difference between this function and the original collator_mutiple_lists_mx() 
#' is that this function takes cat.adjustments, dict, and binbreaks arguments.  
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
#' @param cat.adjustments
#'  The cat.adjustments list containing the cat.adjustments for multiple variables.
#'  Within the function the specific cat.adjustments for the variable of interest are 
#'  extracted from the list in further embedded functions.
#' 
#' @param dict
#'  Dictionary object.
#' 
#' @param binbreaks
#' The binbreaks for the specific outcome variable.
#'   
#' @return
#'  a matrix with the averaged values of runs.
#'
#' @export 
#' @keywords internal
collator_mutiple_lists_mx2 <- function(runs, CI=TRUE, cat.adjustments=NULL, dict, binbreaks=NULL) {
  runs_array <- flatten_mxlists_to_array(runs)
  #reattach attributes (varname and grpby.tag)
  if (length(attr(runs, "meta"))>0) {
    attr(runs_array, "meta") <- attr(runs, "meta")
  } else if (length(attr(runs[[1]], "meta"))>0) {
    attr(runs_array, "meta") <- attr(runs[[1]], "meta")
  } else if (length(attr(runs[[1]][[1]], "meta"))>0) {
    attr(runs_array, "meta") <- attr(runs[[1]][[1]], "meta")
  } else {
    stop("Error in collator_mutiple_lists_mx2: lost varname attribute")
  }
  
  mean_array_z_pctile_CIs2(runs_array, CI=CI, cat.adjustments=cat.adjustments, dict=dict, binbreaks=binbreaks)
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



#' Identify and return the indices of columns that 
#' are for the zero category where grouping has been employed.  
#' Identifies the outcome variable's zero categories but not the grouping variable's zero categories.  
#' Zero category column names begin with a "0" or, for flatten column names, contain " 0". 
#' 
#' @param mx
#'  matrix with column names
#' @return
#'  vector of zero column positions
#'
#' @export 
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
#' @param CI
#'  if TRUE, lower and upper confidence intervals 
#'  are returned in additional columns
#' 
#' @param num.runs
#'  number of runs
#' 
#' @return 
#' a matrix of pecentages within groups
#' 
#' @seealso \code{\link{prop.table.mx.grped.rows}}
#' @export  
#' @examples
#' \dontrun{
#' mx.flattened <- structure(matrix(c(1,2,1,3,1,4,2,2,2,3,2,4,1,2,3,4), nrow=2, byrow = TRUE, dimnames=list(NULL, c("Female 1", "Female 2", "Female 3", "Female 4", "Male 1", "Male 2", "Male 3", "Male 4"))), meta=c(varname="disability_state", grpby.tag="sexLvl1"))
#' dict <- dict_demo
#' percentages_flattened_mx(mx.flattened, dict, num.runs = 1)
#'
#' mx.flattened <- structure(matrix(c(1,2,1,3,1,4,2,2,2,3,2,4,1,2,3,4), nrow=2, byrow = TRUE, dimnames=list(NULL, c("65-69 1", "65-69 2", "65-69  3", "65-69  4", "70-74  1", "70-74 2", "70-74 3", "70-74 4"))), meta=c(varname="disability_state", grpby.tag="age_grp_output"))
#' percentages_flattened_mx(mx.flattened, dict, num.runs = 1)
#' }
#' mx.flattened <- structure(matrix(c(1,2,1,3,1,4,2,2,2,3,2,4,1,2,3,4), nrow=2, byrow = TRUE, dimnames=list(NULL, c("Female 1", "Female 2", "Female 3", "Female 4", "Male 1", "Male 2", "Male 3", "Male 4"))), meta=c(varname="disability_state", grpby.tag="sex"))
#' dict <- dict_example
#' percentages_flattened_mx(mx.flattened, dict, num.runs = 1)

percentages_flattened_mx <- function(mx.flattened, dict, CI=FALSE, num.runs) {
  grpby.tag <- attr(mx.flattened, "meta")["grpby.tag"]
  
  groupnameprefixes <- if(is.null(grpby.tag) || is.na(grpby.tag)) NULL else names(dict$codings[[grpby.tag]])
  
  if(is.null(grpby.tag) || is.na(grpby.tag)) {
    groupnameprefixes <- NULL
  } else if (!is.null(grpby.tag) & !is.na(grpby.tag) & is.null(names(dict$codings[[grpby.tag]]))) {
    groupnameprefixes <- c("Not in subgroup", "In specified subgroup")
  } else if (!is.null(grpby.tag) & !is.na(grpby.tag) & !is.null(names(dict$codings[[grpby.tag]]))) {
    groupnameprefixes <- names(dict$codings[[grpby.tag]])
  } else {
    stop("Check percentages_flattened_mx()")
  }
  
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
#' @export
#' @examples
#' \dontrun{
#' x <- runs_mx
#' varname <- grpby.tag
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
  
  if (any(is.na(codings_indices))) {
    warning(
      paste("For varname", varname, "Expecting codings of",paste(catcodings, collapse=" "),"but got", paste(cnames_numeric[is.na(codings_indices)],collapse=" "))
    )
  }
  
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
#'  If grpby.tag is NULL, NA, or "", then the flattened code will be in the form "0", 
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
#' @return
#'  a flattened matrix with labels.
#' 
#' @export
#' @examples 
#' \dontrun{
#' mx.flattened <- runs_mx
#' mx.flattened <- structure(matrix(c(1,2,1,3,1,4,2,2,2,3,2,4), nrow=2, byrow = TRUE, dimnames=list(NULL, c("F 1", "F 2", "F 3", "M 1", "M 2", "M 3"))), meta=c(grpby.tag="sex", varname="disability_state"))
#' dict <- dict_demo
#' label_flattened_mx(mx.flattened, dict, row.dim.label="Year")
#' }
label_flattened_mx <- function(mx.flattened, dict, row.dim.label="", col.dim.label="") {
  varname <- attr(mx.flattened, "meta")["varname"]
  grpby.tag <- attr(mx.flattened, "meta")["grpby.tag"]
  
  if (!is.null(grpby.tag)) {
    if (!is.na(grpby.tag)) {
      if (grpby.tag=="") {
        grpby.tag <- NULL
      }
    }
  }
  
  #label
  colnames_original <- attr(mx.flattened, "colnames_original")
  cnames <- if (!is.null(colnames_original)) colnames_original else colnames(mx.flattened)
  
  colnames(mx.flattened) <- dict$cmatchFlattened(cnames, varname, grpby.tag)
  names(dimnames(mx.flattened)) <- c(row.dim.label,col.dim.label)
  
  means_suffix<-attr(mx.flattened, "means_suffix")
  
  if (!is.null(means_suffix)) colnames(mx.flattened)<-paste(colnames(mx.flattened), means_suffix)
  
  structure(mx.flattened, grpingNames=  attr(colnames(mx.flattened), "grpingNames"))
  
}

#' Extension of label_flattened_mx() to label a flattened matrix that contains confidence 
#' intervals.  Can handle grouping or no grouping. 
#' Labels flattened columns according to dictionary codings, 
#' and applies the specified row and col dimension label (if any).
#' 
#' @param mx.flattened
#'  a flattened matrix, ie: a matrix that has rows that contain
#'  groups of values.  
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
#' @param CI
#'  if TRUE and length(runs) > 1, lower and upper confidence intervals 
#'  are returned in additional columns
#' 
#' @param num.runs
#'  number of runs
#' 
#' @param binbreaks
#' The binbreaks for the specific outcome variable.
#' 
#' @return 
#'  a flattened matrix with labels and CI.
#' 
#' @export
label_flattened_mx_grping.and.CIs <- function(mx.flattened, dict, row.dim.label="", col.dim.label="", CI=TRUE, num.runs, binbreaks=NULL) {
  varname <- attr(mx.flattened, "meta")["varname"]
  grpby.tag <- attr(mx.flattened, "meta")["grpby.tag"]
  grpingNames <- attr(colnames(mx.flattened), "grpingNames")
  
  #e.g. colnames start off as:
  #  [1] "1 0 Mean"  "1 0 Lower" "1 0 Upper" "1 1 Mean"  "1 1 Lower" "1 1 Upper"
  #[7] "2 0 Mean"  "2 0 Lower" "2 0 Upper" "2 1 Mean"  "2 1 Lower" "2 1 Upper"
  #[13] "3 0 Mean"  "3 0 Lower" "3 0 Upper" "3 1 Mean"  "3 1 Lower" "3 1 Upper"
  
  col.names <- colnames(mx.flattened)
  
  #identify the position of the last space in each name
  #browser()
  last.space.output <- identify.position.last.space(col.names)
  pos.last.space.vec <- last.space.output[[1]]
  num.spaces <- last.space.output[[2]]
  
  if ((CI==TRUE)&(num.runs>1)) {
    # Need to remove the Mean, Lower, and Upper parts
    sub.col.names <- str_sub(col.names, 1, pos.last.space.vec-1)
  } else if ((CI==FALSE)|(num.runs==1)) {
    sub.col.names <- col.names
  }
  
  #if grpby.tag="" then convert it to NA
  if (!is.null(grpby.tag)) {
    if (!is.na(grpby.tag)) {
      if (grpby.tag=="") {
        grpby.tag <- NA
      }
    }
  }
  
  last.space.output <- identify.position.last.space(sub.col.names)
  num.spaces <- last.space.output[[2]]
  if ((num.spaces[1]==0)) {
    #no grouping
    if (!is.null(binbreaks)) {
      if (any(is.na(as.numeric(sub.col.names)))) {
        #names are already the character versions and the match is not needed
        un.ordered.names <- sub.col.names
      } else {
        #convert numeric codes to character names
        un.ordered.names <- dict$cmatchFlattened(sub.col.names, varname, grpby.tag)
      }
      ord <- match(un.ordered.names, names(binbreaks[-1]))
      ordered.names <- un.ordered.names[order(ord)]
      #reorder results
      mx.flattened <- mx.flattened[,order(ord)]
      colnames(mx.flattened) <- ordered.names
    } else {
      colnames(mx.flattened) <- dict$cmatchFlattened(sub.col.names, varname, grpby.tag)
    }
  } else if (num.spaces[1]>0) {
    #there is grouping
    
    #names of the variable of interest (not the grouping variable)
    #take last part of sub.col.names to match
    name.length <- str_length(sub.col.names)
    
    #identify the position of the last space in each name again 
    #(will have changed if removed Mean, Lower, Upper but will be same if didn't
    last.space.output <- identify.position.last.space(sub.col.names)
    pos.last.space.vec <- last.space.output[[1]]
    num.spaces <- last.space.output[[2]]
    
    #primary variable names
    simple.names <- str_sub(sub.col.names, pos.last.space.vec+1, name.length)
    if (any(is.na(as.numeric(simple.names)))) {
      #names are already the character versions and the match is not needed
      simple.names.words <- simple.names
    } else {
      #convert numeric codes to character names
      simple.names.words <- dict$cmatch(simple.names, varname)
    }
    
    if (!is.null(binbreaks)) {
      un.ordered.names <- simple.names.words
      ord <- match(un.ordered.names, names(binbreaks[-1]))
      ordered.names <- unique(un.ordered.names[order(ord)])
      if ((CI==TRUE)&(num.runs>1)) {
        num.grps <- (length(un.ordered.names)/length(ordered.names))/3
      } else if ((CI==FALSE)|(num.runs==1)) {
        num.grps <- length(un.ordered.names)/length(ordered.names)
      }
      ord2 <- NULL
      for (i in 1:num.grps) {
        ord2 <- c(ord2, ord[1:(length(ord)/num.grps)]+(i-1)*length(ordered.names))
      }
      ordered.names <- un.ordered.names[order(ord2)]
      
      #reorder results
      mx.flattened <- mx.flattened[,order(ord2)]
      
      #grping variable names
      grp.names <- str_sub(sub.col.names, 1, pos.last.space.vec-1)
      if (any(is.na(as.numeric(grp.names)))) {
        #names are already the character versions and the match is not needed
        grp.names.words <- grp.names
      } else {
        #no binbreaks
        #convert numeric codes to character names
        grp.names.words <- dict$cmatch(grp.names, grpby.tag)
      }
      
      final.names <- rep(NA, length(sub.col.names))
      for (i in 1:length(sub.col.names)) {
        final.names[i] <- paste(grp.names.words[i], ordered.names[i])
      }
      colnames(mx.flattened) <- final.names
      
    } else {
      colnames(mx.flattened) <- dict$cmatchFlattened(sub.col.names, varname, grpby.tag)
    }
    
    if ((sum(grepl("NA", colnames(mx.flattened)))==ncol(mx.flattened)) | (any(grepl("subgroup", colnames(mx.flattened))))) {
      #should be inside collate_all_run_results() and are collating results by a user specified subgroup
      
      #take first part of sub.col.names ('Not in subgroup' or 'In subgroup')
      grp.names.words <- str_sub(sub.col.names, 1, pos.last.space.vec-1)
      final.names <- rep(NA, length(sub.col.names))
      for (i in 1:length(sub.col.names)) {
        final.names[i] <- paste(grp.names.words[i], simple.names.words[i])
      }
      colnames(mx.flattened) <- final.names
    }
    
  }
  names(dimnames(mx.flattened)) <- c(row.dim.label,col.dim.label)
  
  result <- structure(mx.flattened, grpingNames=grpingNames, varname=varname)
  return(result)
}


#' Identify the position of the last space in each element in a character vector.
#' Used to identify the last space in the column names in label_flattened_mx_grping.and.CIs().
#' Returns a list of 2 elements.  
#' The first element, pos.last.space.vec, is a vector identifying the position of the last space 
#' in each element of the input vector. 
#' The second element, num.spaces, states the number of spaces in each element of the input 
#' vector.  
#' 
#' @param col.names
#'  a character vector.  
#' 
#' @return
#' A list with two vectors: a vector of the position of last space and 
#' a vector of number of spaces
#' 
#' @export
#' @examples
#' \dontrun{
#' col.names <- c(" ", "1 2", "1  2", "1 2 3")
#' identify.position.last.space(col.names)
#' }
identify.position.last.space <- function(col.names) {
  space.ids <- str_locate_all(col.names, " ")
  num.spaces <- unlist(lapply(space.ids, function(x) {nrow(x)}))
  pos.last.space <- rep(NA, length(num.spaces))
  if (sum(num.spaces)>0) {
    for (i in 1:length(num.spaces)) {
      pos.last.space[i] <- space.ids[[i]][num.spaces[i], 2]
    }
  }
  pos.last.space.vec <- pos.last.space
  result.list <- list(pos.last.space.vec=pos.last.space.vec, num.spaces=num.spaces)
  return(result.list)
}


#' Calculates the proportions within row groupings of a flattened matrix. 
#' 
#' @param mx.grped.rows
#'  a matrix with grped rows, ie: within each row there are groups of 
#'  columns that form a set. Proportions are then calculated within these groups.
#' 
#' @param groupnameprefixes
#'	 names of the groups. If NULL then no groups.
#' 
#' @param CI
#'  if TRUE, lower and upper confidence intervals 
#'  are returned in additional columns
#' 
#' @param num.runs
#'  number of runs
#' 
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
#' prop.table.mx.grped.rows(mx.grped.rows, groupnameprefixes, num.runs = 1)
prop.table.mx.grped.rows <- function (mx.grped.rows, groupnameprefixes, CI=FALSE, num.runs) {
  
  grpingNames <- attr(mx.grped.rows,"grpingNames")
  
  if (is.null(groupnameprefixes) || is.null(grpingNames)) {
    grpby <- rep(1, ncol(mx.grped.rows))
  } else {
    grpby <- match(grpingNames,groupnameprefixes)
    if (sum(is.na(grpby))==length(grpby)) {
      #grpby <- as.numeric(grpingNames)
      grpby <- grpingNames
    }
    assert(!is.na(grpby))	
  }
  # get proportions by grp
  mx.grped.rows.prop <- apply(mx.grped.rows, ROW, prop.table.grpby, grpby=grpby, CI=CI, num.runs=num.runs)
  
  mx.grped.rows.prop.t <- t(mx.grped.rows.prop)
  
  result <- structure(mx.grped.rows.prop.t, meta = attr(mx.grped.rows, "meta"), dimnames=dimnames(mx.grped.rows))
  
  return(result)
}

# run is a list of length = number of iterations
check.row.names <- function(run) {
  #for each iteration, check row.names exist, if not, set rownames to be the same as those that do exist
  rownames.list <- lapply(run, rownames)
  not.present <- lapply(rownames.list, function(x) { which(is.null(x)) })
  not.present.id <- as.numeric(names(unlist(not.present)))
  present.id <- (1:length(run))[-not.present.id]
  
  #for iterations where row names are not present, make the row names the same as the rows where they are present
  for (i in not.present.id) {
    rownames(run[[i]]) <- rownames(run[[present.id[1]]])
  }
  return(run)
}