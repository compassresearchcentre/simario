# Chart utility functions
# 
# Author: oman002
###############################################################################



#' Draw a 2-series (base & scenario) side-by-side bar graph with 
#' error bars on the current device.
#' 
#' @param title
#'  chart title. Also used in navigator node name.
#' 
#' @param xlab
#'  x axis label. 
#' 
#' @param ylab
#'  y axis label
#' 
#' @param result.row.base
#'   the base result row, ie: a vector with values named Mean and Lower eg:
#' 
#'>  envs$`Scenario 1`$years1_5$run_results_collated$means$kids["Total",]
#'     Mean    Lower    Upper 
#' 10.99488 10.62256 11.36721 
#' 
#'  if there are no values named Mean, then it will be assumed that all values
#'  are Means and that Lower is 0.
#' 
#' @param result.row.scenario
#'  the scenario result row, formatted the same as result.row.base
#' 
#' @param scenario.name
#'  name of scenario. Used to great navigator node path and label series.
#'
#' @seealso result.as.means.and.errs
#' 
#' @return 
#'  NULL
#' 
#' @export
#' @examples
#'  
#' \dontrun{
#' varname <- "kids"
#' result.row.base <- env.base$modules$years1_5$run_results_collated$means_by_ethnicity[[varname]]["All Years",]
#' result.row.scenario <- envs$`Scenario 1`$modules$years1_5$run_results_collated$means_by_ethnicity[[varname]]["All Years",]
#' scenario.name <- "Scenario 1" 
#' title <- paste(dictLookup(varname), "all years")
#' xlab="Ethnicity"
#' ylab=paste("Mean")
#'
#' varname <- "cond"
#' result.row.base <- env.base$modules$years6_13$run_results_collated$means_by_SESBTH[[varname]]["All Years",]
#' result.row.scenario <- envs$`Scenario 1`$modules$years6_13$run_results_collated$means_by_SESBTH[[varname]]["All Years",]
#' scenario.name <- "Scenario 1" 
#' title <- paste(dictLookup(varname), "all years")
#' xlab="SES at Birth"
#' ylab=paste("Mean")
#'
#' varname <- "gptotvis"
#' result.row.base <- env.base$modules$years1_5$run_results_collated$histogram[[varname]]
#' result.row.scenario <- envs$`Scenario 1`$modules$years1_5$run_results_collated$histogram[[varname]]
#' scenario.name <- "Scenario 1" 
#' title <- paste(dictLookup(varname), "all years")
#' xlab="Value"
#' ylab="Freq"
#'
#' varname <- "cond"
#' result.row.base <- env.base$modules$years6_13$run_results_collated$histogram[[varname]]["All Years",]
#' result.row.scenario <- env.scenario$modules$years6_13$run_results_collated$histogram[[varname]]["All Years",]
#' scenario.name <- "Scenario 1" 
#' title <- paste(dictLookup(varname), "all years")
#' xlab="Value"
#' ylab="Freq"
#'   
#' chart.2series.bar.err(title, xlab, ylab, result.row.base, result.row.scenario, scenario.name)
#' }
chart.2series.bar.err <- function(title, xlab, ylab, result.row.base, result.row.scenario, scenario.name) {
  
  # remove NAs
  result.row.base <- na.omit(result.row.base)
  result.row.scenario <- na.omit(result.row.scenario)
  
  me.base <- result.as.means.and.errs(result.row.base)
  me.scenario <- result.as.means.and.errs(result.row.scenario)
  
  me <- merge_list_mx.by.rows(me.base, me.scenario)
  
  #replace NAs with zeros
  me <- lapply(me, function(x) {
    #x <- me$means
    x[is.na(x)] <- 0; x
  })
  
  chart.bar.err(me$means, me$errs, 
                xlab=xlab, 
                ylab=ylab, main=title,
                col=c("cornflowerblue", "chocolate1"),
                legend.text=c("Base", scenario.name))
}

#' Close over parameters to chart.2series.bar.err 
#'
#' @param title
#'  chart title. Also used in navigator node name.
#' 
#' @param xlab
#'  x axis label. 
#' 
#' @param ylab
#'  y axis label
#' 
#' @param result.row.base
#'   the base result row, ie: a vector with values named Mean and Lower eg:
#' 
#'>  envs$`Scenario 1`$years1_5$run_results_collated$means$kids["Total",]
#'     Mean    Lower    Upper 
#' 10.99488 10.62256 11.36721 
#' 
#'  if there are no values named Mean, then it will be assumed that all values
#'  are Means and that Lower is 0.
#' 
#' @param result.row.scenario
#'  the scenario result row, formatted the same as result.row.base
#' 
#' @param scenario.name
#'  name of scenario. Used to great navigator node path and label series.
#'
#' @seealso result.as.means.and.errs
#' 
#' @return 
#'  NULL
#' 
#' @export 
chart.2series.bar.err.closure <- function(title, xlab, ylab, result.row.base, result.row.scenario, scenario.name) {
  function() {
    chart.2series.bar.err(title, xlab, ylab, result.row.base, result.row.scenario, scenario.name) 
  }
}

#' Draw a side-by-side bar plot with error bars and a legend to the right
#' of the plot on the current device.
#' 
#' @param y
#'  vector/matrix of y values. Each row of matrix is a separate series.
#' @param y.err
#'  amount above and below y to draw error bar
#' @param legend.text
#'  vector of text to appear in legend
#' @param col
#'   a vector of colors for the bars or bar components. By default, grey is used if height is a vector, 
#'   and a gamma-corrected grey palette if height is a matrix.
#' @param ...
#'  additional params to pass to barplot
#' 
#' @return 
#' NULL
#' 
#' @export
#' @examples
#' \dontrun{
#' y <- me$means
#' y.err <- me$errs
#' col <- c("cornflowerblue", "chocolate1") 
#' legend.text <- c("Base", "Scenario 1")
#' chart.bar.err(y, y.err, legend.text, col)
#' }
chart.bar.err <- function(y, y.err, legend.text, col, ...) {
  # default margins
  #mar.default <- c(5, 4, 4, 2) + 0.1
  #par(mar.default) #set default
  
  # xpd=T: allow drawing outside of plot (ie. in margin)
  # par()$mar+c(0,0,0,6): extend current right margin of plot by 4 lines
  # mar.default + c(0,0,0,6): default margin + 4 lines extended to the right 
  #par(xpd=T, mar=mar.default+c(0,0,0,6))
  
  # barx <- barplot(y, beside=TRUE, ylim=c(0,max(y + y.err)), names.arg=names(y), axis.lty=1, col=col)
  barx <- barplot(y, beside=TRUE, ylim=c(0,max(y + y.err)*1.25), 
                  names.arg=names(y), axis.lty=1, col=col, ...)
  legend(x=mean(barx), xjust = 0.5, y=max(y+y.err) *1.25, legend=legend.text, fill=col)
  
  error.bar(barx, y, y.err)
  
}


#' Draws an error bar on the current device.
#' 
#' @param x
#'   vector/matrix of x locations to draw error bar, usually mid point of any bar (e.g. that returned from a barplot)
#' @param y
#'   vector/matrix of y locations of middle of error bar, usually the top of a data bar
#' @param upper
#'   vector/matrix of amounts above y to draw, eg: 
#' @param lower
#'   vector/matrix of amounts below y to draw, defaults to upper.
#' @param length
#'   width of error bar whisker, defaults to 0.1
#' @param ...
#'   any other parameters to arrows
#' 
#' @return 
#' NULL
#' @export
#' @examples
#' \dontrun{
#' y <- rnorm(500, mean=1)
#' y <- matrix(y,100,5)
#' y.means <- apply(y,2,mean)
#' y.sd <- apply(y,2,sd)
#' y.err <- 1.96*y.sd/10
#' barx <- barplot(y.means, names.arg=1:5,ylim=c(0,1.5), col="blue", axis.lty=1, xlab="Replicates", ylab="Value (arbitrary units)")
#' error.bar(barx, y.means, y.err)
#' 
#' error.bar(barx, y.means, y.err)
#' x = barx; y = y.means
#' upper = y.err; lower=upper}
error.bar <- function(x, y, upper, lower=pmin(y,upper), length=0.1,...){
  if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper))
    stop("vectors must be same length")
  
  #don't draw when no arrow, else get warning
  ind.nonzero <- abs(upper) + abs(lower) > 0
  
  arrows(x[ind.nonzero], (y+upper)[ind.nonzero], x[ind.nonzero], (y-lower)[ind.nonzero], 
         angle=90, code=3, length=length, ...)
}


