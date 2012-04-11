#' simario is a framework for executing multi-run dynamic micro-simulations, including under different scenarios, and 
#' taking measurements of the results.
#' 
#' Each simulation establishes (via a parameter file) the set of variables (the simframe) used 
#' throughout the simulation. A simframe is a dataframe of multiple variables with observations (or values)
#' for each micro-unit. 
#' 
#' Variables in the simframe, both categorical or continuous, can be modified before or during the 
#' simulation to test different scenarios.
#' 
#' A simulation consists of one or more modules called Simmodules.
#' 
#' A Simmodule contains user supplied R code to transform the simframe. Typically, transformations will 
#' move variables for micro-units in the simframe through multiple iterations (or time steps). Transformations
#' occur based on transition probabilities, or generated via logistic, binomial, poission
#' negative binomial, or normal models.
#'   
#' At the end of each iteration the current value of each variable for all micro-units is stored as an outcome.
#' At the end of all iterations, the outcome list consists of a matrix for each variable giving its value
#' for each micro-unit at each iteration.
#' 
#' At the end of each run a series of run stats is calculated on outcomes. A run stat is essentially a function that takes
#' a matrix of outcomes across multiple iterations and produces an aggregate value. Example run stats include mean,
#' frequency, summary, and quantile. 
#' 
#' Run stats are averaged across multiple runs to get a final simulation result.
#' 
#' Final results can be displayed in tables or graphs, and can be compared across scenarios.
#' 
#' @docType package
#' @name simario
#' @aliases simario package-simario
#' @import abind plyr xlsx proto Hmisc
NULL

#' An example Dictionary proto object. 
#' 
#' Provides descriptions for
#' \itemize{
#'   \item age  
#'   \item age_grp 
#'   \item alive
#'   \item disability_state
#'   \item earnings  
#'   \item sex
#' }
#' 
#' Provides codings for
#' \itemize{
#'   \item age_grp 
#'   \item alive
#'   \item disability_state
#'   \item sex
#' }
#' 
#' @docType data
#' @keywords datasets
#' @name dict_example
#' @usage dict_example
#' @format A Dictionary proto object
NULL
