#' Framework for executing multi-run dynamic micro-simulations including under different scenarios.
#' 
#' A simulation can consist of multiple modules (Simmodule).
#' 
#' Each simulation establishes (via a parameter file) the set of variables (the simframe) used 
#' by all modules during the simulation.
#' 
#' This variables, both categorical or continuous, can be modified before or during the simulation to
#' test different scenarios.
#' 
#' A simulation run consists of multiple iterations, and there may be multiple simulation runs.
#' 
#' Each Simmodule has a distinct set of outcomes (i.e: outputs). A run stat is a calculation performed at 
#' the end of the run for an outcome across all iterations. Example run stats include mean,
#' frequency, summary, and quantile. All that required is a R function that takes a matrix. 
#' 
#' Run stats are averaged across multiple runs to get a final simulation result.
#' 
#' Final results can be displayed in tables or graphs, and can be compared across scenarios.
#' 
#' @docType package
#' @name simar
#' @aliases simar package-simar
#' @import abind plyr xlsx proto
NULL

