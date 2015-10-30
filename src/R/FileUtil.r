# TODO: Add comment
# 
# Author: oman002
###############################################################################

library(xlsx)


#' Returns the file extension, i.e: everything after the final dot.
#' 
#' @param x
#'  chacter vector
#' 
#' @return 
#'  file extension, eg: "csv"
#' 
#' @export
#' @examples
#' x <- "data.txt"
#' x <- "d:/workspace/data old.csv"
#' x <- "d:/workspace/data.new.xlsx"
#' file_extension(x)
file_extension <- function(x) {
	matched <- regexpr("\\.([^\\.]+)$", x)
	substr(x, start=matched+1, stop=matched+attr(matched, "match.length")-1) 
}


#' Reads a CSV file.
#' 
#' @param filedir
#'  file directory, with or without trailing slash
#' 
#' @param filename
#'  file name
#' 
#' @param stringsAsFactors
#'  logical: should character vectors be converted to factors?
#' 
#' @param ...
#'  additional parameters to read.csv
#' 
#' @return 
#'  a data frame
#' 
#' @export
#' @examples
#' \dontrun{
#' filedir <- "D:/workspace.sim/simario/demo/data/base"
#' people <<- read_csv(base_dir, "Base_file_(people).csv")
#' }
read_csv <- function (filedir, filename, stringsAsFactors = FALSE, ...) {
	filedir <- add_trailing_slash(filedir)		
	read.csv(paste(filedir, filename, sep=""), stringsAsFactors = stringsAsFactors, ...)
}


#' Read a file and return a dataframe.
#' 
#' @param filedir
#'  file directory, with or without trailing slash
#' 
#' @param filename
#'  file name 
#' 
#' @param filetype
#'  "csv", "xls", or "xlsx". Defaults to the extension of filename
#' 
#' @param stringsAsFactors
#'  logical: should character vectors be converted to factors?
#' 
#' @param ...
#'  additional parameters to read_csv or read.xlsx2
#' 
#' @return
#'  a data frame
#' 
#' @export
#' @examples 
#' \dontrun{
#' filedir <- "D:/workspace.sim/simario/demo"
#' filename <- "Disability state transition probabilities.xlsx"
#' filetype = file_extension(filename)
#' read_file(filedir, filename, filetype)
#' }
read_file <- function (filedir, filename, filetype = file_extension(filename), stringsAsFactors = FALSE, ...) {
	switch(filetype,
		csv = read_csv(filedir, filename, stringsAsFactors = stringsAsFactors, ...),
		xls = readXLSSheet1(filedir, filename, stringsAsFactors = stringsAsFactors, ...),
		xlsx = readXLSSheet1(filedir, filename, stringsAsFactors = stringsAsFactors, ...))
}


#' Returns first sheet of XLS as dataframe
#' 
#' @param filedir
#'  file directory, with or without trailing slash
#' 
#' @param filename
#'  file name
#' 
#' @param stringsAsFactors
#'  logical: should character vectors be converted to factors?
#' 
#' @param ...
#'  additional parameters to read.xlsx2
#' 
#' @return 
#'  a data frame
#' 
#' @export
readXLSSheet1 <- function (filedir, filename, stringsAsFactors = FALSE, ...) {
	filedir <- add_trailing_slash(filedir)		
	oldOpt <- options(stringsAsFactors = stringsAsFactors)
	on.exit(options(oldOpt))
	read.xlsx2(paste(filedir, filename, sep=""), sheetIndex = 1, ...)
} 
