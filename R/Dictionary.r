# Dictionary object.
# This object holds:
#  * a mapping of variable names to descriptions
#  * codings (i.e: category names) for categorical variables
# 
# Author: oman002
###############################################################################

library(proto)
library(xlsx)

Dictionary <- proto(expr = {
	
	#' Vector of variable desciptions.
	#' names(dict) are the variable names.
	vardesc <- NULL
	
	#' List of codings.
	#' names(codings) are the variable names.
	#' Each element is a vector. The vector values are the category values (eg: 1, 2)
	#' and the names of the vector are the category descriptions. 
	#' Each element also contains the varname attribute.
	#' 
	#' Example of an element:
	#'  
	#' Professional     Clerical Semi-skilled 
	#'        1            2            3 
	#' attr(,"varname")
	#' [1] "SESBTH"
	codings <- list()
	
	#' Return the category names for a vector of coded values.
	#' If no category names, returns the list as is.
	#'
	#' @param .
	#'  this 
	#' @param x
	#'  coded values
	#' @param varname
	#'  name of the category of x, or NULL
	#' 
	#' @param
	#'  x, is there are no codings for varname, else a vector of category names
	#'  corresponding to the values in x
	#' 
	#' @examples
	#'  x <- c(0,1,0,1,0,1) ;  varname <- z1singleLvl1
	#'  x <- character(0) ; varname <- NULL 
	#'  codings$cmatch(x, varname)
	cmatch <- function (., x, varname) {
		if (is.null(varname)) return(x)
		
		xcodings <- .$codings[[varname]]
		if (is.null(xcodings)) return(x)
		
		# match x in catcodings
		codings.indices <- match(x, xcodings)
		names(xcodings)[codings.indices]
		
	}
	
	#' Returns the category names for the vector of flattened codes.
	#'
	#' @param .
	#'  this 
	#' @param varname
	#'  identifies the varname coding
	#' @param grpby.tag
	#'  identifies the grping coding, or NULL if no grouping coding.
	#' @param x.flat
	#'  a vector of flattened codes. A flattened code is in the form "0 1",
	#'  where the first value is a grping code and the second a varname code.
	#'  If grpby.tag is NULL or NA, then the flattened code will be in the form "0", 
	#'  i.e: no grping codes only varname codes.
	#' 
	#' @examples
	#'  x.flat = c("1 0", "1 1", "2 0", "2 1", "3 0", "3 1"); varname = "z1singleLvl1"; grpby.tag = "r1stchildethn" 
	#'  x.flat = c("0","1") ; varname = "z1singleLvl1"; grpby.tag = NULL
	#'  x.flat = c("0", "1", "3", "5") ; varname = "gptotvis" ; grpby.tag = NULL
	#'  x.flat = c("1 0", "2 1", "3 3", "1 0", "3 5") ; varname = "gptotvis" ; grpby.tag = "r1stchildethn"
	#'  . <- dict.MELC
	#'  .$cmatchFlattened(x.flat, varname, grpby.tag)
	cmatchFlattened <- function (., x.flat, varname, grpby.tag) {
		# add category and group by coding names, if any
		grping.match <- regexpr(".*\\s", x.flat)
		
		if (is.null(grpby.tag) || is.na(grpby.tag)) {
			#no grping codes, only varname codes
			.$cmatch(x.flat, varname)	
		} else {
			
			grping <- trim(regmatches(x.flat, grping.match))
			grpingNames <- .$cmatch(grping, grpby.tag)
			
			cats.match <- regexpr("\\s.*", x.flat)
			cats <- trim(regmatches(x.flat, cats.match))
			catsNames <- .$cmatch(cats, varname)
			
			paste(grpingNames, catsNames)
		}
		
	}
	
	#' Returns the names of the codings for the supplied variables.
	#' NB: Requires codings variable to have been set in global environment.
	#' 
	#' @param .
	#'  this
	#' @param vars
	#'  vector of variable names
	#' 
	#' @return 
	#'  list of coding names for the variables supplied
	#' 
	#' @examples
	#' 
	#' vars <- c("z1msmokeLvl1", "SESBTH") 
	#' vars <- names(runs.mean.freq)
	#' . <- codings 
	#' codings$cnamesLookup(vars)
	cnamesLookup <- function(., vars) {
		cn <- .$codings[vars]
		lapply(cn, names)
	}
	
	#' Lookup description of variable x in the dictionary
	#' first determines the name of variable x, then does the lookup
	#' 
	#' @examples
	#' . <- dict.MELC
	#' x <- env.base$modules$years1_5$results$means$all$kids
	#' x <- env.base$modules$years1_5$results$means$all.by.ethnicity$kids
	#' .$dlookup(x)
	#' .$dlookup("burt")
	#' .$dlookup(c(1,2))
	#' .$dlookup(freqSingle)
	#' .$dlookup("single")
	#' .$dlookup("don't exist")
	#' .$dlookup(runs.mean.freq$base$o.chres)
	dlookup <- function(., x) {
		
		name <- c()
		grouping <- c()
		set <- c()
		weighting <- c()
		meta <- attr(x, "meta")
		
		#get the variable name
		if (!is.null(meta)) {
			#use the meta attribute
			name <- meta["varname"]
			if (!is.na(meta["grouping"])) grouping <- paste(" by ", meta["grouping"], sep="")
			if (!is.na(meta["grpby.tag"])) grouping <- paste(" by ", .$dlookup(meta["grpby.tag"]), sep="")
			if (!is.na(meta["set"])) set <- paste(" (", meta["set"], ")", sep="")
			if (!is.na(meta["weighting"])) weighting <- meta["weighting"]
			
		} 
		
		# if no meta, or no name from meta
		if (is.null(name) || is.na(name)) {
			
			if (class(x) %in% c("matrix", "array", "table") && !is.null(names(dimnames(x)))) {
				#get name from names of dimensions
				namesdim <- names(dimnames(x))
				namesdim <- stripEmpty(namesdim) #remove NAs and empty strings
				
				# get last dim for name
				name <- namesdim[length(namesdim)]
				
			} else if (class(x) == "character") {
				#get name from first position of char vector
				name <- x[1]
				
			} else {
				#fail
				firstParamName <- as.character(sys.call())[2]
				stop(gettextf("cannot determine varname from %s: no meta or names", firstParamName))
			}
		}
		
		#lookup name in dictionary
		if (!name %in% names(.$vardesc)) {
			stop(gettextf("'%s' does not exist in the data dictionary", name))
		}
		
		desc <- .$vardesc[[name]]
		
		if (is.null(desc)) {
			stop(gettextf("variable named '%s' does not exist in data dictionary", name))
			name <- dname
		}
		
		#add grouping, weighting, and set descriptions (if any)
		weightdesc <- ifelse(weighting == "weightBase", "", " scenario")
		paste(desc, grouping, weightdesc, set, sep="")
	}

	#' Order the list by the results returned applying
	#' dlookup to the list's elements
	#' 
	#' @param ...
	#'  objects to perform dlookup on
	#' @seealso \code{\link{dlookup}}
	#' 
	#' @return 
	#'  ... in order according to dlookup
	#' @examples
	#'  . <- dict.MELC
	#'  xlist <- means$all.by.ethnicity
	#'  .$order_by_dlookup(xlist)
	order_by_dlookup <- function (., ...) {
		xlist <- c(...)
		ordering <- sort.list(sapply(xlist, .$dlookup))
		xlist[ordering]
	}
	
	#' Load codings XLS file and return named list of codings.
	#' 
	#' @param .
	#'  this
	#' @param filedir
	#'  file directory, ending with "/", eg: "d:/workspace/"
	#' @param dict_filename
	#'  file name for codings, eg: "myfile.xls".
	#' @param codings_filename
 	#'  file name for dictionary, eg: "myfile.xls". Can be same file as 
	#'  codings_filename
	#' 
	#' @seealso Uses \code{\link{loadCodingsXLS}, \link{loadDictionaryXLS}} 
	#' 
	#' @examples
	#' filedir = "D:/workspace.sim/MELC/CHDS/base/"
	#' filename = "CHDS data dictionary.xlsx"
	#' 
	#' codings <- Dictionary$new_from_XLS(filedir, filename, filename)
	new_from_XLS <- function (., filedir, dict_filename, codings_filename) {
		vardesc <- loadDictionaryXLS(filedir, dict_filename)
		codings <- loadCodingsXLS(filedir, codings_filename)
		
		# return new object
		proto(.,
			 vardesc = vardesc,
			 codings = codings)
	}

	#' Load codings XLS file and return named list of codings.
	#' 
	#' @param .
	#'  this
	#' @param filedir
	#'  file directory, ending with "/", eg: "d:/workspace/"
	#' @param filename
	#'  file name, eg: "myfile.xls". 
	#' The codings file must contain 2 columns:
	#'  Varname = the name of the categorical variable
	#'  Codings_Expr = an expression which generates the codings, eg:
	#'  				c("Other"=1, "Pacific"=2, "Maori"=3)
	#'  			   NB: Codings must be specified in numeric order
	#' 
	#' @examples
	#' filedir = "D:/workspace.sim/MELC/CHDS/base/"
	#' filename = "CHDS data dictionary.xlsx"
	#' 
	#' codings <- loadCodingsXLS(filedir, filename)
	loadCodingsXLS <- function (filedir, filename) {
		codings.df <- readXLSSheet1(filedir, filename)
		
		#remove empty variables, 
		#these are blank lines at the end of the file or
		#variables without any coding expr
		codings.df  <- subset(codings.df, !(Varname==""))
		codings.df  <- subset(codings.df, !(Codings_Expr==""))
		
		#evaluate "Codings_Expr" column in global environment
		codings <- eval.list(codings.df$Codings_Expr)
		
		names(codings) <- codings.df$Varname
		
		#add "varname" attribute for use with wtdmeancols.lbl function
		codings <- mapply(function(coding, name) {
					structure(coding, varname=name)
				}, codings, names(codings))
		
		codings
	}
	
	#' Load dictionary xls file and return dictionary vector.
	#'
	#' @param filedir
	#'  file directory, ending with "/", eg: "d:/workspace/"
	#' @param filename
	#'  file name, eg: "myfile.xls". Must contain a column called
	#'  "Description" and a column called "Varname".
	#' 
	#' @return 
	#'  a vector with values = Description column and names = Varname column
	#'  of xls file.
	#' 
	#' @examples
	#' filedir = "D:/workspace.sim/MELC/CHDS/base/"
	#' filename = "CHDS data dictionary.xlsx"
	#' 
	#' loadDictionaryXLS(filedir, filename)
	loadDictionaryXLS <- function (filedir, filename) {
		dict_frame <- readXLSSheet1(filedir, filename)
		
		#remove empty variables, generally these are blank lines
		#at the end of the file
		dict_frame <- subset(dict_frame, !(Varname==""))
		
		dict <- dict_frame$Description
		names(dict) <- dict_frame$Varname
		
		dict
	}
	
})

cat("Loaded Dictionary\n")

