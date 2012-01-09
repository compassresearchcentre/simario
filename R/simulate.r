# Simulation support functions.
#
# Requires support.r
# 
# Author: Oliver Mannion
###############################################################################


#' Checks that the variables used in all of the models
#' exist in simvalues, if not generate stop
#' 
#' @examples
#' simframe <- simframe.start
#' checkModelVars(models, simframe)
checkModelVars <- function (models, simframe) {
	# try a predict on each model
	predtest <- lapply(models, function (model) try({
							#model <- models$gptotvis
							#predict(model, simframe)
							model.terms <- attr(delete.response(terms(model)), "variables")
							eval(model.terms, simframe)
						}, silent = TRUE))
	
	# gather errors
	tryerrs <- tryerrorMsgs(predtest) 
	
	if (length(tryerrs) > 0) {
		# concate errors together with model name
		msg <- paste("Model ",names(tryerrs),":",sub(".*:", "", tryerrs ), sep="",collapse="")
		stop(msg)
	}
}

#' Checks that the outcome variables have a source variable in simframe.
#'  
#' @param outcomes list of outcome variables with a varname attribute.
#' @param simframe simframe to test for presence of varname
#' @return nothing if successful, otherwise errors
#' @examples
#' checkOutcomeVars(createMELCfoutcomes(children),simframe) #outcomes <- createMELCfoutcomes(children) 
checkOutcomeVars <- function(outcomes, simframe) {
	#check varname specified on outcome var exists in simframe
	srcvar <- unlist(lapply(outcomes, attr, "varname"))
	nonexistent_srcvar <- !(srcvar %in% names(simframe))
	if (any(nonexistent_srcvar)) {
		stop(gettextf("src variable(s) %s: not in simframe", paste(srcvar[nonexistent_srcvar],collapse=", ")))
	}
}

#' Return the category names for a vector of coded values.
#' If no category names, returns the list as is.
#' 
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
#'  codingsMatch(x, varname)
codingsMatch <- function (x, varname) {
	if (is.null(varname)) return(x)
	
	xcodings <- codings[[varname]]
	if (is.null(xcodings)) return(x)
	
	# match x in catcodings
	codings.indices <- match(x, xcodings)
	names(xcodings)[codings.indices]
	
}

#' Returns the category names for the vector of flattened codes.
#'  
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
#'  codingMatchFlattened(x.flat, varname, grpby.tag)
codingMatchFlattened <- function (x.flat, varname, grpby.tag) {
	# add category and group by coding names, if any
	grping.match <- regexpr(".*\\s", x.flat)
	
	if (is.null(grpby.tag) || is.na(grpby.tag)) {
		#no grping codes, only varname codes
		codingsMatch(x.flat, varname)	
	} else {
		
		grping <- trim(regmatches(x.flat, grping.match))
		grpingNames <- codingsMatch(grping, grpby.tag)
		
		cats.match <- regexpr("\\s.*", x.flat)
		cats <- trim(regmatches(x.flat, cats.match))
		catsNames <- codingsMatch(cats, varname)
		
		paste(grpingNames, catsNames)
	}
	
}


#' Returns the names of the codings for the supplied variables.
#' NB: Requires codings variable to have been set in global environment.
#' 
#' @param vars
#'  vector of variable names
#' 
#' @return 
#'  list of coding names for the variables supplied
#' 
#' @examples
#' 
#' vars <- c("msmoke", "fsmoke") 
#' vars <- names(runs.mean.freq)
#' 
#' codingsNamesLookup(vars)
codingsNamesLookup <- function(vars) {
	cn <- codings[vars]
	lapply(cn, names)
}

#' Create a generalized linear model
#' from a model dataframe that contains the model variables
#' and their coefficients
#' NB: the variable "_Alpha" will not be added to the model formula
#'     but will be added as the model component m.glm$alpha 
#' NB: the variable "_sd" will not be added to the model formula
#'     but will be added as the model component m.glm$sd 
#' 
#' "Variable" = variable name. can be an expression like MAGE*MAGE
#' "ClassVal0" = variable level. If present is appended to the variable name, eg: MAGELvl1
#' "Estimate" = variable coeffient
#' 
#' @examples 
#' 
#' m.glm <- createGLM(model_msmoke) 
createGLM <- function (modeldf) {
	
	#extract model components from dataframe
	varnames <- modeldf$Variable
	varlevels <- as.numeric(modeldf$ClassVal0)
	mcoef <- as.numeric(modeldf$Estimate)
	
	alphaIndex <- which(varnames == "_Alpha")
	if (length(alphaIndex) > 0)
	{
		alpha <- mcoef[alphaIndex]
		varnames <- varnames[-alphaIndex]
		varlevels <- varlevels[-alphaIndex]
		mcoef <- mcoef[-alphaIndex]
	}	

	sdIndex <- which(varnames == "_sd")
	if (length(sdIndex) > 0)
	{
		sd <- mcoef[sdIndex]
		varnames <- varnames[-sdIndex]
		varlevels <- varlevels[-sdIndex]
		mcoef <- mcoef[-sdIndex]
	}	
	
	#add "LvlX" where appropriate to the var name
	vars <- paste(varnames,
			ifelse(is.na(varlevels) | varlevels=="","",paste("Lvl",varlevels,sep="")),
			sep="")
	
	names(mcoef) <- vars
	
	#construct formula from vars (except first var, ie: Intercept) and get terms
	#wrap varnames in I() so that terms like MAGE*MAGE will work as expected
	#when submitted to model.frame and model.matrix during prediction
	fmla <- as.formula(paste("y ~ ", paste("I(",vars[-1],")", sep="", collapse= "+")))
	mterms <- terms(fmla)
	
	#create glm
	m.glm <- list(coefficients=mcoef, 
			#family=binomial(), 
			terms=mterms, 
			qr=list(pivot=seq_len(length(mcoef))), 
			rank=length(mcoef))
	
	if (exists("alpha")) {
		m.glm$alpha <- alpha
	}

	if (exists("sd")) {
		m.glm$sd <- sd
	}
	
	class(m.glm) <- c("glm","lm") 
	m.glm
}

#' Create a list of empty outcome matrix variables for a specified outcome set.
#' The variables to create are those from the outcome set specified by "outcomeSetName". 
#' 
#' @param simframe simulation frame
#' @param outcomesetName name of the outcomeset to create variables for
#' @param iterations either a vector of names for each iteration,
#'  eg: ("Year 5", "Year 6" ...) or a scalar specifying the number 
#'  of iterations.
#' 
#' @return 
#' 
#' A dataframe of outcome variables. Each output variable is a matrix 
#' of simvalues by iterations. Each matrix is assigned the "varname" 
#' attribute that names the source variable in the simframe that will be used 
#' during simulation to fill the matrix. This source variable is the
#' same name as the name outcome matrix.
#' 
#' @examples
#' simframe <- simframe.start
#' simframe <- env.base$simframe
#' outcomeSetName <- "years1_5"
#' outcomeSetName <- "years6_13"
#' iterations <- 8
#' iterations <- c(6:13)
#' outcomes <- createOutcomeMatrices(simframe, outcomeSetName, iterations)
createOutcomeMatrices <- function (simframe, outcomeSetName, iterations) {
	setVars <- getOutcomeVars(simframe, select_outcome_set=outcomeSetName, sorted=TRUE)
	
	outcomes <- lapply(setVars, function (var) 
				createOutputMatrix(var, length(simframe[[var]]), 
						iterations) )
	
	#convert list to dataframe 
	as.data.frame.list.as.is(outcomes, row.names(simframe))
}

#' Create a matrix of NA with specified col/row names/lengths.
#' 
#' @param cols columns names, or a numeric scalar for the number of cols
#' @param rows row names, or a numeric scalar for the number of rows
#' @param simvarname 
#' 		simframe source var. stored in the "simvar" attribute.
#'      of the matrix. This is the name of the variable in the
#' 		simframe that will be used to fill this matrix during simulation
#' @return
#' a matrix with the "varname" attribute set to simvarname
#' 
#' @examples 
#' rows <- length(children$z1msmoke1)
#' cols <- 5
#' simvarname <- "z1msmokeLvl1"
#' createOutputMatrix(simvarname, nrows, ncols)
createOutputMatrix <- function (simvarname, rows, cols) {
	structure(namedMatrix(rows, cols), varname = simvarname)
}

#' Creates a set of run outputs. 
#' 
#' @param freqvars
#' 			frequency variable names, or NULL
#' @param cfreqvars
#' 			continuous frequency variable names, or NULL
#' @param meanvars
#' 			mean variable names, or NULL
#' @param freqs.args
#' 			frequency variable args, or NULL
#' @param means.args
#' 			mean variable args, or NULL
#' @return
#'  	list(freq = freq, cfreq = cfreq, mean.sets = mean.sets, mean.grouped = mean.grouped)
#' 		each element of the list is a list of empty catvar or convar elements, either grouped
#' 		into sets (mean.grouped and mean.sets) or listed straight.
#' 
#' @examples 
#'
#' catvars <- NULL
#' convars <- NULL
#' means.args <- NULL
#' mean.grouped.spec <- NULL
#'  
#' catvars <- c("msmoke", "fsmoke", "single", "kids", "householdsize", "welfare", "mhrswrk", "fhrswrk", "accom", "homeown", "bedrooms",	"chpar", "chres")
#' convars <- c("gptotvis", "hadmtot", "houtptot", "gpresp", "gpmorb", "gpprev")
#' 
#' freqs.args <- list( by.ethnicity = list(grpbycoding=codings$r1stchildethn) )
#' means.args <- list(	all = list(), males = list(logiset=childsets$males),	females = list(logiset=childsets$females),pacific = list(logiset=childsets$pacific),	maori = list(logiset=childsets$maori))
#' 
#' freqvars <- catvars
#' runs <- createRunOutputs(catvars, convars, means.args, mean.grouped.spec)
createRunOutputs <- function(freqvars, cfreqvars, meanvars, freqs.args, means.args) {
	# Frequency tables for categorical variables
	freqslist <- namedList(freqvars)
	
	# Frequency tables for continuous variables
	cfreqs <- namedList(cfreqvars)
	
	# Mean tables for continuous variables
	meanslist <- namedList(meanvars)
	
	freqs <- lapply(freqs.args, function(x) freqslist)
	attr(freqs, "args.list") <- freqs.args
	
	means <- lapply(means.args, function(x) meanslist)
	attr(means, "args.list") <- means.args
	
	list(freqs = freqs, 
			cfreqs = cfreqs, 
			means = means, 
			summaries = meanslist,
			quantiles = meanslist
	)
	
}


#' Create a matrix of NA with specified col/row names/lengths.
#' 
#' @param cols columns names, or a numeric scalar for the number of cols
#' @param rows row names, or a numeric scalar for the number of rows
#' @param simvarname 
#' 		simframe source var. stored in the "simvar" attribute.
#'      of the matrix. This is the name of the variable in the
#' 		simframe that will be used to fill this matrix during simulation
#' @return
#' a matrix with the "varname" attribute set to simvarname
#' 
#' @examples 
#' rows <- length(children$z1msmoke1)
#' cols <- 5
#' simvarname <- "z1msmokeLvl1"
#' createOutputMatrix(simvarname, nrows, ncols)
createOutputMatrix <- function (simvarname, rows, cols) {
	structure(namedMatrix(rows, cols), varname = simvarname)
}

#' Lookup description of variable x in the dictionary
#' first determines the name of variable x, then does the lookup
#' 
#' @examples
#' x <- env.base$years1_5$results$freqs$all$z1msmokeLvl1
#' x <- env.base$years1_5$results$freqs$all.by.ethnicity$z1msmokeLvl1
#' dictLookup(x)
#' dictLookup("burt")
#' dictLookup(c(1,2))
#' dictLookup(freqSingle)
#' dictLookup("single")
#' dictLookup("don't exist")
#' dictLookup(runs.mean.freq$base$o.chres)
dictLookup <- function(x) {
	
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
		if (!is.na(meta["grpby.tag"])) grouping <- paste(" by ", dictLookup(meta["grpby.tag"]), sep="")
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
	if (!name %in% names(dict)) {
		stop(gettextf("'%s' does not exist in the data dictionary", name))
	}
	
	desc <- dict[[name]]
	
	if (is.null(desc)) {
		stop(gettextf("variable named '%s' does not exist in data dictionary", name))
		name <- dname
	}
	
	#add grouping, weighting, and set descriptions (if any)
	weightdesc <- ifelse(weighting == "weightBase", "", " scenario")
	paste(desc, grouping, weightdesc, set, sep="")
}


#' Prepare run results for display by:
#'  flattening into a 3D array
#'  calculate percentages
#'  remove zero category
#'  label columns
#'  and calculate mean across all matrices
#' 
#' @param lol.mx
#'  a list of lists of matrices, eg:
#'  List of 2
#'  $ year1:List of 2
#'  ..$ 1: 'table' int [1:2, 1:3] 1 2 3 4 5 6
#'  ..$ 2: 'table' int [1:2, 1:3] 21 22 23 24 25 26
#'  $ year2:List of 2
#'  ..$ 1: 'table' int [1:2, 1:3] 31 32 33 34 35 36
#'  ..$ 2: 'table' int [1:2, 1:3] 41 42 43 44 45 46
#' 
#'  NB: matrices can have different dimensions and are aligned first within a list, and then between the lists.
#'  @seealso align.by.name.list.mx
#' @param asPercentages
#'  convert values to percentages of their grouping (if any)
#' @param removeZeroCategory 
#'  remove zero category
#' @param CI
#'  if TRUE, will produce confidence intervals on the means
#' @return
#'  a matrix of means
#' 
#' @examples
#' varname = "z1singleLvl1" ; varname = "gptotvis"
#' lol.mx <- env.base$modules$years1_5$runs$freqs$all$z1singleLvl1
#' lol.mx <- env.base$modules$years1_5$runs$freqs$all.by.ethnicity$z1singleLvl1
#' lol.mx <- env.base$modules$years1_5$runs$cfreqs$gptotvis
#'
#' lol.mx <- env.base$modules$years1_5$runs$freqs$all$sptype
#'  
#' lol.mx <- env.scenario$modules$years1_5$runs$freqs$all$z1singleLvl1 
#' lol.mx <- env.scenario$modules$years1_5$runs$freqs$all.by.ethnicity$z1singleLvl1
#' lol.mx <- env.scenario$modules$years1_5$runs$cfreqs$gptotvis
#' 
#' asPercentages = T; removeZeroCategory = T; CI = F
#' removeZeroCategory = F
#' finialise.lolmx(lol.mx)
#' finialise.lolmx(lol.mx, asPercentages, removeZeroCategory, CI)
finialise.lolmx <- function(lol.mx, asPercentages = T, removeZeroCategory = T, CI = F) {
	# flatten into 3D array
	lol.mx.array <- flatten.lolmx(lol.mx)
	
	# get percentages
	if (asPercentages) {
		lol.mx.array <- prop.table.grpby.array.zdim(lol.mx.array) * 100
	}
	
	# label cols, remove 0
	lol.mx.array.lbl <- labelFlattenedArrayCols(lol.mx.array, removeZeroCategory = removeZeroCategory)
	
	# add (%) to column names
	if (asPercentages) {
		colnames(lol.mx.array.lbl) <- paste(colnames(lol.mx.array.lbl), "(%)")
	}
	
	# combine into a single matrix by taking mean across z dimension
	result <- mean.array.z(lol.mx.array.lbl, CI = CI)
	
	# add year as a row label
	names(dimnames(result)) <- c("Year","")
	
	result
}


#' Get a set of outcome variables from the simframe.
#' 
#' @param simframe
#'  simframe
#' @param select_outcome_type
#'  a vector of outcome_types, or NULL to select all types.
#' @param select_outcome_set
#'  a vector of outcome_set, , or NULL to select all sets.
#' @param sorted
#'  return outcome variables sorted by outcome name?
#' 
#' @return
#'  a vector of simframe variables (with names the same as the vector value)
#' 
#' @examples 
#' 
#' select_outcome_set = "years1_5"
#' select_outcome_set = c("years1_5", "years6_13")
#' select_outcome_set = NULL
#' 
#' select_outcome_type = "categorical"
#' select_outcome_type = "continuous"
#' select_outcome_type = NULL
#' sorted = TRUE
#' 
#' getOutcomeVars(simframe, select_outcome_type, select_outcome_set, sorted)
getOutcomeVars <- function(simframe, select_outcome_type=NULL, select_outcome_set=NULL, sorted=FALSE) {
	
	df.outcome.vars <- attr(simframe, "df.outcome.vars")
	select_expr <- 
			outcomeSet <- subset(df.outcome.vars, (is.null(select_outcome_set) | outcome_set %in% select_outcome_set)
							& (is.null(select_outcome_type) | outcome_type %in% select_outcome_type ))
	
	setVars <- outcomeSet$simvarname
	names(setVars) <- outcomeSet$simvarname
	if (sorted)	setVars <- setVars[sort(names(setVars))]
	setVars
}

#' Label columns of a 3D array that has flattened codes for colnames.
#' 
#' @param xa
#'  3D array with flattened codes for colnames
#' @param varname
#'  category variable name of this 3D array
#' @param grpby.tag
#'  grouping of flattened codes if any, else NULL or NA  
#' @param removeZeroCategory 
#'  remove zero category
#' @return
#'  3D array with relabled columns and dropped zero category (if requested)
#' 
#' @examples
#' xa <- lol.mx.array
#' 
#' lol.mx <- env.base$modules$years1_5$runs$freqs$all$z1singleLvl1  
#' lol.mx <- env.scenario$modules$years1_5$runs$freqs$all$z1singleLvl1 
#' lol.mx <- env.base$modules$years1_5$runs$freqs$all.by.ethnicity$z1singleLvl1
#' lol.mx <- env.base$modules$years1_5$runs$cfreqs$gptotvis
#' xa <- flatten.lolmx(lol.mx) 
#' 
#' removeZeroCategory = F
#' labelFlattenedArrayCols(xa, removeZeroCategory=removeZeroCategory)
labelFlattenedArrayCols <- function(xa, varname=attr(xa, "meta")["varname"], 
		grpby.tag=attr(xa, "meta")["grpby.tag"], removeZeroCategory = T) { 
	# identify 0 category columns
	cnames <- colnames(xa)
	if (is.null(grpby.tag) || is.na(grpby.tag)) {
		zerocols <- which(cnames == "0")
	} else {
		zerocols <- grep("\\s0", cnames)
	}
	
	colnames(xa) <- codingMatchFlattened(cnames, varname, grpby.tag)
	
	# remove 0 category, if requested
	if (removeZeroCategory && length(zerocols)) {
		structure(xa[,-zerocols,,drop=FALSE], meta=c(attr(xa, "meta")))
	} else {
		xa
	}
}

library(xlsx)

#' Load codings xls file and return named list of codings.
#' 
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

#' Load and create a GLM from the first sheet in an xlsx file.
#'
#' @param filedir
#'  file directory, ending with "/", eg: "d:/workspace/"
#' @param filename
#'  file name, eg: "myfile.xls"
#' @return 
#'  a glm model object
#' 
#' @examples
#' filedir <- "D:/workspace.sim/MELC/CHDS/models/"
#' filename <- "gptotvis.xlsx" ; filename <- "paddhsbetas.xlsx"
#' 
#' loadGLMXLS(filedir, filename)
loadGLMXLS <- function (filedir, filename) {
	modeldf <- readXLSSheet1(filedir, filename)
	tryCatch(	createGLM(modeldf), 
			error = function(e) stop(paste(filename, e$message), call. = FALSE),
			warning = function(e) stop(paste(filename, e$message), call. = FALSE)
			)
}

#' Load and create a GLM from a csv file.
#'
#' @param filedir
#'  file directory, ending with "/", eg: "d:/workspace/"
#' @param filename
#'  file name, eg: "myfile.csv"
#' @return 
#'  a glm model object
#'  
#' @examples
#' filedir <- "D:/workspace.sim/MELC/CHDS/models/"
#' filename <- "Burt9_10.csv"
#' 
#' loadGLMCSV(filedir , filename)
loadGLMCSV <- function (filedir, filename) {
	modeldf <- read.csv(paste(filedir, filename, sep=""),stringsAsFactors = FALSE)
	tryCatch(	createGLM(modeldf), 
			error = function(e) stop(paste(filename, e$message), call. = FALSE),
			warning = function(e) stop(paste(filename, e$message), call. = FALSE)
	)
}

#' Helper function that loads a CSV file.
#' 
#' @param filedir
#'  file directory, ending with "/", eg: "d:/workspace/"
#' @param filename
#'  file name, eg: "myfile.csv"
#' @return 
#'  a data frame containing the file
#' 
#' @examples 
#' loadCSV(propensityfiledir, "accom_Propens.csv")
loadCSV <- function(filedir, filename) {
	read.csv(paste(filedir, filename, sep=""),stringsAsFactors = FALSE)
}

#' Creates a simulation frame. A simulation frame is the set of all variables
#' (input, intermediate, and outcome) used or produced by models during 
#' a simulation. During simulation the simulation frame will contain the
#' current iteration's set of variables being used in the simulation.
#' This function establishes the values of the simulation frame at the start 
#' prior to simulation using the initial values specified by the simframe
#' definition.
#' 
#' @param bf 
#'   base file
#' @param sfdef
#'   simframe definition. A dataframe of variables that make up the simframe, 
#'   defined as follows:
#'       $simvarname - the name of the variable in the simframe
#'       $previous_var - (if any) the name in which to store the variable value in at the end
#'                       the iteration, for use in the next iteration
#'       $initial_value - an expression that generates the initial value of the variable.
#'                        The expression is evaluated in the context of the base file, and
#'                        if not found then in the context of the calling frame.
#'					      If empty then an initial numeric value of NA is used.
#' 		 $outcome_type - if this is an outcome variable, it's tpype, one of "categorical" or "continuous"
#' 		 $outcome_set - if an outcome variable, the set it belongs to
#'
#' @return
#'  the simulation frame. The simulation frame contains the variables in sfdef with
#'  their inital_value evaluated. Each variable contains a set of observations. Each
#'  observation represents a case in the basefile.
#'  
#'  If an observation is NA, then that case (ie: it's observations for all 
#'  variables) is removed. This however does not apply to initial_value expressions 
#'  that return a singular NA. These remain as a vector containing NA for 
#'  as each observation.
#'   
#'  The frame has same rownames as basefile. This is useful during investigation 
#'  or debugging.
#' 
#' 	The simulation frame object has the following attributes:
#' 
#' 	"previous" - variables that represent values in the previous iteration
#'  "df.outcome.vars" - a dataframe of simvarname, outcome_type, and outcome_set
#'	"na.omit" - an omit variable that indicates the observations that were removed
#' 				because their were NA. Initial_values that returned a singular NA
#' 				remain.
#' 
#' @examples 
#' 
#' bf <- children
#' basefiledir <- "D:/workspace.sim/MELC/CHDS/base/"
#' sfdef <- readXLSSheet1(basefiledir, "simframedef.xlsx")
#' simframe.start <- loadSimFrame(bf, sfdef)
#' simframe <- loadSimFrame(bf, sfdef)
loadSimFrame <- function (bf, sfdef) {
	
	#remove empty variables, generally these are blank lines
	#at the end of the file
	sfdef <- subset(sfdef, !(simvarname==""))
	
	#check for duplicated simvarnames
	dups <- which(duplicated(sfdef$simvarname))
	
	if(length(dups) > 1) {
		stop(paste("Simframe varname duplicate:", sfdef$simvarname[dups], "\n"))
	}
	
	#load initial_value for evaluation
	initial_value_all <- sfdef$initial_value
	names(initial_value_all) <- sfdef$simvarname

	#replace empty intial_value with numeric NA
	initial_value_all[(initial_value_all == "")] <- as.numeric(NA)
	
	#evaluate "initial_value" column. Any objects in the exprs must exist 
	#in bf or if not then in the global environment
	sfvalues  <- eval.list(initial_value_all, bf)
	
	# convert non NA values to data.frame
	# this repeats any inital values that are singular
	sfvalues.df <- data.frame(sfvalues[!is.na(sfvalues)])
	rownames(sfvalues.df) <- rownames(bf)	#setup rownames same as basefile
	
	#remove obs. that have NAs in one of their values
	sfvalues.df <- na.omit(sfvalues.df)
	nas <- attr(sfvalues.df, "na.action")
	
	# add singular NA values back to data.frame
	sfvalues.df  <- cbind(sfvalues.df, 
			as.list(sfvalues[is.na(sfvalues)]), stringsAsFactors=FALSE)
	
	# setup sfprevious
	# sfprevious = the names of the variables that represent values in the previous iteration
	# names(sfprevious) = the source value for the previous variable
	sfprevious <- as.character(sfdef$previous_var)
	names(sfprevious) <- sfdef$simvarname
	sfprevious <- stripEmpty(sfprevious)
	
	#check previous values exist in simvarname
	nonexistent_previous <- !(sfprevious %in% sfdef$simvarname)
	if (any(nonexistent_previous)) {
		stop(gettextf("previous variable(s) %s: not in simframe", 
						paste(sfprevious[nonexistent_previous],collapse=", ")))
	}
	
	#data frame of outcome var mappings, types and set
	df.outcome.vars <- subset(sfdef, outcome_set != "", c("simvarname", "outcome_type", "outcome_set"))
	
	#return
	structure(sfvalues.df, previous=sfprevious,
			df.outcome.vars = df.outcome.vars,
			na.actions=nas)
}

#' Get the column (category) values (or as a percentage) of the mean of all runs in list.var.run.mx
#' combining all year matrices into a single matrix.
#' 
#' For each list in list.var.run.mx:
#' *take the mean, combining multiple lists of matrices into a single list of matrices.
#' *flatten each matrix into a single row, calculating proportions and removing 0 category if requested
#' *combine the rows (ie. iterations) into a single matrix
#'  
#' @param list.var.run.mx
#'  a list of variable elements, containing run elements, containing year matrices.
#'  Year matrices are frequency tables, columns are the grouped by elements
#'  and rows the categories
#' 
#'  eg:
#' 
#'  > str(list.var.run.mx)
#'  List of 13
#'  $ msmoke       :List of 2
#'  ..$ :List of 5
#'  .. ..$ 1: 'table' int [1:2, 1:3] 646 284 23 12 52 58
#'
#' @param removeZeroCategory
#'  if TRUE, remove all category (column) that is named "0"
#' @param asPercentages
#'  if TRUE, return the values as a column percentage 
#' 
#' @return
#'  a list of matrices containing the calculated proportion of the 
#'  mean of all runs
#' 
#' @examples
#' . <- env.base$modules$years1_5 ; . <- env.scenario$modules$years1_5
#' list.var.run.mx <- .$runs$cfreqs ;  removeZeroCategory = F; asPercentages = F
#' list.var.run.mx <- env.base$modules$years1_5$runs$cfreqs ;  removeZeroCategory = F
#' list.var.run.mx <- env.base$modules$years1_5$runs$freqs$all
#' list.var.run.mx <- .$runs$freqs$all.by.ethnicity
#' 
#' removeZeroCategory = TRUE ; removeZeroCategory = F 
#' list.var.run.mx.mean.flat.combined <- mean.list.var.run.mx ( list.var.run.mx )
#' list.var.run.mx.mean.flat.combined <- mean.list.var.run.mx ( list.var.run.mx , removeZeroCategory)
mean.list.var.run.mx <- function (list.var.run.mx, removeZeroCategory = T, asPercentages = T) {

	if(length(list.var.run.mx) == 0) {
		return(list.var.run.mx)
	}
	
	# get mean across all years' matrices. combines multiple lists of matrices into a single list.
	# to do this, aligns each inner combination first
	# lapply.inner.combination (list.var.run.mx[[1]], .FUN=mean.list.mx)
	# lapply.inner.combination (list.var.run.mx$typeofchange, .FUN=mean.list.mx)
	list.var.run.mx.mean <- lapply(list.var.run.mx, lapply.inner.combination, .FUN=mean.list.mx)

	# add varname attribute from list element name. used by next step
	varnames <- names(list.var.run.mx)
	if(is.null(varnames)) varnames <- ""
	list.var.run.mx.mean2 <- mapply(function(x, varname) {
				lapply(x, `attr<-`, "varname.tag", varname)
			}, list.var.run.mx.mean, varnames, SIMPLIFY = FALSE)
	
	# flatten each matrix into a single row, calculating proportions and removing 0 category if requested,
	# and adding group by and category codings if any
	list.var.run.mx.mean.flat <- lapply.inner(list.var.run.mx.mean2, function(mx)  {
		#mx <- list.var.run.mx.mean2[[1]][[1]]
		#mx <- list.var.run.mx.mean2[[1]][[2]]
		#mx <- list.var.run.mx.mean2[[3]][[1]]
		#mx <- list.var.run.mx.mean2[[14]][[1]]
		#mx <- list.var.run.mx.mean2[[14]][[2]]				
		#mx <- list.var.run.mx.mean2[[14]][[3]]
		
		# get COL proportions, i.e: proportions by each group by (if any)
		if (asPercentages) {
			mx <- prop.table(mx, COL) * 100
		}
		
		# add group by codings, if any
		grpby.tag <- attr(mx, "meta")["grpby.tag"]
		if(!is.null(grpby.tag)) {
			colnames(mx) <- names(codings[[grpby.tag]])
		}
		
		# identify 0 category
		zero.cat.row <- which(rownames(mx) %in% "0")
		
		# add category codings, if any
		varname <- attr(mx, "varname.tag")
		catcodings <-  codings[[varname]]
		if (!is.null(catcodings)) {
			#rownames(mx) <- names(catcodings)
			
			# match row names into codings
			codings.indices <- match(rownames(mx), catcodings)
			rownames(mx) <- names(catcodings)[codings.indices]
		}
		
		# remove 0 category
		if (removeZeroCategory && length(zero.cat.row) > 0) {
			# drop = FALSE so we don't coerce down to vector
			# and lose the rowname if only a single row left
			mx <- mx[-zero.cat.row,,drop = FALSE]
		}
		
		# flatten into single row matrix
		mx.flat <- flatten.mx(mx)
		
		# NB: we add percentages to column names in the last stage below,
		# AFTER we have done the aligning
		
		# add meta for identification during display
		structure(mx.flat, meta=c(varname=varname, grpby.tag))
	})
	
	# align each matrix, and then collapse into a single matrix with rows = years and z = cols
	list.var.run.mx.mean.flat.array <- lapply(list.var.run.mx.mean.flat, function(x) {
				#x <- list.var.run.mx.mean.flat[[3]]
				#x <- list.var.run.mx.mean.flat$typeofchange
				
				# NB: do not drop extra dimensions of length 1, else if the 
				# column dimension is 1 (as in the case of a single category)
				# then vectors will be created and the column name heading will be lost
				# this will return an array
				
				# NB: may be possible to use unlist here instead of laply
				# but it's working now with laply
				x.aligned <- align.by.name.list.mx(x)
				x.aligned.combined <- laply(x.aligned , .fun=identity, .drop = FALSE)
				
				# return with first meta
				structure(x.aligned.combined, meta=attr(x[[1]],"meta"), rowlabels = names(x))
		})

	# drop y dimension of length 1 and add rowlabels. keep meta.
	lapply(list.var.run.mx.mean.flat.array, function(x) {
				#x <- list.var.run.mx.mean.flat.array[[1]]
				#x <- list.var.run.mx.mean.flat.array[[2]]
			
				col.names <- dimnames(x)[[ZDIM]]
				if (asPercentages) {
					# add percentages to column names
					# NB: we add them at the last stage because adding (%) to NA
					# makes a non-numeric value which will produce a non-numeric
					# sort during align.by.name.list.mx above
					col.names <- paste(col.names , "(%)")
				}
				
				structure(
						matrix(x, nrow=dim(x)[ROW], ncol=dim(x)[ZDIM], 
								dimnames=list("Year"=attr(x, "rowlabels"), col.names)),
						meta=attr(x,"meta"))
			})
}



#' Create means of frequencies across all runs
#' 
#' @examples
#' runs.freqs <- runs$freqs$all
#' results$freqs$all <- meanRunFreqs(runsfreqs)
meanRunFreqs <- function(runs.freqs) {
	
	# Take mean across Z dim, label the column names with coding names,
	# and set the column title to the same as the name in the list
	freqs <- labelColFromVec(labelColTitleFromList(
					lapply(runs.freqs, function(x) 
								mean.array.z(x, CI=FALSE))), codingsNamesLookup(names(runs.freqs)))
	
	# add row percents and "Year" row title
	freqs <- lapply(freqs, 
			function(x) labelTitle(addRowPercents(x), ROW, "Year"))
	
	# remove counts 
	freqs <- lapply(freqs,
			function(x) {
				# remove first two columns, keep meta
				structure(x[,-c(1,2)], meta=attr(x, "meta"))
			})
	
	freqs
}

#' Create means of continuous frequencies across all runs
#' 
#' @param runs.cfreqs
#'  a list of lists. the inner lists contain as many elements
#'  as their were runs
#' 
#' @examples
#'
#' runs.cfreqs <- runs$cfreqs
#' runs.cfreqs <- env.base$years1_5$runs$cfreqs
#' runs.cfreqs <- runs$summaries
#' results$cfreqs <- meanRunCFreqs(runs)
meanRunCFreqs <- function(runs.cfreqs) {
	
	if (length(runs.cfreqs) == 0) {
		return(list())
	}
	
	iterationNames <- rownames(runs.cfreqs[[1]][[1]])
	
	# merge rows to get by year (in sorted column order)
	by.year <- lapply(runs.cfreqs, merge.list.mx.by.rows)
	
	# calculate mean across runs, NAs are return as 0.
	cfreqs <- lapply(by.year, colmeans.list)
	
	# combine each year into a single matrix and label row with year
	cfreqs <- lapply(cfreqs, function(x) {
				mx <- as.matrixFromList(x)
				rownames(mx) <- iterationNames
				mx
			})
	
	# calculate as proportion
	cfreqs.prop <- lapply(cfreqs, prop.table, ROW)
	
	cfreqs.prop
}

#' Return the coefficients used in the supplied model.
#' 
#' @param model
#'  a glm object with a coef variable, or a numeric vector of coefficients
#' @param combineMultipleLevels
#'  variables that have multiple levels (eg: SESBTHLvl1, SESBTHLvl2) 
#'  are combined into a single variable (eg: SESBTH) by summing the 
#'  coefficients
#' @param ignoreMultiplicativeTerms
#'  remove terms with * in them
#' @param directionOnly
#'  show "+" or "-" instead of coefficent value
#' @return
#'   chr named vector of coefficients
#' 
#' @examples
#' model <- models$z1homeownLvl1
#' model <- c(coef(models$logit_gp0totvis), coef(models$gptotvis))
#' model <- models.to.show$burt 
#' model <- models$gpmorb
#' model <- models$cond7_8
#' model <- models$welfareLvl1
#' combineMultipleLevels = TRUE; ignoreMultiplicativeTerms = TRUE; directionOnly = TRUE
#' modelVariableCoefs(model) 
#' modelVariableCoefs(model, strip.Lvl = FALSE)
modelVariableCoefs <- function (model, combineMultipleLevels = TRUE, ignoreMultiplicativeTerms = TRUE, directionOnly = TRUE) {
	
	cls <- match.arg(class(model)[1], c("glm","numeric")) 
	coefs <- switch(cls,
			glm = coef(model),
			numeric = model)
	
	# remove any named "Intercept"
	coefs <- coefs[!(match(names(coefs), "Intercept", nomatch = 0))]

	# remove terms with * in them
	if (ignoreMultiplicativeTerms) {
		multiplicateTerms <- grep("\\*", names(coefs))
		if (any(multiplicateTerms)) {
			coefs <- coefs[-multiplicateTerms]
		}
	}
	
	if (combineMultipleLevels) {
		# remove trailing "LvlX"
		coefs.root <- gsub("Lvl.$", "", names(coefs))
		
		# sum coefs based on their unique root 
		coefs <- sapply(unique(coefs.root), function(x) {
					#x <- coefs.unique[1]
					sum(coefs[as.logical(match(coefs.root, x))], na.rm=T)
				}) 
	}
	
	if (directionOnly) {
		ifelse(coefs < 0, "-", "+")
	} else {
		coefs
	}
	
}

#' Return the variables used in the supplied terms.
#' Removes "LvlX" suffix, and surrounding "I( )"
#' as well as disaggregates squared variables
#' 
#' @param xterms
#'   terms
#' @param strip.Lvl
#'  strip the LvlX from names. Defaults to TRUE.
#' @return
#'   chr vector of variable names
#' 
#' @examples
#' model <- models$z1accomLvl1
#' modelVariableNames(model) 
#' modelVariableNames(model, strip.Lvl = FALSE)
modelVariableNames <- function (model, strip.Lvl = TRUE) {

	l <- labels(terms(model))
	
	# strip prefix "I(" and suffix ")"
	l <- gsub("^I\\(|\\)$", "", l)

	# strip trailing "LvlX"
	if (strip.Lvl) l <- gsub("Lvl.$", "", l)
	
	# replace squared variables by variable name, eg: "X * X" -> "X"
	#l <- gsub("(\\w*)\\s\\*\\s\\1", "\\1", l)
	
	# sort and remove duplicates
	unique(sort(l))
	
}

#' Predict. Looks in envir for variables specified by model, then multiples the coefficients
#' by each variable and summs the results.
#' 
#' @param model
#'  model with terms and coefficiens
#' @param envir
#'  an environment in which model variables are evaluated. May also be NULL, a list, 
#'  a data frame, a pairlist or an integer as specified to sys.call.
#'  
#'  If the specified envir is a list or data.frame, then the parent frame is used
#'  as the enclosing environment in which variables that don't exist in envir will
#'  be evaluated.
#' 
#'  If not specified then the parent frame is used, ie: the environment of the function
#'  calling this (i.e: the dynamic parent rather than the lexical parent in which this
#'  function is defined).
#'  
#'  The usual order of evaluation is used, i.e: in envir, then its parents and along
#'  the search path. For example, if a model variable is reassigned in the function
#'  calling this, then that reassigned value will be used before a global or attached
#'  value.
#' 
#' @param set
#'  subset logical expression indicating elements or rows to keep, or NULL to use
#'  all elements returned by evaluated model variables
#' 
#' @examples 
#'  model <- model.glm
#'  model <- models$gptotvis ; set <- c(T, rep(F, 1074))
predict <- function(model, envir = parent.frame(), set = NULL) {
	
	# get vars from model
	vars <- attr(delete.response(terms(model)), "variables")
	
	#evalute vars, return as list
	vars.evaluated <- eval(vars, envir)
	#names(vars.evaluated) <- as.character(vars)[-1]
	
	#convert to matrix 
	vars.evaluated.mx <- as.matrixFromList(vars.evaluated, byrow = F)
	
	#subset
	if (!is.null(set)) {
		vars.evaluated.mx <- subset(vars.evaluated.mx, set) 
	}
	
	#add intercept of 1 
	vars.evaluated.mx <- cbind(Intercept=1, vars.evaluated.mx)
	
	# matrix multiple by model coefficients
	drop(vars.evaluated.mx %*% model$coefficients)
}

#' Predict and simulate binary value from logistic model
#' 
#' @param model.glm
#'  model specifying variables to evaluate and coefficients
#'  to multiple by.
#' @param envir
#'  environment in which to evaluate model variables.
#'  if unspecified, uses caller's environment
#' @param set
#'  subset logical expression indicating elements or rows to keep, or NULL to use
#'  all elements returned by evaluated model variables
#'  
#' @examples
#'  model.glm <- models$z1msmokeLvl1
predSimBin <- function(model.glm, envir=parent.frame(), set = NULL) {
	
	#determine predicted values
	predicted_logits <- predict(model.glm, envir, set)
	predicted_probabilities <- exp(predicted_logits)/(1+exp(predicted_logits))
	
	#simulate
	randunif <- runif(length(predicted_probabilities)) 
	ifelse(randunif <= predicted_probabilities, 1, 0) 
}

#' Predict and simulate continuous value from binomial
#' distribution with probability
#' 
#' @param model.glm
#'  model specifying variables to evaluate and coefficients
#'  to multiple by.
#' @param envir
#'  environment in which to evaluate model variables.
#'  if unspecified, uses caller's environment
#' @param set
#'  subset logical expression indicating elements or rows to keep, or NULL to use
#'  all elements returned by evaluated model variables
#'  
#' @examples
#' model.glm <- models$houtptot
#' newdata <- simvalues
predSimBinom <- function(model.glm, envir=parent.frame(), set = NULL) {
	
	#determine predicted values
	predicted_logits <- predict(model.glm, envir, set)
	predicted_probabilities <- exp(predicted_logits)/(1+exp(predicted_logits))
	
	#simulate
	sapply(predicted_probabilities , function (x) rbinom(1, size=1, prob=x)) 
}

#' Predict and simulate continuous value from poisson model
#' 
#' @param model.glm
#'  model specifying variables to evaluate and coefficients
#'  to multiple by.
#' @param envir
#'  environment in which to evaluate model variables.
#'  if unspecified, uses caller's environment
#' @param set
#'  subset logical expression indicating elements or rows to keep, or NULL to use
#'  all elements returned by evaluated model variables
#' 
#' @examples 
#'  model.glm <- models$hadmtot
#' 	newdata <- simvalues
predSimPois <- function(model.glm, envir=parent.frame(), set = NULL) {
	#determine predicted values
	predicted_logs <- predict(model.glm, envir, set)
	predicted_means <- exp(predicted_logs)
	
	#simulate
	sapply(predicted_means, function (x) rpois(1, x)) 
}

#' Predict and simulate continuous value from negative binomial
#' distribution with mean = exp(predicted) and size=1/alpha
#' where alpha is specified in the model.
#'
#' @param model.glm
#'  model specifying variables to evaluate and coefficients
#'  to multiple by.
#' @param envir
#'  environment in which to evaluate model variables.
#'  if unspecified, uses caller's environment
#' @param set
#'  subset logical expression indicating elements or rows to keep, or NULL to use
#'  all elements returned by evaluated model variables
#'  
#' @examples 
#' model.glm <- models$houtptot
#' newdata <- simvalues
#' predSimNBinom (model.glm, newdata)
predSimNBinom <- function(model.glm, envir=parent.frame(), set = NULL) {
	
	#determine predicted values
	predicted_logs <- predict(model.glm, envir, set)
	predicted_means <- exp(predicted_logs)
	
	alpha <- model.glm$alpha
	
	#simulate
	sapply(predicted_means, function (x) rnbinom(1, size=1/alpha, mu=x)) 
}

#' Predict and simulate continuous value from normal
#' distribution of specified standard deviation
#' 
#' @param model.glm
#'  model specifying variables to evaluate and coefficients
#'  to multiple by.
#' @param envir
#'  environment in which to evaluate model variables.
#'  if unspecified, uses caller's environment
#' @param set
#'  subset logical expression indicating elements or rows to keep, or NULL to use
#'  all elements returned by evaluated model variables
#'  
#' @examples
#' model.glm <- models$gpprev12
#' newdata <- simvalues
predSimNorm <- function(model.glm, envir=parent.frame(), set = NULL) {
	#determine predicted values
	predicted <- predict(model.glm, envir, set)
	
	sd <- model.glm$sd
	
	#simulate
	sapply(predicted, function (x) rnorm(1, mean=x, sd=sd)) 
}

#' Calculate the proportions within groups that 
#' exist in the rows of the matrics in the ZDIM of xa
#' 
#' @param xa
#'  and 3D array, with matrices in the ZDIM.
#' @param grpby.tag
#'  specifies a categorical variable with codings, or NULL if no groups 
#' @return
#'  xa as proportions
#' @examples
#' lol.mx <- env.base$modules$years1_5$runs$freqs$all$z1singleLvl1 
#' lol.mx <- env.base$modules$years1_5$runs$freqs$all.by.ethnicity$z1singleLvl1
#' lol.mx <- env.base$modules$years1_5$runs$cfreqs$gptotvis
#' 
#' lol.mx <- env.scenario$modules$years1_5$runs$freqs$all$sptype
#' xa <- flatten.lolmx(lol.mx)
#' 
#' xa <- array(c(1:5, rep(0,5)), dim=c(5,1,2), dimnames=list(LETTERS[1:5],c("Col")))
#' xa <- array(c(1:5, 9,8,7,6,5), dim=c(5,2,1), dimnames=list(LETTERS[1:5], c("Col 1", "Col 2")))
#' 
#' xa <- lol.mx.array
#' prop.table.grpby.array.zdim(xa)
prop.table.grpby.array.zdim <- function (xa, grpby.tag=attr(xa, "meta")["grpby.tag"]) {
	  
	# if no groupings, then across the whole row
	numgrps <- if(is.null(grpby.tag) || is.na(grpby.tag)) 1 else length(codings[[grpby.tag]]) 
	grpsize <- ncol(xa) / numgrps
	grpby <- rep(1:numgrps, each=grpsize)
	
	# get proportions by grp
	xa.prop <- apply(xa, c(ROW,ZDIM), prop.table.grpby, grpby=grpby)
	
	if (ncol(xa)==1) {
		# apply simplifies if we only have one column, so reapply dimensions
		xa.prop.t <- structure(xa.prop, .Dim=dim(xa))
	} else {
		# transpose ZDIM
		xa.prop.t <- aperm(xa.prop, perm=c(2,1,3))
	}
	
	structure(xa.prop.t, meta = attr(xa, "meta"), dimnames=dimnames(xa))
	
}


#' Takes a result row and returns the means and error amounts as seperate vectors in a matrix or list.
#' 
#' @param result.row
#'  a result row, ie: a vector with values named Mean and Lower eg:
#' 
#'>  envs$`Scenario 1`$years1_5$results$means$all$kids["Total",]
#'     Mean    Lower    Upper 
#' 10.99488 10.62256 11.36721 
#' 
#'  if there are no values named Mean, then it will be assumed that all values
#'  are Means and that Lower is 0.
#' 
#' @param simplify
#'  if TRUE (default), returns a matrix instead of a list. 
#' 
#' @return
#'  a matrix/list of means and errs. The first row/means vector is the means from the result row, and the
#'  second row/errs vector is the difference between each mean and it's lower value.
#' 
#' @examples
#' 
#' eg:
#' 
#' > result.row <- envs$`Scenario 1`$years1_5$results$means$all$kids["Total",]
#' > result.row
#'     Mean    Lower    Upper 
#' 10.99488 10.62256 11.36721
#'  
#' > result.as.means.and.errs(result.row)
#' $means
#' 
#' 10.99488 
#' 
#' $errs
#' 
#' 0.3723213 
#'
#'  
#' result.row <- c("0%"=5,"20%"=5,"40%"=9,"60%"=11,"80%"=15,"100%"=50)
#' result.row <- structure(c(5, 5, 5, 5, 5, 5, 9, 9, 9, 11, 11, 11, 15, 15, 15,50.5, 6.02828342338857, 94.9717165766114), .Names = c("0% Mean","0% Lower", "0% Upper", "20% Mean", "20% Lower", "20% Upper","40% Mean", "40% Lower", "40% Upper", "60% Mean", "60% Lower","60% Upper", "80% Mean", "80% Lower", "80% Upper", "100% Mean","100% Lower", "100% Upper"))
#' 
#' result.row <- env.base$years1_5$results$quantiles$kids["Total",]
#' result.row <- envs$`Scenario 1`$years1_5$results$quantiles$kids["Total",]
#' result.row <- envs$`Scenario 1`$years1_5$results$means$all$kids["Total",]
#' 
#' result.as.means.and.errs(result.row)
result.as.means.and.errs <- function(result.row, simplify = T) {
	ind.means <- grep("Mean", names(result.row))
	ind.lowers <- grep("Lower", names(result.row))
	
	#result.row.means <- c()
	#result.row.err <- c()
	if(!length(ind.means)) {
		result.row.means <- result.row
		result.row.err <- structure(rep(0, length(result.row.means)), .Names = names(result.row.means))
	} else {
		result.row.means <- result.row[ind.means]
		names(result.row.means) <- trim(gsub("Mean", "", names(result.row.means)))
		
		result.row.err <- result.row.means - result.row[ind.lowers]
	}
	
	if (simplify) {
		rbind(means=result.row.means, errs=result.row.err)
	} else {
		list(means=result.row.means, errs=result.row.err)
	}
}	

#' Convience method for saving environment to
#' an Rdata file for debugging.
#' 
#' @param env
#'  environment to save, defaults to env.scenario
#' @param file
#'  file name, defaults to "env.rdata"
#' 
#' @examples
#'  save.env()
save.env <- function(env=env.scenario, file="env.rdata") {
	save(env, file=file)
}

#' Appends the supplied simenv to envs (the list of
#' scenario environments) and sets env.scenario
#' to point to the simenv.
#' 
#' Called by initMELC()
#'
#' @param simenv
#'  a simenv. If name is NULL then the name will be set to "Scenario X" where X is the
#'  next scenario number.
#' 
#' @examples
#' setScenarioSimenv(SimenvMELC$new())
setScenarioSimenv <- function(simenv) {
	if (!exists("envs")) {
		envs <<- list()
		if (existsFunction("ascapeKeepObject")) ascapeKeepObject("envs")
	}
	
	if (is.null(simenv$envName)) {
		simenv$envName <- paste("Scenario",length(envs)+1)
	}
	
	env.scenario <<- simenv
	envs[[simenv$envName]] <<- env.scenario
}

#' Produce a proportioned table for x, using
#' the specified coding as names and 
#' setting the "meta" attribute to "varname"
#' of coding.
#' 
#' @param x
#'  vector of values
#' @param coding
#'  a coding variable. names(coding) is the labels
#'  attr(coding, "varname") is a named element in xlist
#' @return 
#'  a table (proportions) with names specified by coding 
#' @examples
#' 
#' table.catvar(children$SESBTH, codings$SESBTH)
#' x <- simframe$z1singleLvl1 ; coding <- codings$z1singleLvl1
#' table.catvar(simframe$z1singleLvl1, codings$z1singleLvl1)
table.catvar <- function (x, coding) {
	
	varname <- attr(coding, "varname")
	
	tbl <- prop.table(table(x)) * 100
	
	# match names into codings
	codings.indices <- match(names(tbl), coding)
	names(tbl) <- paste(names(coding)[codings.indices], "(%)")
	
	attr(tbl, "meta") <- c("varname" = varname)
	
	tbl
}

#' Display a continuous variable in a table using the
#' breaks supplied.
#' Attachs a meta attribute with varname
#' 
#' @examples
#' 
#' x <- env.scenario$simframe$bwkg
#' breaks <- binbreaks$bwkg
#' 
#' table.contvar(env.scenario$simframe$bwkg, binbreaks$bwkg, "bwkg")
table.contvar <- function (x, breaks, varname) {
	tbl <- prop.table(table(bin(x, breaks, breaklast=NULL), useNA='ifany')) * 100
	attr(tbl, "meta") <- c("varname" = varname)
	tbl
}

cat("Loaded simulate\n")
