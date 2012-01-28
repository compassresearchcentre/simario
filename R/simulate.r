# Simulation support functions.
#
# Requires support.r
# 
# Author: Oliver Mannion
###############################################################################


#' Create a generalized linear model
#' from a model dataframe that contains the model variables
#' and their coefficients
#' NB: the variable "_Alpha" will not be added to the model formula
#'     but will be added as the model component m.glm$alpha 
#' NB: the variable "_sd" will not be added to the model formula
#'     but will be added as the model component m.glm$sd 
#' 
#' @param modeldf
#'  model dataframe containing the rows:
#' "Variable" = variable name. can be an expression like MAGE*MAGE
#' "ClassVal0" = variable level. If present is appended to the variable name, eg: MAGELvl1
#' "Estimate" = variable coeffient
#' 
#' @examples 
#' 
#' #m.glm <- createGLM(modeldf) 
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
		sd_ <- mcoef[sdIndex]
		varnames <- varnames[-sdIndex]
		varlevels <- varlevels[-sdIndex]
		mcoef <- mcoef[-sdIndex]
	}	
	
	#add "LvlX" where appropriate to the var name
	vars <- paste(varnames,
			ifelse(is.na(varlevels) | varlevels=="","",paste("Lvl",varlevels,sep="")),
			sep="")
	
	names(mcoef) <- vars
	
	if (length(vars) == 1) {
		#no vars, only an Intercept
		fmla <- as.formula("y ~ 0")
	} else {
		#construct formula from vars (except first var, ie: Intercept) and get terms
		#wrap varnames in I() so that terms like MAGE*MAGE will work as expected
		#when submitted to model.frame and model.matrix during prediction
		fmla <- as.formula(paste("y ~ ", paste("I(",vars[-1],")", sep="", collapse= "+")))
	}
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

	if (exists("sd_")) {
		m.glm$sd <- sd_
	}
	
	class(m.glm) <- c("glm","lm") 
	m.glm
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
#' \dontrun{
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
#' }
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
#' @param dict
#'  Dictionary object instance
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
#' \dontrun{
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
#' }
finialise.lolmx <- function(lol.mx, dict, asPercentages = T, removeZeroCategory = T, CI = F) {
	# flatten into 3D array
	lol.mx.array <- flatten.lolmx(lol.mx)
	
	# get percentages
	if (asPercentages) {
		#calculating numgrps. If no groupings, then across the whole row
		grpby.tag = attr(lol.mx.array, "meta")["grpby.tag"]
		numgrps <- if(is.null(grpby.tag) || is.na(grpby.tag)) 1 else length(dict$codings[[grpby.tag]]) 
		
		lol.mx.array <- prop.table.grpby.array.zdim(lol.mx.array, numgrps) * 100
	}
	
	# label cols, remove 0
	lol.mx.array.lbl <- labelFlattenedArrayCols(lol.mx.array, dict, removeZeroCategory = removeZeroCategory)
	
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

#' Label columns of a 3D array that has flattened codes for colnames.
#' 
#' @param xa
#'  3D array with flattened codes for colnames.
#'  A flattened code is in the form "0 1",
#'  where the first value is a grping code and the second a varname code.
#'  If grpby.tag is NULL or NA, then the flattened code will be in the form "0", 
#'  i.e: no grping codes only varname codes.
#' @param dict
#'  a Dictionary proto object
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
#' \dontrun{
#' xa <- lol.mx.array
#' 
#' lol.mx <- env.base$modules$years1_5$runs$freqs$all$z1singleLvl1  
#' lol.mx <- env.scenario$modules$years1_5$runs$freqs$all$z1singleLvl1 
#' lol.mx <- env.base$modules$years1_5$runs$freqs$all.by.ethnicity$z1singleLvl1
#' lol.mx <- env.base$modules$years1_5$runs$cfreqs$gptotvis
#' xa <- flatten.lolmx(lol.mx) 
#' dict <- dict.MELC
#' removeZeroCategory = F
#' 
#' xa <- r$all.by.gender$householdsize
#' 
#' labelFlattenedArrayCols(xa, dict=dict, removeZeroCategory=removeZeroCategory)
#' }
labelFlattenedArrayCols <- function(xa, dict, varname=attr(xa, "meta")["varname"], 
		grpby.tag=attr(xa, "meta")["grpby.tag"], removeZeroCategory = T) {
	
	# identify 0 category columns
	cnames <- colnames(xa)
	if (is.null(grpby.tag) || is.na(grpby.tag)) {
		zerocols <- which(cnames == "0")
	} else {
		zerocols <- grep("\\s0", cnames)
	}
	
	colnames(xa) <- dict$cmatchFlattened(cnames, varname, grpby.tag)
	
	# remove 0 category, if requested
	if (removeZeroCategory && length(zerocols)) {
		structure(xa[,-zerocols,,drop=FALSE], meta=c(attr(xa, "meta")))
	} else {
		xa
	}
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
#' @examples
#' \dontrun{
#' x <- structure(matrix(1:2, nrow=1, dimnames=list(1, c("0","1"))), meta=c("grpby.tag"="z1gender"))
#' x <- structure(matrix(1:6, nrow=1, dimnames=list(1, c("0 Mean","0 Lower","0 Upper","1 Mean","1 Lower","1 Upper"))), meta=c("grpby.tag"="z1gender"))
#' 
#' x <- env.base$modules$years1_5$results$means$all.by.gender$kids
#' x <- env.scenario$modules$years1_5$results$means$all.by.gender$kids
#' x <- env.base$modules$years1_5$results$means$all$kids
#' x <- env.base$modules$years1_5$results$means$all.by.gender$gptotvis
#' varname=attr(x, "meta")["grpby.tag"]
#' dict <- dict.MELC
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
	cnames_numeric_desc <- names(catcodings)[codings_indices]
	
	# combine desc with existing alpha, NB: assume alpha is at the end
	dimnames(x)[[COL]] <- paste(cnames_numeric_desc, cnames_alpha, sep = "")	
	
	# add varname desc
	desc <- dict$descriptions[[varname]]
	names(dimnames(x))[[COL]] <- desc
	
	x
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
#' \dontrun{
#' loadCSV(propensityfiledir, "accom_Propens.csv")
#' }
loadCSV <- function(filedir, filename) {
	read.csv(paste(filedir, filename, sep=""),stringsAsFactors = FALSE)
}

library(xlsx)

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
#' \dontrun{
#' filedir <- "D:/workspace.sim/MELC/CHDS/models/"
#' filename <- "gptotvis.xlsx" ; filename <- "paddhsbetas.xlsx"
#' 
#' loadGLMXLS(filedir, filename)
#' }
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
#' \dontrun{
#' filedir <- "D:/workspace.sim/MELC/CHDS/models/"
#' filename <- "Burt9_10.csv"
#' 
#' loadGLMCSV(filedir , filename)
#' }
loadGLMCSV <- function (filedir, filename) {
	modeldf <- read.csv(paste(filedir, filename, sep=""),stringsAsFactors = FALSE)
	tryCatch(	createGLM(modeldf), 
			error = function(e) stop(paste(filename, e$message), call. = FALSE),
			warning = function(e) stop(paste(filename, e$message), call. = FALSE)
	)
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
#' \dontrun{
#' model <- models$z1homeownLvl1
#' model <- c(coef(models$logit_gp0totvis), coef(models$gptotvis))
#' model <- models.to.show$burt 
#' model <- models$gpmorb
#' model <- models$cond7_8
#' model <- models$welfareLvl1
#' combineMultipleLevels = TRUE; ignoreMultiplicativeTerms = TRUE; directionOnly = TRUE
#' modelVariableCoefs(model) 
#' modelVariableCoefs(model, strip.Lvl = FALSE)
#' }
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
#' @param model
#'   a glm model
#' @param strip.Lvl
#'  strip the LvlX from names. Defaults to TRUE.
#' @return
#'   chr vector of variable names
#' 
#' @examples
#' \dontrun{
#' model <- models$z1accomLvl1
#' modelVariableNames(model) 
#' modelVariableNames(model, strip.Lvl = FALSE)
#' }
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
#' NB: In order to know how many values to predict, there needs to be at least
#' one variable in the model. If you wish to create a intercept only model,
#' then create a model with a single variable and a zero coefficient.
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
#' \dontrun{
#'  model <- model.glm
#'  model <- models$gptotvis3_6 ; set <- c(T, T, rep(F, 1073))
#'  model <- models$gptotvis2
#'  set <- rep(F, 1075)
#'  envir = .GlobalEnv
#'  predict(model, envir, set)
#'  set = NULL
#' }
predict <- function(model, envir = parent.frame(), set = NULL) {
	
	set_all_false <- !is.null(set) && all(!set) 
	if (set_all_false) {
		zero_length_vector <- vector(mode="numeric")
		return(zero_length_vector)
	}
	
	# get vars from model
	vars <- attr(delete.response(terms(model)), "variables")
	
	#evalute vars, return as list
	vars.evaluated <- eval(vars, envir)
	#names(vars.evaluated) <- as.character(vars)[-1]
	
	#convert to matrix 
	vars.evaluated.mx <- as.matrixFromList(vars.evaluated, byrow = F)
	
	#subset
	if (!is.null(set)) {
		vars.evaluated.mx <- vars.evaluated.mx[set, ,drop = F]
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
#'  #model.glm <- models$z1msmokeLvl1
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
#' \dontrun{
#' model.glm <- models$houtptot
#' newdata <- simvalues
#' }
predSimBinom <- function(model.glm, envir=parent.frame(), set = NULL) {
	
	#determine predicted values
	predicted_logits <- predict(model.glm, envir, set)
	predicted_probabilities <- exp(predicted_logits)/(1+exp(predicted_logits))
	
	if(length(predicted_probabilities) == 0) return(predicted_probabilities)
	
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
#' \dontrun{
#'  model.glm <- models$hadmtot
#' 	newdata <- simvalues
#' }
predSimPois <- function(model.glm, envir=parent.frame(), set = NULL) {
	#determine predicted values
	predicted_logs <- predict(model.glm, envir, set)
	predicted_means <- exp(predicted_logs)
	
	if(length(predicted_logs) == 0) return(predicted_logs)
	
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
#' @param alpha
#'  if supplied, use this value for alpha rather than the value in the model
#'  
#' @examples
#' \dontrun{
#' model.glm <- models$houtptot
#' newdata <- simvalues
#' predSimNBinom (model.glm, newdata)
#' }
predSimNBinom <- function(model.glm, envir=parent.frame(), set = NULL, alpha=NULL) {
	
	#determine predicted values
	predicted_logs <- predict(model.glm, envir, set)
	predicted_means <- exp(predicted_logs)
	
	if(length(predicted_logs) == 0) return(predicted_logs)
	
	if (is.null(alpha))	alpha <- model.glm$alpha
	
	if (is.null(alpha)) stop("Missing alpha value")
	
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
#' \dontrun{
#' model.glm <- models$gpprev12
#' newdata <- simvalues
#' model.glm <- model2
#' }
predSimNorm <- function(model.glm, envir=parent.frame(), set = NULL) {
	#determine predicted values
	predicted <- predict(model.glm, envir, set)
	
	if(length(predicted) == 0) return(predicted)
	
	sd <- model.glm$sd
	
	if (is.null(sd)) stop("Model missing sd value")
	
	#simulate
	sapply(predicted, function (x) rnorm(1, mean=x, sd=sd)) 
}

#' Calculate the proportions within groups that 
#' exist in the rows of the matrices in the ZDIM of xa
#' 
#' @param xa
#'  and 3D array, with matrices in the ZDIM.
#' @param numgrps
#'  the num of groups. Each row in the ZDIM is divided into this number of groups.
#'  Proportions are then calculated within these groups. If numgrps = 1, then 
#'  there is only 1 group and proportions are calculated across the whole row.
#' @return
#'  xa as proportions
#' @examples
#' \dontrun{
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
#' codings <- dict.MELC$codings
#' prop.table.grpby.array.zdim(xa,codings)
#' }
prop.table.grpby.array.zdim <- function (xa, numgrps) {
	  
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
#' \dontrun{
#' 
#' result.row <- envs$`Scenario 1`$years1_5$results$means$all$kids["Total",]
#' \dontrun{
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
#' }
#' result.row <- c("0%"=5,"20%"=5,"40%"=9,"60%"=11,"80%"=15,"100%"=50)
#' result.row <- structure(c(5, 5, 5, 5, 5, 5, 9, 9, 9, 11, 11, 11, 15, 15, 15,50.5, 6.02828342338857, 94.9717165766114), .Names = c("0% Mean","0% Lower", "0% Upper", "20% Mean", "20% Lower", "20% Upper","40% Mean", "40% Lower", "40% Upper", "60% Mean", "60% Lower","60% Upper", "80% Mean", "80% Lower", "80% Upper", "100% Mean","100% Lower", "100% Upper"))
#' 
#' result.row <- env.base$years1_5$results$quantiles$kids["Total",]
#' result.row <- envs$`Scenario 1`$years1_5$results$quantiles$kids["Total",]
#' result.row <- envs$`Scenario 1`$years1_5$results$means$all$kids["Total",]
#' 
#' result.as.means.and.errs(result.row)
#' }
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
#' \dontrun{
#' table.catvar(children$SESBTH, codings$SESBTH)
#' x <- simframe$z1singleLvl1 ; coding <- codings$z1singleLvl1
#' table.catvar(simframe$z1singleLvl1, codings$z1singleLvl1)
#' }
table.catvar <- function (x, coding) {
	
	varname <- attr(coding, "varname")
	
	tbl <- prop.table(table(x)) * 100
	
	# match names into codings
	codings.indices <- match(names(tbl), coding)
	names(tbl) <- paste(names(coding)[codings.indices], "(%)")
	
	attr(tbl, "meta") <- c("varname" = varname)
	
	tbl
}

#' Display a vector of continuous values in a table using the
#' breaks supplied.
#' Attachs a meta attribute with varname
#' 
#' @param x
#'  vector of continous values
#' 
#' @param breaks
#' a numeric vector of two or more cut points
#' NB: note that the cut point value is not included in the bin 
#' (ie: include.lowest = FALSE)
#' Therefore the very first cut point must be less than min(x)
#' 
#' @param varname
#'  added as a tag on the meta attribute
#' 
#' @examples
#' \dontrun{
#' x <- env.scenario$simframe$bwkg
#' breaks <- binbreaks$bwkg
#' 
#' table.contvar(env.scenario$simframe$bwkg, binbreaks$bwkg, "bwkg")
#' }
table.contvar <- function (x, breaks, varname) {
	tbl <- prop.table(table(bin(x, breaks, breaklast=NULL), useNA='ifany')) * 100
	attr(tbl, "meta") <- c("varname" = varname)
	tbl
}

cat("Loaded simulate\n")
