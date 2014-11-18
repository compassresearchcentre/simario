# Loading, predicting and simulating from generalised linear models.
# 
# Author: oman002
###############################################################################


#' Create a generalized linear model from a model dataframe that contains 
#' the model variables and their coefficients. The model variables specified
#' here should be present in the simframe.
#' 
#' NB: the variable "_Alpha" will not be added to the model formula
#'     but will be added as the model component m.glm$alpha 
#' NB: the variable "_sd" will not be added to the model formula
#'     but will be added as the model component m.glm$sd 
#' 
#' @param modeldf
#'  model dataframe containing the cols (in any order):
#' "Variable" = variable name. can be an expression like MAGE*MAGE
#' "ClassVal0" = variable level. If present is appended to the variable name, eg: MAGELvl1
#' "Estimate" = variable coeffient
#' 
#' @export
#' @examples 
#' 
#' #m.glm <- createGLM(modeldf) 
createGLM <- function (modeldf) {
	
	if (!"Variable" %in% names(modeldf)) stop ("Model must contain a column named 'Variable'")
	if (!"ClassVal0" %in% names(modeldf)) stop ("Model must contain a column named 'ClassVal0'")
	if (!"Estimate" %in% names(modeldf)) stop ("Model must contain a column named 'Estimate'")
	
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


#' Load and create a GLM from the first sheet in an xlsx file.
#' See \code{\link{createGLM}} for file format.
#'
#' @param filedir
#'  file directory, ending with "/", eg: "d:/workspace/"
#' 
#' @param filename
#'  file name, eg: "myfile.xls". See \code{\link{createGLM}} for file format.
#' 
#' @return 
#'  a glm model object
#'
#' @seealso See \code{\link{createGLM}} for file format.
#' 
#' @export
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
#' See \code{\link{createGLM}} for file format.
#' 
#' @param filedir
#'  file directory, ending with "/", eg: "d:/workspace/"
#' 
#' @param filename
#'  file name, eg: "myfile.csv"
#' 
#' @return 
#'  a glm model object
#' 
#' @seealso See \code{\link{createGLM}} for file format.
#' 
#' @export 
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


#' Get the unique set of names of model coefs for the 
#' supplied list of models
#' 
#' @param models_list
#'  list of models
#' 
#' @return 
#' NULL
#' 
#' @export
#' @examples 
#' \dontrun{
#' 	model_coefs_names_unique(models)
#' }
model_coefs_names_unique <- function(models_list) {
	names.all.coefs <- unlist(lapply(models_list, function(model) {
				names(modelVariableCoefs(model))
			}), use.names = F)

	names.all.coefs.unique <- unique(names.all.coefs)
	
	names.all.coefs.unique.is.previous <- grepl("_previous$", names.all.coefs.unique)
	
	names.all.coefs.unique[!names.all.coefs.unique.is.previous]
}


#' Return the coefficients used in the supplied model.
#' 
#' @param model
#'  a glm object with a coef variable, or a numeric vector of coefficients
#' 
#' @param combineMultipleLevels
#'  variables that have multiple levels (eg: SESBTHLvl1, SESBTHLvl2) 
#'  are combined into a single variable (eg: SESBTH) by summing the 
#'  coefficients
#' 
#' @param ignoreMultiplicativeTerms
#'  remove terms with * in them
#' 
#' @param directionOnly
#'  show "+" or "-" instead of coefficent value
#' 
#' @return
#'   chr named vector of coefficients
#' 
#' @export
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
#' 
#' @param strip.Lvl
#'  strip the LvlX from names. Defaults to TRUE.
#' 
#' @return
#'   chr vector of variable names
#' 
#' @export
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
#'  logical vector indicating elements or rows to keep, or NULL to use
#'  all elements returned by evaluated model variables
#' 
#' @return
#'  a vector of predicted values
#' 
#' @export
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
	if (length(vars)==0) {stop("Model does not contain any variables.  If this is an Intercept only model include a single variable with coefficient equal to 0")}
	
	#evalute vars, return as list
	vars.evaluated <- eval(vars, envir)
	names(vars.evaluated) <- as.character(vars)[-1]
	
	
	#convert to matrix 
	vars.evaluated.mx <- as.matrixFromList(vars.evaluated, byrow = F)
	
	#subset
	if (!is.null(set)) {
		vars.evaluated.mx <- vars.evaluated.mx[set, ,drop = F]
	}
	
	columns.with.NAs <- apply(vars.evaluated.mx, COL, function(x) {any(is.na(x))})
	if (any(columns.with.NAs)) {
		cat("Warning: During predict(), NAs present in", names(columns.with.NAs)[columns.with.NAs], "\n")
	}
	
	#add intercept of 1 
	vars.evaluated.mx <- cbind(Intercept=1, vars.evaluated.mx)
	
	# matrix multiple by model coefficients
	drop(vars.evaluated.mx %*% model$coefficients)
}


predSimNormsSelect <- function(x.cat, models, envir=parent.frame()) {
	x.cat <- as.integer(x.cat)
	result <- rep(NA, length(x.cat))
	for (i in 1:length(models)) {
		select <- x.cat == i
		result[select] <- predSimNorm(models[[i]], envir, set=select)
	}
	result
}

#' Predict probabilities from the coefficients of a logistic regression
#' 
#' @param model.glm
#'  model specifying variables to evaluate and coefficients
#'  to multiple by.
#' @param envir
#'  environment in which to evaluate model variables.
#'  if unspecified, uses caller's environment
#' @param set
#'  logical vector indicating elements or rows to simulate, or NULL to 
#'  simulate using all values in envir
#' 
#' @return 
#' a vector of predicted probabilities
#'  
#' @export   
#' @examples
#' \dontrun{
#' model.glm <- models$houtptot
#' newdata <- simvalues
#' }
predLogistic <- function(model.glm, envir=parent.frame(), set = NULL) {
	
	#determine predicted values
	predicted_logits <- predict(model.glm, envir, set)
	predicted_probabilities <- exp(predicted_logits)/(1+exp(predicted_logits))
	
	predicted_probabilities
}


#' Predict probabilities from the coefficients of a multinomial regression  (currently only works if output catgegories are 1,2,3 etc with 1 being the reference)
#' 
#' @param model.glm.list
#'  List of logit models specifying variables to evaluate and coefficients
#'  to multiple by - each logit model referring to an output category from the multinomial model. IMPORTANT NOTE - only models for the 
#' non-reference output catgegories must be listed, with the logit for the reference group- i.e log(1/1)=0 - being taken care of in the
#'  function itself.
#' Logit models must be listed in order of their output category value - e.g the first model refers to output category=2,
#'  second model refers to output category=3, and so on. (Output category=1 has to be the reference category).
#' @param envir
#'  environment in which to evaluate model variables.
#'  if unspecified, uses caller's environment
#' @param set
#'  logical vector indicating elements or rows to simulate, or NULL to 
#'  simulate using all values in envir
#' 
#' @return 
#' a matrix of predicted probabilities
#'  
#' @export   
#' @examples
#' \dontrun{
#'  model.glm.list<-list(models$GP_multinomial_response_cat2,
#' 			models$GP_multinomial_response_cat3,models$GP_multinomial_response_cat4,
#' 						models$GP_multinomial_response_cat5)
#' set<-still_alive_and_65plus_NOTRES
#' }
predMultinomial <- function(model.glm.list, envir=parent.frame(), set = NULL) {
	
	#determine predicted values
	predicted_logits <- sapply(model.glm.list, function(x) {
				predict(x, envir, set)				
			})
	#note - exp(0) is the predicted exp(logit) for the first category (the refernce category - not yet included in the matrix	
	predicted_probabilities <- exp(predicted_logits)/(exp(0) + rowSums(exp(predicted_logits)))
	
	#getting predicted probabilities with reference category value tagged on as a column at the start of the matrix
	predicted_probabilities2<-cbind(exp(0)/(exp(0) + rowSums(exp(predicted_logits))) ,predicted_probabilities )
	
	predicted_probabilities2
}


#' Predict and simulate value from a multinomial model (currently only works if output catgegories are 1,2,3 etc with 1 being the reference)
#' 
#' @param model.glm.list
#' List of logit models specifying variables to evaluate and coefficients
#'  to multiple by - each logit model referring to an output category from the multinomial model.IMPORTANT NOTE - only models for the 
#' non-reference output catgegories must be listed, with the logit for the reference group- i.e log(1/1)=0 - being taken care of in the
#'  function itself.
#' Logit models must be listed in order of their output category value - e.g the first model refers to output category=2,
#'  second model refers to output category=3, and so on. (Output category=1 has to be the reference category.)
#' @param envir
#'  environment in which to evaluate model variables.
#'  if unspecified, uses caller's environment
#' @param set
#'  logical vector indicating elements or rows to simulate, or NULL to 
#'  simulate using all values in envir
#' 
#' @return 
#' NULL
#'
#' @export
#' @examples
#' \dontrun{}
predSimMultinomial <-function(model.glm.list, envir=parent.frame(), set = NULL) {
		
	probs<-predMultinomial(model.glm.list, envir=envir, set = set)
	
	apply(probs, ROW, function(probabilities) {
					sample(1:length(probabilities), size=1, prob=probabilities)
				
				})
	}
	
		
#' Predict and simulate binary value from binomial
#' distribution with probability
#' 
#' @param model.glm
#'  model specifying variables to evaluate and coefficients
#'  to multiple by.
#' @param envir
#'  environment in which to evaluate model variables.
#'  if unspecified, uses caller's environment
#' @param set
#'  logical vector indicating elements or rows to simulate, or NULL to 
#'  simulate using all values in envir
#' 
#' @return 
#' a vector of binary values drawn from a Binomial distribution 
#'  
#' @export   
#' @examples
#' \dontrun{
#' model.glm <- models$houtptot
#' newdata <- simvalues
#' }
predSimBinom <- function(model.glm, envir=parent.frame(), set = NULL) {
	
	#determine predicted values
	predicted_probabilities <- predLogistic(model.glm, envir, set)
	
	if(length(predicted_probabilities) == 0) return(predicted_probabilities)
	
	#simulate
	sapply(predicted_probabilities , function (x) rbinom(1, size=1, prob=x)) 
}


#' Predict and simulate binary value from 2 binomial models.
#' 
#' @param select
#'  a logical vector, or vector of 0s and 1s, which determine
#'  when to use model0 and when to use model1, i.e:
#'  when select == 0, then result is predSimBinom using model0
#'  when select == 1, then result is 1 - predSimBinom using model1
#' @param model0
#'  model0
#' @param model1
#'  model1
#' @param envir
#'  environment in which to evaluate model variables.
#' 
#' @examples
#' \dontrun{
#' 	select <- z1single_previousLvl1
#' 	model0 <- models$z1singlePrev0 ; model1 <- models$z1singlePrev1
#' 	predSimBinomsSelect(select, model0, model1)
#' }
#envir=.GlobalEnv
predSimBinomSelect <- function(select, model0, model1, envir=parent.frame()) {
	select0 <- select == 0
	select1 <- select == 1
	result <- rep(NA, length(select))
	result[select0] <- predSimBinom(model0, envir, set=select0)
	result[select1] <- predSimBinom(model1, envir, set=select1)
	return(result)
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
#'  logical vector indicating elements or rows to simulate, or NULL to 
#'  simulate using all values in envir
#'
#' @return 
#' a vector of predicted value
#' 
#' @export   
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
#'  logical vector indicating elements or rows to simulate, or NULL to 
#'  simulate using all values in envir
#' @param alpha
#'  if supplied, use this value for alpha rather than the value in the model
#' 
#' @return 
#' a vector of predicted value
#' 
#' @export   
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
#'  logical vector indicating elements or rows to simulate, or NULL to 
#'  simulate using all values in envir
#' 
#' @return 
#' a vector of predicted value
#'
#' @export   
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


#' Predict and simulate value from n normal models.
#' 
#' for the case where each category has a separate model that should be used to simulate a value
#' 
#' @param x.cat
#' a categorical vector
#' @param models
#'  a list of models with length equal to the number of categories in x.cat
#' @param envir
#'  environment in which to evaluate model variables.
#' 
#' @return 
#' a vector of predicted value
#'
#' @export   
#' @examples
#' \dontrun{
#' fhrswrk.cat <- bin(simframe.master$fhrswrk, binbreaks$fhrswrk)
#' test <- predSimNormsSelect(fhrswrk.cat, catToContModels$fhrswrk, envir=simframe.master)
#' }
predSimNormsSelect <- function(x.cat, models, envir=parent.frame()) {
	x.cat <- as.integer(x.cat)
	result <- rep(NA, length(x.cat))
	for (i in 1:length(models)) {
		select <- x.cat == i
		result[select] <- predSimNorm(models[[i]], envir, set=select)
	}
	result
}


#' Predict and simulate value from n normal models with truncation/rounding to ensure simulated 
#' values stay within their category bounds. 
#' 
#' A function based on PredSimNormsSelect with the modification that if any simulated values are 
#' outside the binbreaks for the group, the simulated values are changed to be equal to the
#' boundary value.   Use when all the catToCont models for a variable are normal.
#' 
#' @param x.cat
#' a categorical vector
#' @param models
#'  a list of models with length equal to the number of categories in x.cat
#' @param cont.binbreaks
#' the binbreaks of the categorical variable
#' @param envir
#'  environment in which to evaluate model variables.
#' 
#' @return 
#' a continuous vector that when binned by cont.bonbreaks will be the same as x.cat
#' 
#' @examples
#' \dontrun{
#' x.cont = simframe.master$fhrswrk
#' fhrs.binbreaks = attr(env.scenario$cat.adjustments$fhrswrk, "cont.binbreaks")
#' x.cat <- bin(x.cont, fhrs.binbreaks)
#' desired_props <- rep(1/7, 7)
#' desired_props <-c(.05, .1, .15, .2, .25, .2, .05) 
#' adj.x.cat <- modifyProps(x.cat, desired_props, propen=NULL, accuracy=.01)
#' adj.x.cont <- predSimNormsSelectWithRounding(adj.x.cat, catToContModels$fhrswrk, cont.binbreaks=fhrs.binbreaks, envir=simframe.master)
#' check <- bin(adj.x.cont, fhrs.binbreaks)
#' table(check)
#' table(check)/sum(table(check))
#' }
predSimNormsSelectWithRounding <- function(x.cat, models, cont.binbreaks, envir=parent.frame()) {
	x.cat <- as.integer(x.cat)
	result <- rep(NA, length(x.cat))
	for (i in 1:length(models)) {
		select <- x.cat == i
		result[select] <- predSimNorm(models[[i]], envir, set=select)
		#round so that simulated values outside the category boundaries are set to be at the boundary of the category
		result[select][result[select]<cont.binbreaks[i]+1] <- cont.binbreaks[i]+1
		result[select][result[select]>cont.binbreaks[i+1]] <- cont.binbreaks[i+1]
	}
	result
}


#' Predict and simulate value from n negative binomial models.
#' No backtransformation included.
#' 
#' Use when all the catToCont models for a variable are negative binomial.
#' 
#' @param x.cat
#' a categorical vector
#' @param models
#'  a list of models with length equal to the number of categories in x.cat
#' @param envir
#'  environment in which to evaluate model variables.
#' 
#' @return 
#' a continuous vector that when binned by cont.bonbreaks will be the same as x.cat
#' 
#' @export 
#' @examples
#' \dontrun{}
predSimNBinomsSelect <- function(x.cat, models, envir=parent.frame()) {
	x.cat <- as.integer(x.cat)
	result <- rep(NA, length(x.cat))
	for (i in 1:length(models)) {
		select <- x.cat == i
		result[select] <- predSimNBinom(models[[i]], envir, set=select)
	}
	result
}


#' Predict and simulate value from n models.   
#' 
#' Models should be normal with the option of a negative binomial model for the last model in the 
#' list.  Normal models include the truncation that is described in predSimNormsSelect(). I.e. any 
#' simulated values outside the binbreaks for the group are truncated to the limits for their group.  
#' Values simulated from a negative binomial distribution are backtransformed by adding the start 
#' value of the last cateogory.  E.g. if the last category is 5+, then 5 is added to any values 
#' simulated from a negative binomial distribution.  
#' 
#' @param x.cat
#' a categorical vector
#' @param models
#'  a list of models with length equal to the number of categories in x.cat
#' @param cont.binbreaks
#' the binbreaks of the categorical variable
#' @param logiset
#' logical vector indicating which observations to include, or NULL to include all.
#' 
#' @param envir
#'  environment in which to evaluate model variables.
#' 
#' @return 
#' a continuous vector that when binned by cont.bonbreaks will be the same as x.cat
#'   envir=.GlobalEnv
#' 
#' @export 
#' @examples
#' \dontrun{}
predSimModSelect <- function(x.cat, models, cont.binbreaks, logiset=NULL, envir=parent.frame()) {
	#envir=simframe.master
	x.cat <- as.integer(x.cat)
	if (!is.null(logiset)) {
		padded.x.cat <- rep(NA, length(logiset))
		padded.x.cat[logiset] <- x.cat
	} else {
		padded.x.cat <- x.cat
	}
	result <- rep(NA, length(padded.x.cat))
	for (i in 1:length(models)) {
		select <- padded.x.cat == i
		select[is.na(select)] <- FALSE
		if (length(models[[i]]$sd)==1) {
			result[select] <- predSimNorm(models[[i]], envir, set=select)
			#round so that simulated values outside the category boundaries are set to be at the boundary of the category
			result[select] <- round(result[select])
			result[select][result[select]<cont.binbreaks[i]+1] <- cont.binbreaks[i]+1
			result[select][result[select]>cont.binbreaks[i+1]] <- cont.binbreaks[i+1]
		} else if (length(models[[i]]$alpha)==1) {
			result[select] <- predSimNBinom(models[[i]], envir, set=select)
			#backtransform the simulated negative binomial values
			result[select] <- result[select] + cont.binbreaks[length(cont.binbreaks)-1]+1 
		} else {
			stop("predSimModSelect() currently only implemented for normal and negative binomial models have either an sd or an alpha component")
		}
	}
	if (!is.null(logiset)) {
		return(result[logiset])
	} else{
		return(result)
	}
}


#' Calculates the predicted probabilities (from an ordinal regression model) to be in each
#' cateory for a three or more level categorical variable. 
#' 
#' @param models. 
#' A list of models - one for each category.  The difference in the models will
#' only be the intercept if the model is an ordinal multinomial regression (clogit in SAS).
#' 
#' @param numchildren
#'  number of units
#' 
#' @param envir
#'  environment in which to evaluate model variables.
#' 
#' @param stochatic.  
#' If TRUE adds random variation around the probabilities to be in each 
#' category.  If  TRUE it will cause the probabilities to no longer add to 1 and so should only 
#' be used when the probabilities are being used as propensities in scenario testing.
#' 
#' @return 
#' A matrix of probabilities.  Rows correspond to individual units (children) and
#' columns correspond to the categories of the variable.  The probabilities will add to 1 and be 
#' the exact probabilities estimated from the model if stocahtic=FALSE, otherwise, if 
#' stochatic=TRUE, they will approximate probabilities that will most likely not add to 1 and 
#' could also be negative or greater than 1.
#' 
#' @export 
#' @examples
#' \dontrun{}
predictOrdinal <- function(models, numchildren, envir=parent.frame(), stochastic=FALSE) {
	#number of categories in the outcome is one plus the number of models in the list
	num.cat <- length(models) + 1
	
	#create matrices of the linear predictor (on the log scale) and the probability to be in each 
	#category for each child (Probs).  prob.current.cat.or.less is an intermediate stage in 
	#calculating the probabiliyt to be in each category. 
	LinPreds <- prob.current.cat.or.less <- Probs <- matrix(ncol=num.cat, nrow=numchildren)
	for (i in 1:num.cat) {
		if (i==1) {
			LinPreds[,i] <- predict(models[[i]], envir)
			Probs[,i] <- exp(LinPreds[,i])/(1 + exp(LinPreds[,i]))
			prob.current.cat.or.less[,i] <- Probs[,i]
		} else if (i>1 & i<num.cat) {
			LinPreds[,i] <- predict(models[[i]], envir)
			prob.current.cat.or.less[,i] <- exp(LinPreds[,i])/(1 + exp(LinPreds[,i]))
			Probs[,i] <- prob.current.cat.or.less[,i] - prob.current.cat.or.less[,i-1]
		} else if (i==num.cat) {
			LinPreds[,i] <- rep(1, numchildren)
			prob.current.cat.or.less[,i] <- rep(1, numchildren)
			Probs[,i] <- prob.current.cat.or.less[,i] - prob.current.cat.or.less[,i-1]
		}
	}
	if (stochastic==TRUE) {
		s <- sqrt(Probs*(1 - Probs))
		Probs <- matrix(rnorm(length(Probs), Probs, s), ncol=ncol(Probs), 
				nrow=nrow(Probs), byrow=FALSE)
	}
	return(Probs)
}



