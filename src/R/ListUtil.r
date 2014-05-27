# List utils.
# 
# Author: oman002
###############################################################################


#' Append the elements of list.src into the corresponding elements of list.dest.
#'
#' NB: attributes of list.dest are replaced by list.src. 
#'  
#' @param list.dest
#'  null or a list of nulls, in which case list.src is
#'  output as a list of lists
#'  a list of elements, or lists, in which case
#'  the elements of list.src are appendend to the
#'  corresponding elements in list.dest 
#' 
#' @param list.src
#'  a list of any element type. Element type may also be a list. 
#'  Each element is appended to the corresponding element of list.dest. 
#' 
#' @param by.name
#'  If FALSE (default) merge so that 
#'  list.dest[[1]] is a list of list.dest[[1]] and list.src[[1]],
#'  list.dest[[2]] is a list of list.dest[[2]] and list.src[[2]], etc...
#'  If TRUE the lists are merged by name rather than by position.
#'  When merging by name lists do not have to have the same 
#'  number of elements and named elements that appear in 
#'  only one of the lists will simply be appended to the result.
#' 
#' @param flatten.src
#'  If FALSE then elements E of list.src will be added as is. If E is a list
#'  it will be added as a list. 
#'  If TRUE then if E is a list its contents will be added, rather than 
#'  as a list.
#' 
#' @return
#'  a list of lists. The inner list contains the
#'  elements supplied by list.src appended to the existing
#'  elements of list.dest.
#' 
#' @export
#' @examples
#' 
#' #list.dest <- results.list; list.src <- results
#' #list.dest <- years1_5$runstats.collated$means
#' #list.src <- years6_13$runstats.collated$means
#' #by.name = TRUE
#' #r <- append.lists(list.dest, list.src, by.name)
#' 
#' #Test first three cases:
#' 
#' #ml0 <- namedList("msmoke", "fsmoke")
#' #x <- env.base$years1_5$outcomes; indices=names(ml0); simplify = FALSE; .FUN=table_mx_cols
#' #list.src <- lapply.subset(x, indices, .FUN)
#'  
#' #ml0 <- namedList("gptotvis", "hadmtot")
#' #x <- env.base$years1_5$outcomes; indices=names(ml0); simplify = FALSE; .FUN=wtdtablecols
#' #list.src <- lapply.subset(x, indices, .FUN)
#' 
#' #ml1 <- append.lists(ml0, list.src)
#' #ml2 <- append.lists(ml1, list.src)
#' #ml3 <- append.lists(ml2, list.src)
#' 
#' #list.dest <- ml0
#' #list.dest <- ml1
#' 
#' #Test with lists with differently named items.
#' 
#' list.dest <- list(foo=matrix(1:4)) # list.dest <- years1_5$run_results_collated$quantiles
#' list.src <- list(bar=matrix(5:10)) # list.src <- years6_13$run_results_collated$quantiles
#' 
#' by.name = TRUE; flatten.src = TRUE
#' list.result <- append.lists(list.dest , list.src, by.name = TRUE, flatten.src = TRUE)
append.lists <- function(list.dest, list.src, by.name = FALSE, flatten.src = FALSE) {
	if (is.null(list.dest) || isListOfNulls(list.dest)) {
		# create a list of lists from list.src
		lapply(list.src, function (xs) { 
					list(xs)
				})
	} else if (is.null(list.src)) {
		list.dest			
	} else {
		if (by.name) {
			# order list by the combined set of names
			elementNames <- unique( c(names(list.dest), names(list.src)) )
			
			list.dest <- list.dest[elementNames]
			list.src <- list.src[elementNames]
			
			names(list.dest) <- elementNames
		} 
		mapply(function(xd,xs) {
					#xd <- list.dest[[1]] ; xs <- list.src[[1]]
					#xd <- list.dest[[2]] ; xs <- list.src[[2]]
					#xd <- list.dest[[7]] ; xs <- list.src[[7]]
					if (is.null(xd)) return(xs)
					if (is.null(xs)) return(xd)
					# if xd is not a list, make it one					
					xd <- if (is.list(xd)) xd else list(xd)
					
					# append xs (as list element) to xd
					if (!flatten.src) xs <- list(xs)
					c(xd,xs)
				}, list.dest, list.src, SIMPLIFY = FALSE)
	}
}


#' Convert list to dataframe simply by setting class attr
#' This preserves any matrices in the list.
#' 
#' @param xlist
#'  list
#' @param row.names
#'  row names for the data.frame
#' 
#' @return
#'  a data frame
#' 
#' @export
#' @examples
#' x <- list(matrix(1:4, nrow=2),c(1:4))
#' x <- as_data_frame_list_as_is(x)
as_data_frame_list_as_is <- function(xlist, row.names=NULL) {
	structure(xlist, 
			class="data.frame", row.names=row.names)
}


#' Check a list for any NAs, producing error if they exist, otherwise
#' silently exit.
#' 
#' @param xlist
#'  list to check
#' 
#' @return 
#'  nothing if no NAs, otherwise error message
#' 
#' @export
#' @examples
#' \dontrun{
#' 	foo <- c(1,NA,3)
#'  names(foo) <- c("a","b","c")
#'  xlist <- foo
#' 
#' 	checkNAs(foo)
#' 	checkNAs(mpropens)
#' }
checkNAs <- function (xlist) {
	
	nas <- sapply(xlist, function(x) { any(is.na(x)) })
	
	if (any(nas)) {
		if (is.null(names(nas))) names(nas) <- paste("[",seq(length(nas)),"]",sep="")
		firstParamName <- as.character(sys.call())[2]
		stop(gettextf("NAs in %s for %s", firstParamName, 
						paste(names(nas[nas]),collapse=", ")))
	}
}


#' Select inner elements of a list of lists, and return as a new list.
#' 
#' @param xlist
#'  list
#' @param fx
#'  function specifing inner elements to select. Should take a single element of xlist
#'  as a parameter.
#' @return
#'  equivalent to c(fx(xlist[[1]]), fx(xlist[[2]] ... ) across every element.
#'  
#' @export
#' @examples 
#' xlist <- list('1'=list(inner=list(A1=1:2,A2=2:3)))
#' xlist <- list('1'=list(inner=list(A1=1:2,A2=2:3)),'2'=list(inner=list(B1=3:4,B2=4:5)))
#' fx <- function(x) x$inner
#' 
#' c(xlist[[1]]$inner, xlist[[2]]$inner)
#' c(fx(xlist[[1]]), fx(xlist[[2]]))
#' c_list(xlist, fx)
#' all.equal(c(xlist[[1]]$inner, xlist[[2]]$inner),c_list(xlist, fx)) 
c_list <- function(xlist, fx) {
	result <- unlist(lapply(xlist, fx), recursive = FALSE)  #do.call(c, lapply(xlist, fx))
	names(result) <- unlist(lapply(xlist, function(x) names(fx(x))))
	result
}


#' True if all list members are NULL
#' 
#' @param xlist
#'  list
#' 
#' @return
#'  a logical value
#' 
#' @export
#' @examples
#' x <- list(NULL)
#' isListOfNulls(x)
isListOfNulls <- function(xlist) {
	all(sapply(xlist, is.null))
}


#' Apply a function to the inner element of a list of lists.
#'
#' @param lol
#'  list of lists
#' 
#' @param .FUN
#'  function that takes an element from lol
#' 
#' @param ...
#'  other arguments to pass to .FUN
#' 
#' @param simplify
#' logical or character string; should the result be simplified to a vector, 
#' matrix or higher dimensional array if possible? See \code{\link{sapply}}.
#' 
#' @param USE.NAMES
#' logical; if TRUE and if X is character, use X as names for the result unless it had names already.
#' See \code{\link{sapply}}.
#' 
#' @return 
#' a list of lists, the result of applying .FUN to lol
#' 
#' @export
#' @examples
#' \dontrun{
#' lol <- env.base$years6_13$runstats$means
#' lol <- env.base$years1_5$runstats$means
#' lol <- env.base$years1_5$runstats$means
#' lol <- env.base$years6_13$runstats$summaries
#' lol <- years1_5$runstats$summaries
#' lapply.inner(lol, mean_array_z)
#' }
lapply.inner <- function (lol, .FUN, ..., simplify = FALSE, USE.NAMES = FALSE) {
	
	lapply(lol, function (outer) {
				#outer <- lol[[1]]
				#x <- lol[[1]][[1]]
				#NB: sapply(*, simplify = FALSE, USE.NAMES = FALSE) is equivalent to lapply(*). 
				sapply(outer, .FUN, ..., simplify = simplify, USE.NAMES = USE.NAMES)
			})
}


#' Takes a list of lists, and combines the corresponding elements of each list
#' into a result list of lists. The first list of the result contain the first elements
#' of the source lists, the second list of the result contains the second elements
#' of the sources lists, and so on.
#' 
#' eg: str(lol)
#' List of 2
#'  $ A:List of 3
#'   ..$ A1: chr "a1"
#'   ..$ A2: chr "a2"
#'   ..$ A3: chr "a3"
#'  $ B:List of 3
#'   ..$ B1: chr "b1"
#'   ..$ B2: chr "b2"
#'   ..$ B3: chr "b3"
#' 
#' The result of zip is:
#' 
#' List of 3
#'  $ A1:List of 2
#'   ..$ A: chr "a1"
#'   ..$ B: chr "b1"
#'  $ A2:List of 2
#'   ..$ A: chr "a2"
#'   ..$ B: chr "b2"
#'  $ A3:List of 2
#'   ..$ A: chr "a3"
#'   ..$ B: chr "b3"
#'
#' zip is the same as the Lisp operation zip:
#' 
#' (zip '(a1 a2 a3) 
#' 		'(b1 b2 b3)
#' 		=> ((a1 b1) (a2 b2) (a3 b3))
#'  
#' @param lol
#'  list of lists. Each inner list should have the same number of elements
#' @return
#'  the corresponding elements of lol combined
#' 
#' @export
#' @examples
#' lol <- list(A=list("A1"="a1","A2"="a2","A3"="a3"),B=list("B1"="b1","B2"="b2","B3"="b3"))
#' lol.zipped <- lzip(lol)
#' #lol.zip.list <- lisp::zip.list(lol$A, lol$B)                 # same as lol.zipped but without names on outer & inner elements
#' lol.mapply <- mapply(list, lol$A, lol$B, SIMPLIFY = FALSE) 		# same as lol.zipped but only has names on outer elements
#' lol.2d <- mapply(list, lol$A, lol$B) 						# creates 2D list [2,3]
#' lol.2ds <- split(lol.2d, col(lol.2d))						# same as lol.zip.list
#' #lol.zwn <- lisp::zip.with.names(lol$A, lol$B)				# same as lol.zipped but only has names on inner elements
lzip <- function(lol) {
	lzipper(lol, c)
}


#' Takes a list of lists, and applies a function to 
#' the combination of each inner element from each outer list.
#' 
#' eg: str(lol)
#' List of 2
#'  $ A:List of 3
#'   ..$ A1: chr "a1"
#'   ..$ A2: chr "a2"
#'   ..$ A3: chr "a3"
#'  $ B:List of 3
#'   ..$ B1: chr "b1"
#'   ..$ B2: chr "b2"
#'   ..$ B3: chr "b3"
#' 
#' .FUN is supplied the elements A1,B1 and then called again with A2,B2, and then A3,B3
#' 
#' The result of zipper(lol, c) is:
#' 
#' List of 3
#'  $ A1:List of 2
#'   ..$ A: chr "a1"
#'   ..$ B: chr "b1"
#'  $ A2:List of 2
#'   ..$ A: chr "a2"
#'   ..$ B: chr "b2"
#'  $ A3:List of 2
#'   ..$ A: chr "a3"
#'   ..$ B: chr "b3"
#'
#' zipper(lol, c) is the same as the Lisp operation zip:
#' 
#' (zip '(a1 a2 a3) 
#' 		'(b1 b2 b3)
#' 		=> ((a1 b1) (a2 b2) (a3 b3))
#'  
#' @param lol
#'  list of lists. Each inner list should have the same number of elements
#' @param .FUN
#'  function 
#' @param ...
#'  additional arguments to .FUN
#' 
#' @return
#'  a list of the results from applying .FUN to each combination
#'  
#' @export
#' @examples
#' lol <- list(A=list("A1"="a1","A2"="a2","A3"="a3"),B=list("B1"="b1","B2"="b2","B3"="b3"))
#' lol.zipped <- lzipper(lol, c)			
#' #lol.zip.list <- lisp::zip.list(lol$A, lol$B)                 # same as lol.zipped but without names on outer & inner elements
#' lol.mapply <- mapply(list, lol$A, lol$B, SIMPLIFY = FALSE) 		# same as lol.zipped but only has names on outer elements
#' lol.2d <- mapply(list, lol$A, lol$B) 						# creates 2D list [2,3]
#' lol.2ds <- split(lol.2d, col(lol.2d))						# same as lol.zip.list
#' #lol.zwn <- lisp::zip.with.names(lol$A, lol$B)				# same as lol.zipped but only has names on inner elements
lzipper <- function (lol, .FUN, ...)  {
	#NB: can be implemented with mapply when no inner element names needed 
	
	# create indices to iterate through
	# set names so output has those names
	indices <- seq(length(lol[[1]]))
	names(indices) <- names(lol[[1]]) 
	
	# NB: see lisp::zip.with.names for a recursive implemention of the lapply below 
	# (which also keeps inner element names)
	
	# for each element j in indices, i.e. lol[[1]]
	lapply(indices, function (j) {
				# j = 1 
				# j = 3
				
				#construct list of all element j's in each outer list
				lol.j <- lapply(lol, function (outer) {
							if (j > length(outer)) {
								stop("List is missing elements")
							}
							outer[[j]]
						})
				
				#apply function
				.FUN(lol.j)
				#.FUN(lol.j, ...)
			})
	
}


#' Executes .FUN on the specified varnames in X and return the results as a list.
#' 
#' @param X
#'  a vector (atomic or list) or an expression object. Other objects (including classed objects) 
#'  will be coerced by base::as.list.
#' @param indices
#'  indices, either numeric or names, of elements in X for which to apply .FUN, or NULL to apply to all elements of X
#' @param .FUN
#'  function
#' @param ...
#'  optional arguments to .FUN.
#' 
#' @return
#'  results of .FUN applied to X as a list
#' 
#' @export
#' @examples
#' 
#' \dontrun{
#' X <- env.base$years1_5$outcomes
#' indices <- names(env.base$years1_5$run_results$confreqs)
#' .FUN <- wtdtablecols
#'
#' X <- env.base$years6_13$outcomes
#' indices <- names(env.base$years6_13$runstats$means$all)
#' .FUN <- summary_mx_cols
#'  
#' 
#' #' lapply.subset(X, indices, .FUN, ...)
#' results.list <-  run_results$freqs.by.ethnicity; X <- outcomes; indices=names(results.list); .FUN=table_mx_cols
#' results <- lapply.subset (X, indices, .FUN, grpby.tag="r1stchildethn")
#' 
#' 
#' lapply.subset (X, indices, .FUN, logiset=logiset, grpby=grpby, grpby.tag = grpby.tag)
#'  
#' results <- lapply.subset (X, indices, .FUN)
#' } 
lapply.subset <- function (X, indices, .FUN, ...) {
	if (is.null(indices)) {
		lapply(X, .FUN, ...)
	} else {
		lapply(X[indices], .FUN, ...)
	}
}


#' Call lapply with a list of arguments to FUN specified as the parameter FUN.args.
#' 
#' @param X
#'  a vector (atomic or list) or an expression object. Other objects (including classed objects) will be coerced by base::as.list.
#' @param FUN 
#'  the function to be applied to each element of xlist
#' @param FUN.args
#'  a named list of arguments to be suppplied to FUN when executed on each element of xlist
#' 
#' @return 
#' NULL
#' 
#' @export
#' @examples
#' mx <- matrix(c(8,2,2,2,8,2,3,2,3,2,2,4,8,2,3,4,2,2,4,3),nrow=4,ncol=5,dimnames=list(NULL, LETTERS[1:5]))
#' grpby <- c('M','F','F','M')
#' logiset <- c(FALSE, TRUE, FALSE, TRUE)
#' result <- table_mx_cols(mx, grpby = grpby, logiset = logiset)
#' 
#' X <- list(mx=mx)
#' FUN <- table_mx_cols
#' FUN.args <- list(grpby = grpby, logiset = logiset)
#' result2 <- lapply.args.as.list(X, FUN, FUN.args)
#' all.equal(result2[[1]], result)
lapply.args.as.list <- function (X, FUN, FUN.args) {
	do.call(lapply,
			args=c(list(
						X=X,
						FUN=FUN)
					,FUN.args))
}


#' Execute a function over a subset of a list, and merges the results into
#' an existing list.
#' 
#' @param results.list
#'  an existing list, eg:
#' 
#' List of 2
#' $ gptotvis: num [1:5, 1, 1] 5.82 4.24 4.09 3.93 4.11
#' 	...
#' $ hadmtot : num [1:5, 1, 1] 0.2344 0.1014 0.1163 0.0847 0.0921
#'  ...
#' 
#' @param X
#'  vector, list or dataframe on which to execute .FUN
#' 
#' @param indices
#'  elements of X to apply .FUN to. If NULL, returns results.list.
#'  Defaults to names(results.list).
#' 
#' @param simplify
#'  if TRUE, then results will be added to the Z dimension of the
#'  matrices in results.list. If FALSE, then results will be added
#'  as a extra list element to each element of results.list 
#' 
#' @param .FUN
#'  .FUN to execute over elements of X
#' 
#' @param ...
#'  additional arguments to .FUN
#' 
#' @return 
#' NULL
#' 
#' @export
#' @examples
#' \dontrun{
#' X <- env.base$years6_13$outcomes
#' results.list <- env.base$years6_13$runstats$summaries
#' lapply.subset.append (results.list, X, .FUN=summary_mx_cols)
#' 
#' X <- env.base$years1_5$outcomes
#' lol <- env.base$years1_5$run_results$confreqs
#'
#' lol <- env.base$years6_13$runstats$summaries
#' X <- env.base$years6_13$outcomes
#' 
#' X <- outcomes
#' .FUN=table_mx_cols
#' 
#' lapply.subset.append (lol, X, simplify = FALSE, .FUN=wtdtablecols)
#' 
#' 
#' lapply.subset.append(results.list=lol.x, X=X, simplify=simplify, .FUN=.FUN, logiset=lol.a$logiset)
#' 
#' results.list <- run_results$freqs; X <- outcomes; indices=names(results.list); simplify = FALSE; .FUN=table_mx_cols
#' lapply.subset.append (run_results$freqs, outcomes, simplify = FALSE, .FUN=table_mx_cols)
#' 	
#' results.list <- run_results$confreqs; X <- outcomes; indices=names(results.list); simplify = FALSE; .FUN=wtdtablecols
#' lapply.subset.append (run_results$confreqs, outcomes, simplify = FALSE, .FUN=wtdtablecols, wgts=outcomes[[wgtsname]])
#' 
#' results.list <- runstats$summaries; X <- outcomes; indices=names(results.list); simplify = FALSE; .FUN=quantile_mx_cols
#' lapply.subset.append (runstats$summaries, outcomes, .FUN=quantile_mx_cols, probs=seq(0.2,1,0.2))
#' 
#' results.list <-  run_results$freqs.by.ethnicity; X <- outcomes; indices=names(results.list); simplify = FALSE; .FUN=table_mx_cols
#' lapply.subset.append (run_results$freqs.by.ethnicity, outcomes, simplify = FALSE, .FUN=table_mx_cols, grpby=outcomes$r1stchildethn, grpby.tag="r1stchildethn")
#' 
#' results.list <- mxlist; X <- xframe ; indices=names(results.list); simplify = TRUE ; .FUN = mean_mx_cols.lbl2
#' lapply.subset.append (results.list, X, indices, simplify, .FUN, logiset=logiset, grpby=grpby, grpby.tag = grpby.tag)
#' }
lapply.subset.append <- function (results.list, X, indices=names(results.list), simplify = TRUE, .FUN, ...) {
	
	if (is.null(indices)) {
		return(results.list)
	}
	
	#results <- lapply.subset(X, indices, .FUN)
	results <- lapply.subset(X, indices, .FUN, ...)
	
	if (simplify) {
		# NB: probably should do a check the dimensions of each results matches
		# dimensions of each results.list
		append.list.mx(results.list, results)	
	} else {
		append.lists(results.list, results)
	}
	
}


#' Apply lapply.subset.append to each element of lol, using the arguments from 
#' the parallel list lol.args and the function .FUN.
#' 
#' @param X
#'   vector, list or dataframe on which to execute .FUN
#' @param lol
#'  a list of lists containing matrix elements
#' @param lol.args
#'  For each list in lol, there is a set of arguments that are supplied to .FUN.
#'  Each matrix in the list will use the arguments from its corresponding lol.args list.
#'  e.g: .FUN will be called on matrices in lol[[1]] with arguments supplied in
#'       lol.args[[1]] 
#'  Defaults to a "args.list" attribute on lol.
#' @param simplify
#'  if TRUE, then results will be added to the Z dimension of the
#'  matrix elements in lol. If FALSE, then results will be added
#'  as a extra list element to each matrix elements of lol 
#' @param .FUN
#'  .FUN to execute over elements of X
#' 
#' @return 
#' NULL
#' 
#' @export
#' @examples
#' \dontrun{
#' X <- outcomes_wtotals
#' lol <- .$runstats$means
#' lol.args <- attr(lol, "args.list")
#' .FUN <- mean_mx_cols
#' 
#' X <- env.base$years1_5$outcomes
#' lol <- env.base$years1_5$runstats$means
#' lol.args <- attr(lol, "args.list")
#' .FUN <- mean_mx_cols.lbl
#' 
#' X <- env.base$modules[[1]]$outcomes
#' lol <- list(all=namedList("gptotvis", "hadmtot"), females.by.ethnicity=namedList("gptotvis", "hadmtot")) 
#' lol.args <- list(all=NULL, females.by.ethnicity=list(logiset=childsets$females, grpby=children$r1stchildethn, grpby.tag="r1stchildethn"))
#' attr(lol, "args.list") <- lol.args
#' .FUN <- mean_mx_cols.lbl
#' }
#' 
#' outcome1 <- structure(matrix(1:8, ncol=2), varname="outcome1")
#' outcome2 <- structure(matrix(11:18, ncol=2), varname="outcome2")
#' X <- list(outcome1=outcome1, outcome2=outcome2)
#' lol <- list(all=namedList("outcome1", "outcome2"), females.by.ethnicity=namedList("outcome1", "outcome2"))
#' lol.args <- list(all=NULL, females.by.ethnicity=list(logiset=c(TRUE,TRUE,TRUE,FALSE), grpby=c("E","E","O","O"), grpby.tag="ethn"))
#' simplify = TRUE
#' attr(lol, "args.list") <- lol.args
#' .FUN <- mean_mx_cols
#'    
#' lapply.subset.append.lol.args(X, lol, lol.args, simplify=simplify, .FUN)
#' mean_mx_cols(outcome1)
#' mean_mx_cols(outcome1, logiset=lol.args$females.by.ethnicity$logiset, grpby=lol.args$females.by.ethnicity$grpby, grpby.tag="ethn")
lapply.subset.append.lol.args <- function(X, lol, lol.args = attr(lol, "args.list"), simplify=TRUE, .FUN) {
	
	result <- mapply(function(lol.x,lol.a) {
				# lol.x <- lol[[1]] ; lol.a <- lol.args[[1]]
				# lol.x <- lol[[2]] ; lol.a <- lol.args[[2]]
				# lol.x <- lol[[6]] ; lol.a <- lol.args[[6]]
				# lol.x <- lol[[10]] ; lol.a <- lol.args[[10]]
				
				# lapply.subset.append(results.list=lol.x, X=X, simplify=simplify, .FUN=.FUN) 
				# lapply.subset.append(results.list=lol.x, X=X, simplify=simplify, .FUN=.FUN, logiset=lol.a$logiset, grpby=lol.a$grpby, grpby.tag=lol.a$grpby.tag)
				do.call(lapply.subset.append, 
						args=c(list(
										results.list=lol.x,
										X=X,
										simplify=simplify,
										.FUN=.FUN), lol.a))
				
			}, lol, lol.args, SIMPLIFY = FALSE)
	
	# preserve arg.list attribute, if any
	structure(result, args.list=attr(lol, "args.list"))
	
}


#' Create a named list of NULL elements.
#' 
#' @param ...
#'  names of the elements
#' 
#' @return 
#' a list of NULL elements
#' 
#' @export
#' @examples 
#' namedList("foo","bar")
namedList <- function(...) {
	elementNames <- c(...)
	
	nlist <- vector("list", length(elementNames))
	names(nlist) <- elementNames
	
	nlist
}


#' Remove named elements from list
#' 
#' @param xlist
#'  list
#' @param names_of_elements_to_remove
#'  character vector
#' 
#' @return 
#' the list after removed
#' 
#' @export
#' @examples
#' xlist <- list(a=1,b=2,c=3,d=4)
#' names_of_elements_to_remove <- c("c","b")
#' names_of_elements_to_remove <- c("e")
#' names_of_elements_to_remove <- c("e","a")
#' remove.elements(xlist, names_of_elements_to_remove)
remove.elements <- function(xlist, names_of_elements_to_remove) {
	matched <- match(names_of_elements_to_remove, names(xlist))
	matched <- matched[!is.na(matched)]
	if (length(matched) == 0) {
		xlist
	} else {
		xlist[-matched]
	}
}

#' Update dest with the values in src, removing any values from dest that don't exist in src.
#' 
#' Use this to update a dest list when the ordering of src is different from dest and you
#' wish to preserve the dest list ordering.
#' 
#' @param dest
#'  dest list
#' @param src
#'  src list
#' 
#' @return 
#' the list "dest" after updated.
#' 
#' @export
#' @examples
#' dest <- list(a=1,b=2,c=3,d=4)
#' src <- list(c=3,a=1,e=5)
#' updatelist(dest, src)
updatelist <- function(dest, src) {
	# get all non nulls
	src <- src[!sapply(src, is.null)]
	
	# get names of vars in dest but not in src
	nD <- length( del <- setdiff(names(dest), (nl <- names(src))) )
	dest[nl] <- src
	
	# if any, remove vars not in src from dest 
	if (nD) { 
		dest[del] <- if (nD == 1) 
					NULL
				else vector("list", nD)
	}
	
	dest
}

