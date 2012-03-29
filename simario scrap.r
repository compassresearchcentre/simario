# TODO: Add comment
# 
# Author: oman002
###############################################################################

install.packages("devtools")
install.packages("roxygen2")

library(devtools)

rmall()


document("simario", clean = T)

document("simario")

as.package("simario")
check("simario")

has_devel()

#test loading as library
load_all("simario")
if ("simario" %in% loadedNamespaces()) unloadNamespace("simario")
if ("package:simario" %in% search()) detach("package:simario", unload = T)
library("simario")
Simenv
simario:::Simenv

#installing
package_path <- "~/simario/build/simario_1.0.3.tar.gz"
install.packages(package_path, repos=NULL, type="source")

install("simario")

build.dir <- "~/simario/build"
deploy.dir <- "~/simario/deploy"
build("simario", build.dir)

deploy_files <- list.files(deploy.dir, "*.*", full.names = TRUE)
file.copy(deploy_files, build.dir)


reload
build("simario", binary = T)

installed.packages()[match("devtools",row.names(installed.packages())),]
subset(installed.packages(), Package="devtools")

installed.packages()[Package == "devtools"]

save(dict_example, file="~/simario/src/data/dict_example.rda")


result$runstats_spec <- list(
		cfreqs = list(FUN = table.grpby.mx.cols, 
				all = list(varnames = "earnings")),
		freqs = list(FUN = table.grpby.mx.cols,
				all = list(varnames = "disability_state"),
				males = list(varnames = "disability_state", FUN.args=list(logiset=people_sets$males)),
				females = list(varnames = "disability_state", FUN.args=list(logiset=people_sets$females)),
				all.by.gender = list(varnames = "disability_state", FUN.args=list(grpby=people$sex, grpby.tag="sex"))),
		means = list(),
		summaries = list(),
		quantiles = list())

runstats_fun_specs <- list(
		cfreqs = list(all = list(FUN = table.grpby.mx.cols, varnames = "earnings")),
		freqs = list(all = list(FUN = table.grpby.mx.cols, varnames = "disability_state"),
				males = list(FUN = table.grpby.mx.cols, varnames = "disability_state", FUN.args=list(logiset=people_sets$males)),
				females = list(FUN = table.grpby.mx.cols, varnames = "disability_state", FUN.args=list(logiset=people_sets$females)),
				all.by.gender = list(FUN = table.grpby.mx.cols, varnames = "disability_state", FUN.args=list(grpby=people$sex, grpby.tag="sex"))),
		means = list(),
		summaries = list(),
		quantiles = list()
)

runstats_fun_specs2 <- list(
		cfreqs = list(varnames = "earnings"),
		freqs = list(varnames = "disability_state"),
		freqs_males = list(varnames = "disability_state"),
		freqs_females = list(varnames = "disability_state"),
		freqs_by_sex = list(varnames = "disability_state"),
		means = list(varnames = "earnings"),
		means_males = list(varnames = "earnings"),
		means_females = list(varnames = "earnings"),
		means_by_sex = list(varnames = "earnings"),
		summaries = list(varnames = "earnings"),
		quantiles = list(varnames = "earnings")
)

FUN = table.grpby.mx.cols; varnames = "disability_state"; FUN.args=list(grpby=people$sex, grpby.tag="sex")



#' Apply a runstat_spec to outcomes. This applies the function (and args) specified in the runstat_spec,
#' to elements of outcomes specified by runstat_spec$varnames.
#' 
#' @param outcomes
#'  a named list of outcomes
#' @param runstat_spec
#'  a list containing:
#'   varnames - the named elements of outcomes to apply FUN to
#'   FUN - a function to apply to outcomes elements
#'   FUN.args - arguments supplied to FUN
#' @examples
#' outcomes <- env.base$modules$demo$outcomes
#' runstat_spec <- list(FUN = table.grpby.mx.cols, varnames = "disability_state", FUN.args=list(grpby=people$sex, grpby.tag="sex"))
#' lapply_runstat_spec(outcomes, runstat_spec)
lapply_runstat_spec <- function(outcomes, runstat_spec) {
	lapply.args.as.list(outcomes[runstat_spec$varnames], runstat_spec$FUN, runstat_spec$FUN.args)
}

FUN = table.grpby.mx.cols; varnames = "disability_state"; FUN.args=list(grpby=people$sex, grpby.tag="sex")
freqs_by_sex <- create_runstat_spec(varnames, FUN, FUN.args)
create_runstat_spec <- function(varnames, FUN, FUN.args) {
	apply <- function(outcomes) {
		lapply.args.as.list(outcomes[varnames], FUN, FUN.args)
	}
}

FUN = table.grpby.mx.cols; varnames = "disability_state"; FUN.args=list(grpby=people$sex, grpby.tag="sex")
freqs_by_sex <- create_runstat_generator(FUN, FUN.args)
freqs_by_sex <- create_runstat_generator(table.grpby.mx.cols, list(grpby=people$sex, grpby.tag="sex"))

freqs_by_sex <- create_runstat_generator(table.grpby.mx.cols, grpby=people$sex, grpby.tag="sex")
create_runstat_generator <- function(FUN, ...) {
	function(outcomes, varnames) {
		lapply(outcomes[varnames], FUN, ...)
	}
}

freqs_collator1 <- Curry("mean.array.z", CI = F) %c% Curry("finialise.lolmx", dict = dict_demo)
freqs_collator2 <- function(runstat) { mean.array.z(finialise.lolmx(runstat, dict = dict_demo), CI=F) }
freqs_collator3 <- function(runstat) { runstat_f = finialise.lolmx(runstat, dict = dict_demo); mean.array.z(runstat_f, CI=F) }


freqs_collator4 <- create_runstat_collator(mean.array.z(finialise.lolmx, dict = simenv$dict)

fg <- compose(mean.array.z(squash_args(finialise.lolmx, dict = simenv$dict)))



result <- freqs_by_sex(outcomes, "disability_state")
