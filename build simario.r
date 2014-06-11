# Build package using devtools. From Eclipse/StatET, run using 
# R Console + Additional Console (simario demo dir) to see the
# additional console output.
#
# Before building bump the version number in the DESCRIPTION file.
# 
# Author: oman002
###############################################################################

#' get_version_from_description_file(package_dir)
get_version_from_description_file <- function(package_dir) {
	DESCRIPTION <- file.path(package_dir, "src", "DESCRIPTION")
	description_file_words <- scan(DESCRIPTION, character(0), quiet = TRUE) 
	Version_index <- match("Version:", description_file_words)
	description_file_words[Version_index+1]
}

if_does_not_exist_create_dir <- function (dir) {
	if (!file.exists(dir)) (
					dir.create(dir)
					)
}

package_name <- ".."
package_dir <- "S:/Research Groups/COMPASS/Martin/Workspace" 

build_dir <- file.path(package_dir, "build")
deploy_dir <- file.path(package_dir, "deploy")
man_dir <- file.path(package_dir, "src/man")

if_does_not_exist_create_dir(build_dir)
if_does_not_exist_create_dir(man_dir)

library(devtools)

document(package_name, clean = T)
#document(package_name)

check_doc()

message(package_name, " version ", get_version_from_description_file(package_dir), "\n")

check(package_name, document = FALSE)

cat("Build complete:", build(package_name, build_dir), "\n")

deploy_files <- list.files(deploy_dir, "*.*", full.names = TRUE)

cat("Copying deploy files to", build_dir, "\n")
copy.result <- file.copy(deploy_files, build_dir)
if (all(copy.result)) cat("Copy success\n") else stop("Copy fail")

#install(package_name)
