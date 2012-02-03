install.packages.not.installed <- function(package_names) {
	if (length(not_already_installed(package_names)) > 0) {
		install.packages(not_already_installed(package_names))
	}
}

not_already_installed <- function(package_names) {
	setdiff(package_names, row.names(installed.packages()))	
}

check_devtools_config_file_path <- function() {
	
	create_devtools_config_file <- function() {
		config_file_contents <- function() {
			"list(
					default = function(x) {
					file.path('~', x)
					}
					)"
		}
		
		# NB: ~ expands to Sys.getenv('HOME')
		# Sys.getenv('HOME') will be the R_USER environment variable, if specified, 
		# otherwise will be the directory in which R is started
		config_file_path <- "~\\.Rpackages"
		cat("Writing", path.expand(config_file_path),"\n")
		cat(config_file_contents(), file=config_file_path)
	}
	
	devtools_config_file_path <- "~\\.Rpackages"
	
	if (!file.exists(devtools_config_file_path)) {
		create_devtools_config_file()
	}
	
}

check_environment <- function() {
	library(devtools)
	
	if(length(as.package("simar"))>1) {
		cat("simar development environment setup")
	}	
}

# install simar required packages
install.packages.not.installed(c("abind", "plyr", "xlsx", "proto"))

# install development tools
install.packages.not.installed(c("devtools", "roxygen2"))

check_devtools_config_file_path()

check_environment()
