check_devtools_config_file <- function() {
	
	create_devtools_config_file <- function() {
		config_file_contents <- function() {
			"list(
					default = function(x) {
					file.path('~', x, 'src')
					}
					)"
		}
		
		# NB: ~ expands to Sys.getenv('HOME') and packages will be looked for under ~
		# Sys.getenv('HOME') will be the R_USER environment variable, if specified, 
		# otherwise will be the directory in which R is started
		# When working in Eclipse the R_USER environment variable should 
		# point to the workspace and may be specified via a Run configuration.
		config_file_path <- "~\\.Rpackages"
		cat("Writing", path.expand(config_file_path),"\n")
		cat(config_file_contents(), file=config_file_path)
	}
	
	devtools_config_file_path <- "~\\.Rpackages"
	
	if (!file.exists(devtools_config_file_path)) {
		create_devtools_config_file()
	}
	
}

test_dev_environment <- function() {
	cat("R_USER (home) directory", Sys.getenv("R_USER"),"\n")
	library(devtools)
	
	if(length(as.package("simar"))>1) {
		cat("simar development environment setup")
	}	
}

install.packages.not.installed <- function(package_names) {
	not_already_installed <- function(package_names) {
		setdiff(package_names, row.names(installed.packages()))	
	}
	
	already_installed <- function(package_names) {
		intersect(package_names, row.names(installed.packages()))	
	}
	
	if (length(already_installed(package_names)) > 0) {
		cat("Already installed:", already_installed(package_names),"\n")
	}
	
	if (length(not_already_installed(package_names)) > 0) {
		install.packages(not_already_installed(package_names))
	}
}

install_development_tools <- function() {
	install.packages.not.installed(c("devtools", "roxygen2"))
}

install_development_tools()

check_devtools_config_file()

test_dev_environment()

cat("NB: if you wish to run R CMD CHECK, pdflatex is required. This is available for download from http://miktex.org/\n")