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

install_simario_required_packages <- function() {
	package_names <- c("abind", "plyr", "xlsx", "proto", "stringr")
	install.packages.not.installed(package_names)	
}

options(repos = c(CRAN = "http://cran.stat.auckland.ac.nz"))

install_simario_required_packages()