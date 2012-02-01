# TODO: Add comment
# 
# Author: oman002
###############################################################################

install.packages("devtools")
install.packages("roxygen2")

library(devtools)

rmall()
detach("package:simar")

document("simar", clean = T)

document("simar")


check("simar")

has_devel()

#installing
install.packages("s:/Symonds Group/soc/Sociology Research Group/Projects/FRST - MEL-C/deploy/simar_1.0.tar.gz",repos=NULL, type="source")
R CMD INSTALL s:\Symonds Group\soc\Sociology Research Group\Projects\FRST - MEL-C\deploy\simar_1.0.tar.gz
install.packages("d:\\eclipse-rcp-indigo-SR1-win32\\simar_1.0.tar.gz", repos=NULL, type="source")

#.Rpackages
s <- Sys.getenv()
Sys.getenv("HOME")
Sys.setenv("HOME"="d:/workspace.sim")
path.expand("~/.Rpackages")
x <- "simar"
list(
		default = function(x) {
			file.path("d:/workspace.sim", x)
		})


as.packages("simar")
