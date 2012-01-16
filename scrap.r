# TODO: Add comment
# 
# Author: oman002
###############################################################################

install.packages("devtools")
install.packages("roxygen2")

library(devtools)

rmall()
detach("package:simar")

document("simar")



document("simar", clean = T)

check("simar")

has_devel()


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
