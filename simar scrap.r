# TODO: Add comment
# 
# Author: oman002
###############################################################################

install.packages("devtools")
install.packages("roxygen2")

library(devtools)

rmall()


document("simar", clean = T)

document("simar")

as.package("simar")
check("simar")

has_devel()

#test loading as library
load_all("simar")
if ("simar" %in% loadedNamespaces()) unloadNamespace("simar")
if ("package:simar" %in% search()) detach("package:simar", unload = T)
library("simar")
Simenv
simar:::Simenv

#installing
install.packages("s:/Symonds Group/soc/Sociology Research Group/Projects/FRST - MEL-C/deploy/simar_1.0.tar.gz",repos=NULL, type="source")
R CMD INSTALL s:\Symonds Group\soc\Sociology Research Group\Projects\FRST - MEL-C\deploy\simar_1.0.tar.gz
install.packages("d:\\eclipse-rcp-indigo-SR1-win32\\simar_1.0.tar.gz", repos=NULL, type="source")
