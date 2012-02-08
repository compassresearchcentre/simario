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
package_path <- "~/simar/simar_1.0.tar.gz"
package_path <- "D:\\workspace.sim\\simar\\simar_1.0.tar.gz"
install.packages(package_path, repos=NULL, type="source")

install("simar")
build("simar", "~/simar/build")
reload
build("simar", binary = T)