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
package_path <- "~/simario/simario_1.0.tar.gz"
package_path <- "D:\\workspace.sim\\simario\\simario_1.0.tar.gz"
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