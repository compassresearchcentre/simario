library(devtools)
library(testthat)

package <- "../../simario/src"

#runs the testthat unit tests found in the inst/tests/
#load_all(package, reset = T)
#test(package)

#auto test - run tests continuously whenever there are changes
auto_test(file.path(package, "R"), file.path(package, "inst/tests"), 
		reporter = "summary")

