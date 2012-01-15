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

list(
		default = function(x) {
			file.path("~/workspace", x)
		})

		