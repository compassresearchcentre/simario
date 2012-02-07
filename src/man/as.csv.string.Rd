\name{as.csv.string}
\alias{as.csv.string}
\title{Produces a csv string from x, returned as a length 1 chr vector.}
\usage{
  as.csv.string(x, title = NULL, row.names = T)
}
\arguments{
  \item{x}{object}

  \item{title}{optional title to prepend to output}

  \item{row.names}{either a logical value indicating
  whether the row names of x are to be written along with
  x, or a character vector of row names to be written}
}
\description{
  Produces a csv string from x, returned as a length 1 chr
  vector.
}
\examples{
x <- matrix(c(2,3,4,5,6,7,8,9),nrow=2)
cat(as.csv.string(x))
cat(as.csv.string(x,"title"))	#title <- "title"
str(as.csv.string(x))
length(as.csv.string(x))

#cat(as.csv.string(t.runs.mean.cfreq.prop.base$o.gpprev, "o.gpprev"))
}

