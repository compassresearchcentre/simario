\name{createOutputMatrix}
\alias{createOutputMatrix}
\title{Create a matrix of NA with specified col/row names/lengths.}
\usage{
  createOutputMatrix(simvarname, rows, cols)
}
\arguments{
  \item{cols}{columns names, or a numeric scalar for the
  number of cols}

  \item{rows}{row names, or a numeric scalar for the number
  of rows}

  \item{simvarname}{simframe source var. stored in the
  "simvar" attribute.  of the matrix. This is the name of
  the variable in the simframe that will be used to fill
  this matrix during simulation}
}
\value{
  a matrix with the "varname" attribute set to simvarname
}
\description{
  Create a matrix of NA with specified col/row
  names/lengths.
}
\examples{
rows <- length(children$z1msmoke1)
cols <- 5
simvarname <- "z1msmokeLvl1"
createOutputMatrix(simvarname, nrows, ncols)
}

