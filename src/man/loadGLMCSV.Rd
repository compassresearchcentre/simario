\name{loadGLMCSV}
\alias{loadGLMCSV}
\title{Load and create a GLM from a csv file.
See \code{\link{createGLM}} for file format.}
\usage{
  loadGLMCSV(filedir, filename)
}
\arguments{
  \item{filedir}{file directory, ending with "/", eg:
  "d:/workspace/"}

  \item{filename}{file name, eg: "myfile.csv"}
}
\value{
  a glm model object
}
\description{
  Load and create a GLM from a csv file. See
  \code{\link{createGLM}} for file format.
}
\examples{
\dontrun{
filedir <- "D:/workspace.sim/MELC/CHDS/models/"
filename <- "Burt9_10.csv"

loadGLMCSV(filedir , filename)
}
}
\seealso{
  See \code{\link{createGLM}} for file format.
}

