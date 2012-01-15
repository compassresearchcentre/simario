\name{loadGLMCSV}
\alias{loadGLMCSV}
\title{Load and create a GLM from a csv file.}
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
  Load and create a GLM from a csv file.
}
\examples{
filedir <- "D:/workspace.sim/MELC/CHDS/models/"
filename <- "Burt9_10.csv"

loadGLMCSV(filedir , filename)
}

