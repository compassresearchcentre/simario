\name{loadGLMXLS}
\alias{loadGLMXLS}
\title{Load and create a GLM from the first sheet in an xlsx file.}
\usage{
  loadGLMXLS(filedir, filename)
}
\arguments{
  \item{filedir}{file directory, ending with "/", eg:
  "d:/workspace/"}

  \item{filename}{file name, eg: "myfile.xls"}
}
\value{
  a glm model object
}
\description{
  Load and create a GLM from the first sheet in an xlsx
  file.
}
\examples{
filedir <- "D:/workspace.sim/MELC/CHDS/models/"
filename <- "gptotvis.xlsx" ; filename <- "paddhsbetas.xlsx"

loadGLMXLS(filedir, filename)
}

