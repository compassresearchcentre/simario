\name{loadDictionaryXLS}
\alias{loadDictionaryXLS}
\title{Load dictionary xls file and return dictionary vector.}
\usage{
  loadDictionaryXLS(filedir, filename)
}
\arguments{
  \item{filedir}{file directory, ending with "/", eg:
  "d:/workspace/"}

  \item{filename}{file name, eg: "myfile.xls". Must contain
  a column called "Description" and a column called
  "Varname".}
}
\value{
  a vector with values = Description column and names =
  Varname column of xls file.
}
\description{
  Load dictionary xls file and return dictionary vector.
}
\examples{
filedir = "D:/workspace.sim/MELC/CHDS/base/"
filename = "CHDS data dictionary.xlsx"

loadDictionaryXLS(filedir, filename)
}

