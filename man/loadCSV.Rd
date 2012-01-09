\name{loadCSV}
\alias{loadCSV}
\title{Helper function that loads a CSV file.}
\usage{
  loadCSV(filedir, filename)
}
\arguments{
  \item{filedir}{file directory, ending with "/", eg:
  "d:/workspace/"}

  \item{filename}{file name, eg: "myfile.csv"}
}
\value{
  a data frame containing the file
}
\description{
  Helper function that loads a CSV file.
}
\examples{
loadCSV(propensityfiledir, "accom_Propens.csv")
}

