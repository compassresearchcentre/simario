\name{file_extension}
\alias{file_extension}
\title{Returns the file extension, i.e: everything after the final dot.}
\usage{
  file_extension(x)
}
\arguments{
  \item{x}{chacter vector}
}
\value{
  file extension, eg: "csv"
}
\description{
  Returns the file extension, i.e: everything after the
  final dot.
}
\examples{
x <- "data.txt"
x <- "d:/workspace/data old.csv"
x <- "d:/workspace/data.new.xlsx"
file_extension(x)
}

