\name{loadCodingsXLS}
\alias{loadCodingsXLS}
\title{Load codings xls file and return named list of codings.}
\usage{
  loadCodingsXLS(filedir, filename)
}
\arguments{
  \item{filedir}{file directory, ending with "/", eg:
  "d:/workspace/"}

  \item{filename}{file name, eg: "myfile.xls". The codings
  file must contain 2 columns: Varname = the name of the
  categorical variable Codings_Expr = an expression which
  generates the codings, eg: c("Other"=1, "Pacific"=2,
  "Maori"=3) NB: Codings must be specified in numeric
  order}
}
\description{
  Load codings xls file and return named list of codings.
}
\examples{
filedir = "D:/workspace.sim/MELC/CHDS/base/"
filename = "CHDS data dictionary.xlsx"

codings <- loadCodingsXLS(filedir, filename)
}

