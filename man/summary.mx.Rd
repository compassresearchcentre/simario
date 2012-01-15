\name{summary.mx}
\alias{summary.mx}
\title{Execute summary on the columns of a matrix.}
\usage{
  summary.mx(mx)
}
\arguments{
  \item{mx}{matrix}
}
\value{
  summary of each column, returned as a row
}
\description{
  Execute summary on the columns of a matrix.
}
\examples{
mxc <- env.base$years6_13$outcomes[["cond"]]
mx <- env.base$years1_5$outcomes[["gptotvis"]]
summary.mx(mxc)
summary.mx(mx)
}

