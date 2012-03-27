\name{summary_mx_cols}
\alias{summary_mx_cols}
\title{Execute summary on the columns of a matrix.}
\usage{
  summary_mx_cols(mx)
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
\dontrun{
mxc <- env.base$years6_13$outcomes[["cond"]]
mx <- env.base$years1_5$outcomes[["gptotvis"]]
summary_mx_cols(mxc)
summary_mx_cols(mx)
}
}

