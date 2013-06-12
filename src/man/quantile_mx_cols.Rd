\name{quantile_mx_cols}
\alias{quantile_mx_cols}
\title{Execute quantile on the columns of a matrix.
The group-by variable must be time-invariant.}
\usage{
  quantile_mx_cols(mx, new.names = NULL, ...)
}
\arguments{
  \item{mx}{matrix}

  \item{new.names}{if specified, the names of the result
  will be set to this vector}

  \item{...}{additional arguments to pass to
  \code{\link{quantile}}}
}
\value{
  quantile of each column returned as a row
}
\description{
  Execute quantile on the columns of a matrix. The group-by
  variable must be time-invariant.
}
\examples{
\dontrun{
mx <- years6_13$outcomes[["cond"]]
mx <- env.base$modules$years1_5$outcomes[["hadmtot"]]
mx <- env.base$modules$years6_13$outcomes[["cond"]]
mx <- env.base$modules$years1_5$outcomes[["gptotvis"]]
quantile_mx_cols(mx)
quantile_mx_cols(mx, probs=seq(0, 1, 0.2), na.rm = TRUE)
quantile_mx_cols(mx, new.names=c("Min", "20th", "40th", "60th","80th","Max"), probs=seq(0, 1, 0.2), na.rm = TRUE)
quantile_mx_cols(mx, probs=seq(0, 1, 0.02), na.rm = TRUE)
}
}

