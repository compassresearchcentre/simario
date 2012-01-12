\name{quantile.mx}
\alias{quantile.mx}
\title{Execute quantile on the columns of a matrix.}
\usage{
  quantile.mx(mx, new.names = NULL, ...)
}
\arguments{
  \item{mx}{matrix}

  \item{new.names}{if specified, the names of the result
  will be set to this vector}
}
\value{
  quantile of each column returned as a row
}
\description{
  Execute quantile on the columns of a matrix.
}
\examples{
mx <- years6_13$outcomes[["cond"]]
mx <- env.base$modules$years1_5$outcomes[["hadmtot"]]
mx <- env.base$modules$years6_13$outcomes[["cond"]]
mx <- env.base$modules$years1_5$outcomes[["gptotvis"]]
quantile.mx(mx)
quantile.mx(mx, probs=seq(0, 1, 0.2), na.rm = TRUE)
quantile.mx(mx, new.names=c("Min", "20th", "40th", "60th","80th","Max"), probs=seq(0, 1, 0.2), na.rm = TRUE)
quantile.mx(mx, probs=seq(0, 1, 0.02), na.rm = TRUE)
}

