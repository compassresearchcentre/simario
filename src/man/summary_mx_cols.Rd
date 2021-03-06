% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/MatrixStats.r
\name{summary_mx_cols}
\alias{summary_mx_cols}
\title{Execute summary on the columns of a matrix.}
\usage{
summary_mx_cols(mx, logiset = NULL)
}
\arguments{
\item{mx}{matrix}

\item{logiset}{logical vector indicating which rows to include, or NULL to include all.}
}
\value{
summary of each column, returned as a row
}
\description{
Execute summary on the columns of a matrix.
}
\examples{
mx <- matrix(c(8,2,2,2,8,2,3,2,3,2,2,4,8,2,3,4,2,2,4,3),nrow=4,ncol=5,dimnames=list(NULL, LETTERS[1:5]))
logiset <- c(FALSE, TRUE, FALSE, TRUE)
summary_mx_cols(mx)
summary_mx_cols(mx, logiset=logiset)
}

