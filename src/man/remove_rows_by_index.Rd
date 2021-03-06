% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/DataframeUtil.r
\name{remove_rows_by_index}
\alias{remove_rows_by_index}
\title{Remove rows specified by indices}
\usage{
remove_rows_by_index(x, indices)
}
\arguments{
\item{x}{matrix or dataframe}

\item{indices}{integer vector of indices to remove}
}
\value{
a matrix or dataframe after remove rows
}
\description{
Remove rows specified by indices
}
\examples{
x <- data.frame(x = c(1, 2, 3), y = c(0, 10, NA))
remove_rows_by_index(x, c(1,3))
}

