% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/MatrixStats.r
\name{table_mx_cols_MELC}
\alias{table_mx_cols_MELC}
\title{Generates a frequency table, with option to group by, and weight, for each column of a
matrix.  The group-by variable may be time-invariant or time-variant.}
\usage{
table_mx_cols_MELC(mx, grpby = NULL, wgts = NULL, grpby.tag = NULL,
  logiset = NULL, dict)
}
\arguments{
\item{mx}{matrix, or dataframe}

\item{grpby}{a vector or matrix of T/F elements to group by, or NULL or unspecified to do no grouping.
 Same length (or number of columns) as the columns of mx.
If  the group-by variable is time-invariant grpby can be provided as a vector or as a
matrix with every column the same.  If the group-by variable is time-variable then grpby
should be a matrix with number of columns equal to number of years.}

\item{wgts}{a vector or matrix of weights, or NULL or unspecified to do no weighting.
 Same length (or number of columns) as the columns of mx.}

\item{grpby.tag}{a character vector. If specified this value with be attached as the
 meta attribute "grpby.tag"}

\item{logiset}{logical vector or matrix indicating which rows to include, or NULL to include all.}

\item{dict}{the specific project dictionary}
}
\value{
list. Each element of the list is a frequency table for a column in mx.
 If grpby is specified this will be a table with columns that are the group by and rows the categories.
 If grpby = NULL then a table with 1 column and rows as categories is returned.
}
\description{
Generates a frequency table, with option to group by, and weight, for each column of a
matrix.  The group-by variable may be time-invariant or time-variant.
}
\examples{
\dontrun{
mx <- matrix(c(8,2,2,2,8,2,3,2,3,2,2,4,8,2,3,4,2,2,4,3),nrow=4,ncol=5)
grpby <- c('M','F','F','M')
grpby<-matrix(c('M','F','F','M',  'M','M','F','M',  'M','M','M','M',  'F','F','F','F', 'F','F','F','M'),nrow=4,ncol=5)
wgts<-matrix(c(rep(c(1,2,1,1),4),1,3,1,1),nrow=4,ncol=5)
table_mx_cols(mx)
table_mx_cols(mx, grpby)
table_mx_cols(mx, grpby,wgts)
logiset <- c(FALSE, TRUE, FALSE, TRUE)
logiset <- c(0, 1, 0, 1)
logiset <- matrix(data=c(rep(c(FALSE, TRUE, FALSE, TRUE),2),rep(c(TRUE, FALSE, TRUE,FALSE),3)), nrow=4,ncol=5)
logiset <- matrix(data=c(rep(c(0, 1, 0, 1),2),rep(c(1, 0, 1,0),3)), nrow=4,ncol=5)
table_mx_cols_BCASO(mx, grpby = grpby, logiset = logiset)
table_mx_cols_BCASO(mx, grpby = grpby, wgts=wgts, logiset = logiset)}
}

