\name{summary_mx_grpby_cols}
\alias{summary_mx_grpby_cols}
\title{Execute summary on the columns of a matrix, allowing for weighting and grouping.
work in progress}
\usage{
  summary_mx_grpby_cols(mx, logiset = NULL, wgts = NULL,
    grpby = NULL)
}
\arguments{
  \item{mx}{matrix}

  \item{logiset}{logical vector indicating which rows to
  include, or NULL to include all.}
}
\value{
  returns each column's summary as an element of a list.
  The element of the list consists of a summary in a row
  (or multiple summary rows if the grpby parameter is used
  - one row for each grpby category for the column).
}
\description{
  Execute summary on the columns of a matrix, allowing for
  weighting and grouping. work in progress
}
\examples{
mx <- matrix(c(8,2,2,2,8,2,3,2,3,2,2,4,8,2,3,4,2,2,4,3),nrow=4,ncol=5,dimnames=list(NULL, LETTERS[1:5]))
logiset <- c(FALSE, TRUE, FALSE, TRUE)
mx <- matrix(c(8,2,2,2,8,2,3,2,3,2,2,4,8,2,3,4,2,2,4,3),nrow=4,ncol=5,dimnames=list(NULL, LETTERS[1:5]))
summary_mx_grpby_cols(mx, logiset=logiset)
grpby<-c("M","F","F","M")
wgts<-c(1,2,2,2)
logiset <- c(FALSE, TRUE, TRUE, TRUE)
summary_mx_grpby_cols (mx=mx, logiset=logiset, wgts=wgts,grpby=grpby)
}

