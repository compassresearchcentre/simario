\name{identify_zero_category_cols}
\alias{identify_zero_category_cols}
\title{Identify and return the indices of columns that
are for the zero category. Zero category column
names begin with a "0" or, for flatten column
names, contain " 0".}
\usage{
  identify_zero_category_cols(mx)
}
\arguments{
  \item{mx}{matrix with column names}
}
\value{
  vector of zero column positions
}
\description{
  Identify and return the indices of columns that are for
  the zero category. Zero category column names begin with
  a "0" or, for flatten column names, contain " 0".
}
\examples{
mx <- matrix(1:3, nrow=1, dimnames=list(NULL, c("1","2","3")))
 mx <- matrix(1:3, nrow=1, dimnames=list(NULL, c("0","1","2")))
 mx <- matrix(1:4, nrow=1, dimnames=list(NULL, c("1 0","1 1","2 0", "2 1")))
 identify_zero_category_cols(mx)
}

