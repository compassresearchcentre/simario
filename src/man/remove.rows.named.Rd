\name{remove.rows.named}
\alias{remove.rows.named}
\title{Remove rows by name.}
\usage{
  remove.rows.named(mx, rnames)
}
\arguments{
  \item{mx}{matrix or dataframe}

  \item{rnames}{vector of rownames}
}
\description{
  Remove rows by name.
}
\examples{
mx <- matrix(1:4, dimnames=list(c("A","NA (\%)"), NULL), nrow = 2)
mx <- matrix(1:4, dimnames=list(c("A","B"), NULL), nrow = 2)
rnames <- "NA (\%)"
remove.rows.named(mx, rnames)
}

