\name{wtdtablecols}
\alias{wtdtablecols}
\title{Produces a weighted frequency distribution for each column
of the matrix mx and returns them altogether in one table.
Each column can have a different set of categories
(ie: frequency "buckets")}
\usage{
  wtdtablecols(mx, wgts = rep(1, nrow(mx)),
    addVariableName = FALSE)
}
\arguments{
  \item{mx}{matrix}

  \item{wgts}{weights. If unspecified, a default weightof 1
  is used for each row}

  \item{addVariableName}{if addVariableName = TRUE, then
  the columns will be given the name of mx}
}
\description{
  Produces a weighted frequency distribution for each
  column of the matrix mx and returns them altogether in
  one table. Each column can have a different set of
  categories (ie: frequency "buckets")
}
\examples{
wgts = rep(1,nrow(mx))
addVariableName = FALSE

mx <- matrix(c(8,2,2,2,8,2,3,2,3,2,2,4,8,2,3,4,2,2,4,3),nrow=4,ncol=5)

wtdtablecols(mx)
}

