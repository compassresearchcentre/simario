\name{wtdmeancols}
\alias{wtdmeancols}
\title{Calculates the weighted mean for each column of the matrix
optionally subsetting and grouping by another (equal length) variable.}
\usage{
  wtdmeancols(mx, grpby = NULL, grpby.tag = NULL,
    logiset = NULL, wgts = NULL, na.rm = F)
}
\arguments{
  \item{mx}{matrix or dataframe to calculate column means
  of}

  \item{grpby}{elements to group by, or NULL or unspecified
  to do no grouping.  Same length as the columns of mx.}

  \item{grpby.tag}{added to meta attribute of the result}

  \item{logiset}{logical vector indicating which
  observations to include, or NULL to include all.}

  \item{wgts}{elements to weight by, or NULL to do no
  weighting}

  \item{na.rm}{logical. Should missing values be removed?}
}
\value{
  if grpby is NULL, vector of means for each column if
  grpby is specified, a matrix of means for each column
  grouped
}
\description{
  Calculates the weighted mean for each column of the
  matrix optionally subsetting and grouping by another
  (equal length) variable.
}
\examples{
mx <- matrix (c(1:10), ncol = 2)
mx <- matrix (c(1:3, NA, 5:7, NA, 9:10), ncol = 2) ; logiset=NULL
grpby = NULL
grpby <- c("A","A","A","B","B") ; grpby.tag = "AB"
wgts = rep(1, nrow(mx))
na.rm = FALSE ; na.rm = TRUE

wtdmeancols(mx, logiset=logiset, wgts=wgts, grpby=grpby, grpby.tag=grpby.tag, na.rm = na.rm)
}

