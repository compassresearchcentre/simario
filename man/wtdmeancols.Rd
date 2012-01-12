\name{wtdmeancols}
\alias{wtdmeancols}
\title{Calculates the weighted mean for each column of the matrix
optionally subsetting first and grouping by another (equal length) variable.}
\usage{
  wtdmeancols(mx, logiset = NULL, wgts = rep(1, nrow(mx)),
    grpby = NULL, grpby.tag = NULL, na.rm = F)
}
\arguments{
  \item{mx}{matrix or dataframe to calculate column means
  of}

  \item{logiset}{logical vector indicating which
  observations to include, or NULL to include all.}

  \item{wgts}{elemets to weight by, or NULL to do no
  weighting}

  \item{grpby}{elements to group by, or NULL or unspecified
  to do no grouping}
}
\description{
  Calculates the weighted mean for each column of the
  matrix optionally subsetting first and grouping by
  another (equal length) variable.
}
\examples{
mx <- children$o.gptotvis
	wgts <- children$weight
	by <- children$z1gender

wtdmeancols(children$o.gptotvis, children$weight, children$z1gender)
wtdmeancols(children$o.gptotvis, children$weight)

mx <- xframeset[[varname]]
wgts <- xframeset[[wgtsname]]
na.rm = T
wtdmeancols(mx)
wtdmeancols(mx,wgts, grpby)

mx <- matrix (c(1:10), ncol = 2)
mx <- matrix (c(1:3, NA, 5:7, NA, 9:10), ncol = 2) ; logiset=NULL
grpby <- c("A","A","A","B","B") ; grpby.tag = "AB"
grpby = NULL
wgts = rep(1, nrow(mx))
na.rm = T ; na.rm = F
wtdmeancols(mx, logiset=logiset, grpby=grpby, grpby.tag=grpby.tag, na.rm = F)
}

