\name{wtdmeancols.lbl}
\alias{wtdmeancols.lbl}
\title{Calculates the mean of each column (ie: wtdmeancols) of xlist[[varname]],
names the columns, and labels the result with a meta attribute.}
\usage{
  wtdmeancols.lbl(mx, logiset = NULL, wgts = NULL,
    grpby = NULL, grpby.tag = NULL)

  wtdmeancols.lbl(mx, logiset = NULL, wgts = NULL,
    grpby = NULL, grpby.tag = NULL)
}
\arguments{
  \item{mx}{matrix or dataframe to calculate column means
  of}

  \item{logiset}{logical vector indicating which
  observations to include, or NULL to include all.  Can
  contain a "desc" attribute which describes the set.}

  \item{wgts}{elemets to weight by, or NULL to do no
  weighting}

  \item{grpby}{elements to group by, or NULL or unspecified
  to do no grouping}

  \item{grpby.tag}{if specified this value with be attached
  as the meta attribute "grpby.tag"}

  \item{mx}{matrix or dataframe to calculate column means
  of}

  \item{logiset}{logical vector indicating which
  observations to include, or NULL to include all.  Can
  contain a "desc" attribute which describes the set.}

  \item{wgts}{elemets to weight by, or NULL to do no
  weighting}

  \item{grpby}{elements to group by, or NULL or unspecified
  to do no grouping}

  \item{grpby.tag}{if specified this value with be attached
  as the meta attribute "grpby.tag"}
}
\description{
  If logiset is not null, then xlist is firstly subsetted
  and will only include those obs that have indexed with a
  TRUE value in the logiset logical vector.

  If logiset is not null, then xlist is firstly subsetted
  and will only include those obs that have indexed with a
  TRUE value in the logiset logical vector.
}
\details{
  If grpbycoding is not null, then grouping is applied
  before calculating the mean.

  If grpbycoding is not null, then grouping is applied
  before calculating the mean.
}
\examples{
mx <- env.base$modules$years1_5$outcomes$gptotvis ; mx <- xframe$gptotvis
logiset <- childsets$females
wgts <- NULL; wgts <- children$weightBase ; wgts <- rep(1, nrow(mx))
grpby <- children$r1stchildethn ;  grpby.tag <- "r1stchildethn"
grpby <- children$z1gender ;  grpby.tag <- "z1gender"

wtdmeancols.lbl(mx, logiset=logiset, grpby=grpby, grpby.tag=grpby.tag)
wtdmeancols.lbl(mx, logiset, wgts, grpby, grpby.tag)
mx <- env.base$modules$years1_5$outcomes$gptotvis ; mx <- xframe$gptotvis
logiset <- childsets$females
wgts <- NULL; wgts <- children$weightBase ; wgts <- rep(1, nrow(mx))
grpby <- children$r1stchildethn ;  grpby.tag <- "r1stchildethn"
grpby <- children$z1gender ;  grpby.tag <- "z1gender"

wtdmeancols.lbl(mx, logiset=logiset, grpby=grpby, grpby.tag=grpby.tag)
wtdmeancols.lbl(mx, logiset, wgts, grpby, grpby.tag)
}

