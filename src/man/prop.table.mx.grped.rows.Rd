\name{prop.table.mx.grped.rows}
\alias{prop.table.mx.grped.rows}
\title{Calculates the proportions within row groupings of a flattened matrix.}
\usage{
  prop.table.mx.grped.rows(mx.grped.rows,
    groupnameprefixes)
}
\arguments{
  \item{mx.grped.rows}{a matrix with grped rows, ie: within
  each row there are groups of columns that form a set.
  Proportions are then calculated within these groups.}

  \item{groupnameprefixes}{names of the groups. If NULL
  then no groups.}
}
\value{
  the original matrix but with its values converted to
  proportions.  Preserves names and any "meta" attribute of
  \code{mx.grped.rows)}.
}
\description{
  Calculates the proportions within row groupings of a
  flattened matrix.
}
\examples{
mx.grped.rows  <- structure(matrix(c(1,2,1,3,1,4,2,2,2,3,2,4,1,2,3,4), nrow=2, byrow = TRUE, dimnames=list(NULL, c("Female 1", "Female 2", "Female 3", "Female 4", "Male 1", "Male 2", "Male 3", "Male 4"))), meta=c(varname="disability_state", grpby.tag="sex"))
groupnameprefixes<-c("Female","Male")

mx.grped.rows <- matrix(c(1,2,1,3,1,4,2,2,2,3,2,4), nrow=2, byrow = TRUE)
groupnameprefixes<-NULL

mx.grped.rows <- structure(matrix(c(1,2,1,3,1,4,2,2,2,3,2,4,1,2,3,4), nrow=2, byrow = TRUE, dimnames=list(NULL, c("65-69 No disability", "65-69 Mild disability", "65-69  Moderate disability", "65-69  Severe disability", "70-74  No disability", "70-74 Mild disability", "70-74 Moderate disability", "70-74 Severe disability"))), meta=c(varname="disability_state", grpby.tag="age_grp_output"))
groupnameprefixes<-c("65-69", "70-74", "75-79", "80-84", "85+" )

mx.grped.rows <- structure(matrix(c(1,2,1,3,1,4,2,2,2,3,2,4,1,2,3,4), nrow=2, byrow = TRUE, dimnames=list(NULL, c("Male No disability", "Male Mild disability", "Male Moderate disability", "Male Severe disability", "Female No disability", "Female  Mild disability", "Female Moderate disability", "Female  Severe disability"))), meta=c(varname="disability_state", grpby.tag="sexLvl1"))
groupnameprefixes<-c("Male","Female")

mx.grped.rows <- matrix(c(1:4), nrow=1)
groupnameprefixes<-NULL

prop.table.mx.grped.rows(mx.grped.rows, groupnameprefixes)
}

