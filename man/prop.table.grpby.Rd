\name{prop.table.grpby}
\alias{prop.table.grpby}
\title{Produce proportions of x in relation to the sum of group
specified by grpby.}
\usage{
  prop.table.grpby(x, grpby, na.rm = TRUE)
}
\arguments{
  \item{x}{a vector of values}

  \item{grpby}{a vector of indices, the length of x, which
  specify the group each value of x belongs to}

  \item{na.rm}{logical. Should missing values (including
  NaN) be removed?}
}
\value{
  vector of proportions, calculated according to group
}
\description{
  Produce proportions of x in relation to the sum of group
  specified by grpby.
}
\examples{
x <- 1:6
grpby <- c(1,1,2,2,3,3)
grpby <- c('M','M','F','F','F','F')
prop.table.grpby(x, grpby)
}

