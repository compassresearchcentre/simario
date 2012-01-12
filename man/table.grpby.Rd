\name{table.grpby}
\alias{table.grpby}
\title{Frequency table, with option to group results.}
\usage{
  table.grpby(x, grpby = NULL)
}
\arguments{
  \item{grpby}{elements to group by, or NULL to do no
  grouping}
}
\value{
  a table. If grpby is specified this will be a 2D table
  with columns that are the group by and rows the
  categories.  If grpby = NULL then a 2D table with 1
  column and rows as categories.
}
\description{
  Frequency table, with option to group results.
}
\examples{
x <- env.base$years1_5$outcomes$z1accomLvl1[,1]
grpby <- env.base$years1_5$outcomes$z1gender
grpby.tag <- "z1gender"


x <- rep(0,1075)
x <- c(8,8,2,1,1,8)
grpby <- c('M','M','F','F','F','F')
grpby.tag <- "gender"

table.grpby(x)
table.grpby(x, grpby, grpby.tag)
}

