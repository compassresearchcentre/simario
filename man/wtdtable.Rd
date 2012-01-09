\name{wtdtable}
\alias{wtdtable}
\title{Weighted frequency table}
\usage{
  wtdtable(x, wgts = rep(1, length(x)))

  wtdtable(x, wgts = rep(1, length(x)))
}
\description{
  x <- c(8,8,2,1,1,8) wgts <- c(1,1,2,1,1,1)

  x <- c(8,8,2,1,1,8) wgts <- c(1,1,2,1,1,1)
}
\details{
  wtdtable(x, wgts)

  1 2 8 2 2 3

  x <- env.base$years1_5$outcomes$z1accomLvl1[,1] grpby <-
  env.base$years1_5$outcomes$z1gender

  table(x, useNA = "ifany") table(x,grpby)

  wtdtable(x, wgts)

  wtdtable(x, wgts)

  1 2 8 2 2 3

  x <- env.base$years1_5$outcomes$z1accomLvl1[,1] grpby <-
  env.base$years1_5$outcomes$z1gender

  table(x, useNA = "ifany") table(x,grpby)

  wtdtable(x, wgts)
}

