\name{lapply.inner.combination}
\alias{lapply.inner.combination}
\title{Takes a list of lists, and applies a function to
the combination of each inner element from each outer list.}
\usage{
  lapply.inner.combination(lol, .FUN, ...)
}
\arguments{
  \item{lol}{list of lists. Each inner list should have the
  same number of elements}

  \item{.FUN}{function}

  \item{...}{additional arguments to .FUN}
}
\value{
  a list of the results from applying .FUN to each
  combination

  eg: List of 3 $ 1: 'table' int [1:2(1d)] 721 354 $ 2:
  'table' int [1:2(1d)] 748 327 $ 3: 'table' int [1:2(1d)]
  756 319
}
\description{
  eg: str(lol) List of 2 $ A:List of 3 ..$ 1: 'table' int
  [1:2(1d)] 721 354 ..$ 2: 'table' int [1:2(1d)] 748 327
  ..$ 3: 'table' int [1:2(1d)] 756 319 $ B:List of 3 ..$ 1:
  'table' int [1:2(1d)] 721 354 ..$ 2: 'table' int
  [1:2(1d)] 736 339 ..$ 3: 'table' int [1:2(1d)] 739 336
}
\details{
  .FUN is supplied the elements A1,B1 and then called again
  with A2,B2, and then A3,B3
}
\examples{
lol <- env.base$years1_5$runs$freqs$all2$z1msmokeLvl1
lol <- env.base$years1_5$runs$freqs$all3$z1msmokeLvl1
lol <- list.var.run.mx[[1]]
lol <- list.var.run.mx$typeofchange
lol.c <- lapply.inner.combination(lol, .FUN=mean.list.mx)
}

