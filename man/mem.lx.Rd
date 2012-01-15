\name{mem.lx}
\alias{mem.lx}
\title{Return amount of memory, in bytes, used by each
element of a list}
\usage{
  mem.lx(lx)
}
\arguments{
  \item{lx}{list}
}
\value{
  num vector of bytes per element
}
\description{
  Return amount of memory, in bytes, used by each element
  of a list
}
\examples{
mem.lx(env.base)
prettyNum(sum(mem.lx(env.base)), big.mark=",")
mem.lx(env.base$years1_5)
}

