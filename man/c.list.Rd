\name{c.list}
\alias{c.list}
\title{Return each element of a list as a new list.}
\usage{
  c.list(xlist, fx)
}
\arguments{
  \item{xlist}{list}

  \item{fx}{function specifing element to return. Should
  take a single element of xlist as a parameter.}
}
\value{
  equivalent to c(fx(xlist[[1]]), fx(xlist[[2]] ... )
  across every element.
}
\description{
  Return each element of a list as a new list.
}
\examples{
xlist <- list('1'=list(inner=list(A1=1:2,A2=2:3)),'2'=list(inner=list(B1=3:4,B2=4:5)))
xlist <- list('1'=list(inner=list(A1=1:2,A2=2:3)))
fx <- function(x) x$inner

c(xlist[[1]]$inner, xlist[[2]]$inner)
c(fx(xlist[[1]]), fx(xlist[[2]]))
c.list(xlist, fx)
all.equal(c(xlist[[1]]$inner, xlist[[2]]$inner),c.list(xlist, fx))
}

