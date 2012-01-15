\name{append.list.mx}
\alias{append.list.mx}
\title{Append two lists of matrices by adding each element of listmx.src into
the corresponding element of listmx.dest in the specified dimension.}
\usage{
  append.list.mx(listmx.dest, listmx.src, into.dim = ZDIM)
}
\arguments{
  \item{listmx.dest}{list of vector/matrix/arrays}

  \item{listmx.src}{list of vector/matrix/arrays}

  \item{into.dim}{the dimension in which to add each
  element of listmx.src.

  If into.dim = ZDIM (i.e: 3) then listmx.src elements will
  be added as an additional z matrix into each matrix/array
  element of listmx.dest. Existing elements of listmx.dest
  will be in the first z dim(s).

  If into.dom = COL (i.e: 2) then listmx.src elements will
  be added as the 2nd col into each vector/matrix element
  of listmx.dest. Existing elements of listmx.dest will be
  in the first column(s).

  If into.dom = ROW (i.e: 1) then listmx.src elements will
  be added as the 2nd row into each vector/matrix element
  of listmx.dest. Existing elements of listmx.dest will be
  in the first row(s).}
}
\description{
  Append two lists of matrices by adding each element of
  listmx.src into the corresponding element of listmx.dest
  in the specified dimension.
}
\examples{
#listmx.dest <- mxlist
#listmx.src <- results
append.list.mx(listmx.dest, listmx.src)

listmx.dest <- list(a=c(1:3), b=c(1:2))
listmx.src <- list(a=c(11:13), b=c(11:12))

into.dim=ROW
result <- append.list.mx(listmx.dest, listmx.src, into.dim)
result <- append.list.mx(result, listmx.src, into.dim)

#listmx.dest <- me.base
#listmx.src <- me.scenario
into.dim=ROW

listmx.dest <- structure(list(means = structure(c(5, 5, 9, 11, 15, 50), .Names = c("0\%","20\%", "40\%", "60\%", "80\%", "100\%")), errs = structure(c(0, 0,0, 0, 0, 0), .Names = c("0\%", "20\%", "40\%", "60\%", "80\%", "100\%"))))
listmx.src <- structure(list(means = structure(c(5, 5, 9, 11, 15, 50.5), .Names = c("0\%","20\%", "40\%", "60\%", "80\%", "100\%")), errs = structure(c(0, 0,0, 0, 0, 44.4717165766114), .Names = c("0\%", "20\%", "40\%", "60\%","80\%", "100\%"))))
append.list.mx(listmx.dest, listmx.src, into.dim)
}

