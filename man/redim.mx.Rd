\name{redim.mx}
\alias{redim.mx}
\title{Re-dimension a matrix.
Takes the matrix mx and transforms it into a length(newrows) x length(newcols) matrix
which has the rows from mx specified in newrows and the cols from mx specified in newcols.}
\usage{
  redim.mx(mx, newrows, newcols, dim.names)
}
\arguments{
  \item{mx}{source matrix}

  \item{newrows}{a vector specifing the rows from the
  source matrix that will appear in the new matrix. The new
  matrix will have length(newrows) rows.  The position a
  source row will have in the new matrix is their index in
  newrows.  A NA in this vector will create a row of NAs at
  the position in the new matrix.  eg: c(2,NA,1) will
  create a new matrix of 3 rows.  Row 1 of the new matrix
  will be row 2 of the source matrix Row 2 of the new
  matrix will be NAs.  Row 3 of the new matrix will be row
  1 of the source matrix.}

  \item{newcols}{a vector specifing the cols from the
  source matrix that will appear in the new matrix. Works
  in the same manner as newrows but for columns.}

  \item{dim.names}{A dimnames attribute for the matrix:
  NULL or a list of length 2 giving the row and column
  names respectively.  An empty list is treated as NULL,
  and a list of length one as row names.  The list can be
  named, and the list names will be used as names for the
  dimensions.}
}
\value{
  a new matrix of length(newrows) rows and length(newcols)
  cols with the source rows and cols as specified by
  newrows and newcols.  If the source matrix is a table
  then will also return a table.
}
\description{
  Re-dimension a matrix. Takes the matrix mx and transforms
  it into a length(newrows) x length(newcols) matrix which
  has the rows from mx specified in newrows and the cols
  from mx specified in newcols.
}
\examples{
mx <- matrix(c(10,20,30,40,50,60), nrow = 3, ncol = 2, dimnames = list(c("a","b","c"), c(2:1)))
mx <- as.table(mx)
\dontrun{
> mx
   2  1
a 10 40
b 20 50
c 30 60

newcols <- c(2,1,NA)
newrows <- c(1,2,3,NA,NA,NA)
dim.names <- list(c(LETTERS[1:6]),1:3)

> redim.mx (mx, newrows, newcols, dim.names)
   1  2  3
A 40 10 NA
B 50 20 NA
C 60 30 NA
D NA NA NA
E NA NA NA
F NA NA NA
}
mx <- listmx[[1]]

# mx as a table
mx <- structure(c(1L, 15L, 3L, 27L, 1029L), .Dim = c(5L, 1L), .Dimnames = structure(list(c("1", "2", "3", "4", "5"), NULL), .Names = c("", "")), class = "table")
newrows = 1:5
newcols = 1
dim.names = list(c("1", "2", "3", "4", "5"), NULL)
redim.mx(mx, newrows , newcols, dim.names)

# mx with meta
mx <- structure(matrix(100, dimnames = list(1, NULL)), meta=c("grpby.tag"="r1stchildethn"))
newrows = 1
newcols = 1
dim.names = list(1, NULL)
redim.mx(mx, newrows , newcols, dim.names)
}

