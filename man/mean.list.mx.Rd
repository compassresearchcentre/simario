\name{mean.list.mx}
\alias{mean.list.mx}
\title{Mean applied over a list of matrices/vectors. Aligns matrices/vectors first
so rows and columns match (see align.by.name.list.mx).}
\usage{
  mean.list.mx(listmx)

  mean.list.mx(listmx)
}
\arguments{
  \item{listmx}{a list of matrices}

  \item{listmx}{a list of matrices}
}
\value{
  mean of each matrix element across all list elements

  mean of each matrix element across all list elements
}
\description{
  Mean applied over a list of matrices/vectors. Aligns
  matrices/vectors first so rows and columns match (see
  align.by.name.list.mx).

  Mean applied over a list of matrices/vectors. Aligns
  matrices/vectors first so rows and columns match (see
  align.by.name.list.mx).
}
\examples{
# list of 2 2x3 matrices
listmx1 <- structure(list(`1` = structure(c(646L, 284L, 23L, 12L, 52L, 58L), .Dim = 2:3, .Dimnames = structure(list(c("0", "1"), c("1","2", "3")), .Names = c("", "")), class = "table"), `2` = structure(c(670L,260L, 24L, 11L, 54L, 56L), .Dim = 2:3, .Dimnames = structure(list(    c("0", "1"), c("1", "2", "3")), .Names = c("", "")), class = "table")), .Names = c("1","2"))

# list of vectors
listmx2 <- list(A = c("0"=720L, "1"=353L), B = c("0"=722L, "1"=355L))

# list of 1 vector
listmx3 <- list(A = c("0"=720L, "1"=353L))

# list of 1 2D vector
listmx4 <- structure(c(200L, 400L), .Dim = c(2L, 1L), .Dimnames = structure(list(c("0", "1"), NULL), .Names = c("", "")), class = "table")

# list of 2 1x1 matrices without dimnames
listmx5 <- list(matrix(100), matrix(200))

# list of 2 4x1 matrices with different number of rows
listmx6 <- list(matrix(c(10,20,30,40), dimnames = list(c(1:4), NULL)), matrix(c(20,30,40,50,60), dimnames = list(c(2:6), NULL)))

# list of 2 4x1 matrices with same number of rows but different row names, and no col names
listmx7 <- list(matrix(c(10,20,30,40), dimnames = list(c(1:4), NULL)), matrix(c(20,30,40,50), dimnames = list(c(2:5), NULL)))

# list of 3x2 matrix and 4x2 matrix
listmx8 <- list(matrix(c(10,20,30,40,50,60), nrow = 3, ncol = 2, dimnames = list(c("a","b","c"), c(2:1))), matrix(c(20,30,40,50,60,70,80,90), nrow = 4, ncol = 2, dimnames = list(c("b","d","e","f"), c(2:3))))

#' list of 5x1 table and 4x1 table
listmx9 <-  list(structure(c(1L, 15L, 3L, 27L, 1029L), .Dim = c(5L, 1L), .Dimnames = structure(list(    c("1", "2", "3", "4", "5"), NULL), .Names = c("", "")), class = "table"),    structure(c(11L, 1L, 18L, 1045L), .Dim = c(4L, 1L), .Dimnames = structure(list(        c("2", "3", "4", "5"), NULL), .Names = c("", "")), class = "table"))

#' list of 2 matrices with meta
listmx10 <- list(structure(matrix(100, dimnames = list(1, NULL)), meta=c("grpby.tag"="r1stchildethn")), matrix(c(200,300), dimnames = list(c(1,2), NULL)))

listmx <- listmx1
listmx <- listmx2
listmx <- listmx3
listmx <- listmx4
listmx <- listmx5
listmx <- listmx6
listmx <- listmx7
listmx <- listmx8
listmx <- listmx9
listmx <- listmx10

test <- lapply(list(listmx1,listmx2,listmx3,listmx4, listmx5, listmx6, listmx7, listmx8, listmx9, listmx10), mean.list.mx)

mean.list.mx(listmx)
mean.list.mx(listmx1)
mean.list.mx(listmx2)
mean.list.mx(listmx3)
mean.list.mx(listmx4)
mean.list.mx(listmx5)
mean.list.mx(listmx6)
mean.list.mx(listmx7)
mean.list.mx(listmx8)
mean.list.mx(listmx9)
mean.list.mx(listmx10)
# list of 2 2x3 matrices
listmx1 <- structure(list(`1` = structure(c(646L, 284L, 23L, 12L, 52L, 58L), .Dim = 2:3, .Dimnames = structure(list(c("0", "1"), c("1","2", "3")), .Names = c("", "")), class = "table"), `2` = structure(c(670L,260L, 24L, 11L, 54L, 56L), .Dim = 2:3, .Dimnames = structure(list(    c("0", "1"), c("1", "2", "3")), .Names = c("", "")), class = "table")), .Names = c("1","2"))

# list of vectors
listmx2 <- list(A = c("0"=720L, "1"=353L), B = c("0"=722L, "1"=355L))

# list of 1 vector
listmx3 <- list(A = c("0"=720L, "1"=353L))

# list of 1 2D vector
listmx4 <- structure(c(200L, 400L), .Dim = c(2L, 1L), .Dimnames = structure(list(c("0", "1"), NULL), .Names = c("", "")), class = "table")

# list of 2 1x1 matrices without dimnames
listmx5 <- list(matrix(100), matrix(200))

# list of 2 4x1 matrices with different number of rows
listmx6 <- list(matrix(c(10,20,30,40), dimnames = list(c(1:4), NULL)), matrix(c(20,30,40,50,60), dimnames = list(c(2:6), NULL)))

# list of 2 4x1 matrices with same number of rows but different row names, and no col names
listmx7 <- list(matrix(c(10,20,30,40), dimnames = list(c(1:4), NULL)), matrix(c(20,30,40,50), dimnames = list(c(2:5), NULL)))

# list of 3x2 matrix and 4x2 matrix
listmx8 <- list(matrix(c(10,20,30,40,50,60), nrow = 3, ncol = 2, dimnames = list(c("a","b","c"), c(2:1))), matrix(c(20,30,40,50,60,70,80,90), nrow = 4, ncol = 2, dimnames = list(c("b","d","e","f"), c(2:3))))

#' list of 5x1 table and 4x1 table
listmx9 <-  list(structure(c(1L, 15L, 3L, 27L, 1029L), .Dim = c(5L, 1L), .Dimnames = structure(list(    c("1", "2", "3", "4", "5"), NULL), .Names = c("", "")), class = "table"),    structure(c(11L, 1L, 18L, 1045L), .Dim = c(4L, 1L), .Dimnames = structure(list(        c("2", "3", "4", "5"), NULL), .Names = c("", "")), class = "table"))

#' list of 2 matrices with meta
listmx10 <- list(structure(matrix(100, dimnames = list(1, NULL)), meta=c("grpby.tag"="r1stchildethn")), matrix(c(200,300), dimnames = list(c(1,2), NULL)))

listmx <- listmx1
listmx <- listmx2
listmx <- listmx3
listmx <- listmx4
listmx <- listmx5
listmx <- listmx6
listmx <- listmx7
listmx <- listmx8
listmx <- listmx9
listmx <- listmx10

test <- lapply(list(listmx1,listmx2,listmx3,listmx4, listmx5, listmx6, listmx7, listmx8, listmx9, listmx10), mean.list.mx)

mean.list.mx(listmx)
mean.list.mx(listmx1)
mean.list.mx(listmx2)
mean.list.mx(listmx3)
mean.list.mx(listmx4)
mean.list.mx(listmx5)
mean.list.mx(listmx6)
mean.list.mx(listmx7)
mean.list.mx(listmx8)
mean.list.mx(listmx9)
mean.list.mx(listmx10)
}

