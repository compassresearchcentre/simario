\name{flatten.lolmx}
\alias{flatten.lolmx}
\title{Takes a list of lists of matrices and converts to array, by:
 flattens each matrix in a single row,
 aligns then single rows within each list, then combines the rows into a single matrix per list
 then aligns each single matrix and combines them in the z dimension of an array
Preserves names and meta attribute.}
\usage{
  flatten.lolmx(lol.mx)
}
\arguments{
  \item{lol.mx}{a list of lists of matrices, eg: List of 2
  $ year1:List of 2 ..$ 1: 'table' int [1:2, 1:3] 1 2 3 4 5
  6 ..$ 2: 'table' int [1:2, 1:3] 21 22 23 24 25 26 $
  year2:List of 2 ..$ 1: 'table' int [1:2, 1:3] 31 32 33 34
  35 36 ..$ 2: 'table' int [1:2, 1:3] 41 42 43 44 45 46

  NB: matrices can have different dimensions and are
  aligned first within a list, and then between the lists.}
}
\description{
  Takes a list of lists of matrices and converts to array,
  by: flattens each matrix in a single row, aligns then
  single rows within each list, then combines the rows into
  a single matrix per list then aligns each single matrix
  and combines them in the z dimension of an array
  Preserves names and meta attribute.
}
\examples{
lol.mx <- .$runs$freqs$all.by.ethnicity$z1msmokeLvl1
lol.mx <- structure(list(year1 = structure(list(`1` = structure(c(1:6), .Dim = 2:3, .Dimnames = structure(list(0:1,1:3), .Names = c("", "")), class = "table", meta = c(grpby.tag="r1stchildethn")), '2'= structure(c(21:26), .Dim = 2:3, .Dimnames = structure(list(0:1,1:3), .Names = c("", "")), class = "table", meta = c(grpby.tag="r1stchildethn"))), .Names = 1:2), year2 = structure(list(`1` = structure(31:36, .Dim = 2:3, .Dimnames = structure(list(0:1,1:3), .Names = c("", "")), class = "table", meta = c(grpby.tag="r1stchildethn")),     `2` = structure(c(41:46), .Dim = 2:3, .Dimnames = structure(list(0:1,1:3), .Names = c("", "")), class = "table", meta = c(grpby.tag="r1stchildethn"))), .Names = c("1", "2"))))

flatten.lolmx(lol.mx)

, , year1

  1 0 1 1 2 0 2 1 3 0 3 1
1   1   2   3   4   5   6
2  21  22  23  24  25  26

, , year2

  1 0 1 1 2 0 2 1 3 0 3 1
1  31  32  33  34  35  36
2  41  42  43  44  45  46
}
\seealso{
  align.by.name.list.mx
}

