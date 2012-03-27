\name{flatten_mxlists_to_array}
\alias{flatten_mxlists_to_array}
\title{Convert a list of lists of matrices to an array, as follows:
 flatten each matrix into a single row,
 align the single rows within each list, then combine the rows into a single matrix per list
 align each single matrix and combine them into the z dimension of an array}
\usage{
  flatten_mxlists_to_array(lol.mx)
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
  Preserves names and meta attribute.
}
\examples{
#lol.mx <- .$run_results$freqs.by.ethnicity$z1msmokeLvl1
lol.mx <- structure(list(year1 = structure(list(`1` = structure(c(1:6), .Dim = 2:3, .Dimnames = structure(list(0:1,1:3), .Names = c("", "")), class = "table", meta = c(grpby.tag="r1stchildethn")), '2'= structure(c(21:26), .Dim = 2:3, .Dimnames = structure(list(0:1,1:3), .Names = c("", "")), class = "table", meta = c(grpby.tag="r1stchildethn"))), .Names = 1:2), year2 = structure(list(`1` = structure(31:36, .Dim = 2:3, .Dimnames = structure(list(0:1,1:3), .Names = c("", "")), class = "table", meta = c(grpby.tag="r1stchildethn")),     `2` = structure(c(41:46), .Dim = 2:3, .Dimnames = structure(list(0:1,1:3), .Names = c("", "")), class = "table", meta = c(grpby.tag="r1stchildethn"))), .Names = c("1", "2"))))

flatten_mxlists_to_array(lol.mx)
\dontrun{
, , year1

  1 0 1 1 2 0 2 1 3 0 3 1
1   1   2   3   4   5   6
2  21  22  23  24  25  26

, , year2

  1 0 1 1 2 0 2 1 3 0 3 1
1  31  32  33  34  35  36
2  41  42  43  44  45  46
}

lol.mx <- structure(list(year1 = structure(list(`2` = structure(c(1:6), .Dim = 2:3, .Dimnames = structure(list(0:1,1:3), .Names = c("", "")), class = "table", meta = c(grpby.tag="r1stchildethn")), `10`= structure(c(21:26), .Dim = 2:3, .Dimnames = structure(list(0:1,1:3), .Names = c("", "")), class = "table", meta = c(grpby.tag="r1stchildethn"))), .Names = c("2", "10")), year2 = structure(list(`2` = structure(31:36, .Dim = 2:3, .Dimnames = structure(list(0:1,1:3), .Names = c("", "")), class = "table", meta = c(grpby.tag="r1stchildethn")),     `10` = structure(c(41:46), .Dim = 2:3, .Dimnames = structure(list(0:1,1:3), .Names = c("", "")), class = "table", meta = c(grpby.tag="r1stchildethn"))), .Names = c("2", "10"))))
flatten_mxlists_to_array(lol.mx)
}
\seealso{
  align.by.name.list.mx
}

