\name{nsort}
\alias{nsort}
\title{Sort numerically.
If x can be represented numerically, then
sort numerically otherwise sort normally.}
\usage{
  nsort(x, stripAlpha = TRUE, ...)
}
\arguments{
  \item{x}{object to sort}

  \item{stripAlpha}{remove alpha characters before
  attempting sort}
}
\description{
  Sort numerically. If x can be represented numerically,
  then sort numerically otherwise sort normally.
}
\examples{
x <- c("1 (\%)", "-2 (\%)", "2 (\%)", "10 (\%)")
x <- c("1 (\%)", "0 (\%)", "2 (\%)", "10 (\%)")
x <- c("1","0","2","10")
x <- c("b","d","a","c")
x <- c(1,4,3,2)
x <- c("1", "2", "3", "4", "5", "6", "7", "8", "10", NA, "9", NA)
nsort(x)
}

