\name{bin}
\alias{bin}
\title{divides x into bins specified by breaks or a bin size}
\usage{
  bin(x, breaks, blabels = names(breaks), breaklast = NULL)
}
\arguments{
  \item{x}{numeric vector to be binned}

  \item{breaks}{either a numeric vector of two or more cut
  points, or a single number giving the size of the bins in
  which case x is divided in bins of size breaks. NB: if
  cut points are specified, note that the cut point value
  is not included in the bin (ie: include.lowest = FALSE)
  Therefore the very first cut point must be less than
  min(x)}

  \item{blabels}{labels for the levels of the resulting
  category. If NULL, labels are constructed using "(a,b]"
  interval notation. If specified, NAs are removed first.
  If unspecified, names(breaks) is used.}

  \item{breaklast}{if breaks is a bin size and breaklast is
  specified then this is the position of the last break}
}
\value{
  the values of x factored into bins
}
\description{
  divides x into bins specified by breaks or a bin size
}
\examples{
table(bin(env.scenario$simframe$bwkg, binbreaks$bwkg))
table(bin(children$bwkg, 0.5)) # x <- children$bwkg; breaks <- 0.5
table(bin(children$ga, 1, breaklast=37)) #x <- children$ga; breaks = 1; breaklast = 37
table(bin(children$pregalc,1,breaklast=7)) #x <- children$pregalc; breaks = 1; breaklast = 7

binbreaks <- list()
binbreaks$pregalc <- c(-1,0,1,2,3,4,5,6,7,max(children$pregalc))
names(binbreaks$pregalc) <- c(NA, 0:7,"8+")
table(bin(children$pregalc,binbreaks$pregalc)); x <- children$pregalc;breaks<-binbreaks$pregalc

binbreaks$bwgrams <- c(0,2499,2999,3499,3999,max(children$bwgrams))
names(binbreaks$bwgrams) <- c(NA, "< 2500", "2500 - 2999", "3000 - 3499", "3500 - 3999", "4000 + ")
table(bin(children$bwgrams,binbreaks$bwgrams))

binbreaks$ga <- c(0,34,35,36,37,max(children$ga))
names(binbreaks$ga) <- c(NA, "< 35", "35", "36", "37", "38+")
table(bin(children$ga,binbreaks$ga))

breaks <- binbreaks$INTERACT
blabels <- names(breaks)
breaklast=NULL
table(bin(x, breaks, breaklast=NULL), useNA="always")
}

