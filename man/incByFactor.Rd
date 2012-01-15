\name{incByFactor}
\alias{incByFactor}
\title{increments x by factor}
\usage{
  incByFactor(x, factoredx, factorincrements)
}
\arguments{
  \item{x}{numeric vector to be incremented}

  \item{factoredx}{x factored}

  \item{factorincrements}{amounts to increment each factor
  of factoredx by}
}
\value{
  x incremented by the factor increments
}
\description{
  increments x by factor
}
\examples{
x <- children$bwkg
xi <- incByFactor(children$bwkg, bin(children$bwkg, 0.5), c(0,0.5,0,0,0,0,0,0,0,0))
table(bin(x, 0.5))
table(bin(xi, 0.5))

x <- env.scenario$simframe$INTERACT
factoredx <- bin(x, binbreaks$INTERACT, breaklast=NULL)
factorincrements <- rep(0,9)
r <- incByFactor(x, factoredx, factorincrements)
table(r, useNA="always")
}

