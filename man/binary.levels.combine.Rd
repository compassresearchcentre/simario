\name{binary.levels.combine}
\alias{binary.levels.combine}
\title{Combine levels specified in seperate binary level variables into one
variable.}
\usage{
  binary.levels.combine(..., levelvalues = NULL)
}
\arguments{
  \item{...}{a series of binary (0,1) vectors, specified as
  multiple arguments or as a list}

  \item{levelvalues}{a vector indicating the level values
  to use. The first value of this vector is the value used
  for the first binary vector argument, the second for the
  second binary vector argument etc.  If unspecified, the
  defaults to (1,2,...n) where n is the number of binary
  vectors specified by ...}
}
\value{
  a single vector
}
\description{
  Combine levels specified in seperate binary level
  variables into one variable.
}
\examples{
binlevel1 <- c(1,0,0)
binlevel2 <- c(0,1,0)
binlevel3 <- c(0,0,1)
levelvalues <- c(1,2,3)
binary.levels.combine(binlevel1, binlevel2, binlevel3)   # [1] 1 2 3

binary.levels.combine(list(binlevel1, binlevel2, binlevel3)) # [1] 1 2 3

level1 <- env.scenario$simframe$SESBTHLvl1
level2 <- env.scenario$simframe$SESBTHLvl2
level3 <- env.scenario$simframe$SESBTHLvl3

binary.levels.combine(level1, level2, level3)
binary.levels.combine(list(level1, level2, level3))
levelvalues <- c(1,2,3)
binlevels <- list(level1, level2, level3)
binlevels <- simframe[c("z1chparLvl0", "z1chparLvl1")] ; levelvalues = c(0,1)
binary.levels.combine (binlevels, levelvalues = levelvalues)
}

