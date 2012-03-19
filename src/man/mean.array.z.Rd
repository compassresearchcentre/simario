\name{mean.array.z}
\alias{mean.array.z}
\title{Mean across Z dimension of a 3D array.
Preserves meta atrribute.}
\usage{
  mean.array.z(xa, CI = TRUE, NA.as.zero = T)
}
\arguments{
  \item{xa}{an array with a Z dimension}

  \item{CI}{if TRUE, lower and upper confidence intervals
  are also returned}

  \item{NA.as.zero}{if TRUE (default), treat NAs as if they
  are zeros}
}
\value{
  a matrix of means across the Z dimension
}
\description{
  Mean across Z dimension of a 3D array. Preserves meta
  atrribute.
}
\examples{
\dontrun{
xa <- env.base$years1_5$runstats$means$all$gpmorb
xa <- env.base$years6_13$runstats$means$all$burt
xa <- env.base$years1_5$runstats$means$all.by.SESBTH.2cat$gpmorb
xa <- years1_5$runstats$means$all.by.gender$gptotvis

xa <- flatten.lolmx(env.scenario$modules$years6_13$runstats$cfreqs[["cond"]])

xa <- env.base$years1_5$runstats$means$all.by.ethnicity$gptotvis
xa <- env.base$years1_5$runstats$means$all.by.gender$gptotvis

result <- mean.array.z(xa)
}
}

