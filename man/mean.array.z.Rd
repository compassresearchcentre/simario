\name{mean.array.z}
\alias{mean.array.z}
\title{Mean across Z dimension of a 3D array.
Preserves meta atrribute.}
\usage{
  mean.array.z(xa, CI = TRUE)

  mean.array.z(xa, CI = TRUE)
}
\arguments{
  \item{xa}{an array with a Z dimension}

  \item{CI}{if TRUE, lower and upper confidence intervals
  are also returned}

  \item{xa}{an array with a Z dimension}

  \item{CI}{if TRUE, lower and upper confidence intervals
  are also returned}
}
\value{
  a matrix of means across the Z dimension

  a matrix of means across the Z dimension
}
\description{
  Mean across Z dimension of a 3D array. Preserves meta
  atrribute.

  Mean across Z dimension of a 3D array. Preserves meta
  atrribute.
}
\examples{
xa <- env.base$years1_5$runs$means$all$gpmorb
xa <- env.base$years6_13$runs$means$all$burt
xa <- env.base$years1_5$runs$means$all.by.SESBTH.2cat$gpmorb
xa <- years1_5$runs$means$all.by.gender$gptotvis


xa <- env.base$years1_5$runs$means$all.by.ethnicity$gptotvis
xa <- env.base$years1_5$runs$means$all.by.gender$gptotvis

mean.array.z(xa)
xa <- env.base$years1_5$runs$means$all$gpmorb
xa <- env.base$years6_13$runs$means$all$burt
xa <- env.base$years1_5$runs$means$all.by.SESBTH.2cat$gpmorb
xa <- years1_5$runs$means$all.by.gender$gptotvis


xa <- env.base$years1_5$runs$means$all.by.ethnicity$gptotvis
xa <- env.base$years1_5$runs$means$all.by.gender$gptotvis

mean.array.z(xa)
}

