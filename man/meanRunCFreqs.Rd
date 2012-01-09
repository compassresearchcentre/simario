\name{meanRunCFreqs}
\alias{meanRunCFreqs}
\title{Create means of continuous frequencies across all runs}
\usage{
  meanRunCFreqs(runs.cfreqs)
}
\arguments{
  \item{runs.cfreqs}{a list of lists. the inner lists
  contain as many elements as their were runs}
}
\description{
  Create means of continuous frequencies across all runs
}
\examples{
runs.cfreqs <- runs$cfreqs
runs.cfreqs <- env.base$years1_5$runs$cfreqs
runs.cfreqs <- runs$summaries
results$cfreqs <- meanRunCFreqs(runs)
}

