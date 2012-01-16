\name{createRunOutputs}
\alias{createRunOutputs}
\title{Creates a set of run outputs.}
\usage{
  createRunOutputs(freqvars, cfreqvars, meanvars,
    freqs.args, means.args)
}
\arguments{
  \item{freqvars}{frequency variable names, or NULL}

  \item{cfreqvars}{continuous frequency variable names, or
  NULL}

  \item{meanvars}{mean variable names, or NULL}

  \item{freqs.args}{frequency variable args, or NULL}

  \item{means.args}{mean variable args, or NULL}
}
\value{
  list(freq = freq, cfreq = cfreq, mean.sets = mean.sets,
  mean.grouped = mean.grouped) each element of the list is
  a list of empty catvar or convar elements, either grouped
  into sets (mean.grouped and mean.sets) or listed
  straight.
}
\description{
  Creates a set of run outputs.
}
\examples{
\dontrun{
catvars <- NULL
convars <- NULL
means.args <- NULL
mean.grouped.spec <- NULL

catvars <- c("msmoke", "fsmoke", "single", "kids", "householdsize", "welfare", "mhrswrk", "fhrswrk", "accom", "homeown", "bedrooms",	"chpar", "chres")
convars <- c("gptotvis", "hadmtot", "houtptot", "gpresp", "gpmorb", "gpprev")

freqs.args <- list( by.ethnicity = list(grpbycoding=codings$r1stchildethn) )
means.args <- list(	all = list(), males = list(logiset=childsets$males),	females = list(logiset=childsets$females),pacific = list(logiset=childsets$pacific),	maori = list(logiset=childsets$maori))

freqvars <- catvars
runs <- createRunOutputs(catvars, convars, means.args, mean.grouped.spec)
}
}

