\name{prop.table.grpby.array.zdim}
\alias{prop.table.grpby.array.zdim}
\title{Calculate the proportions within groups that
exist in the rows of the matrics in the ZDIM of xa}
\usage{
  prop.table.grpby.array.zdim(xa, codings,
    grpby.tag = attr(xa, "meta")["grpby.tag"])
}
\arguments{
  \item{xa}{and 3D array, with matrices in the ZDIM.}

  \item{grpby.tag}{specifies a categorical variable with
  codings, or NULL if no groups}
}
\value{
  xa as proportions
}
\description{
  Calculate the proportions within groups that exist in the
  rows of the matrics in the ZDIM of xa
}
\examples{
lol.mx <- env.base$modules$years1_5$runs$freqs$all$z1singleLvl1
lol.mx <- env.base$modules$years1_5$runs$freqs$all.by.ethnicity$z1singleLvl1
lol.mx <- env.base$modules$years1_5$runs$cfreqs$gptotvis

lol.mx <- env.scenario$modules$years1_5$runs$freqs$all$sptype
xa <- flatten.lolmx(lol.mx)

xa <- array(c(1:5, rep(0,5)), dim=c(5,1,2), dimnames=list(LETTERS[1:5],c("Col")))
xa <- array(c(1:5, 9,8,7,6,5), dim=c(5,2,1), dimnames=list(LETTERS[1:5], c("Col 1", "Col 2")))

xa <- lol.mx.array
prop.table.grpby.array.zdim(xa)
}

