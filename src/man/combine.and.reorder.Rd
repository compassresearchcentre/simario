\name{combine.and.reorder}
\alias{combine.and.reorder}
\title{Combines and reorders (so correct original order) after modifyProps has been called on
a logiset.
Called in adjust.proportions().}
\usage{
  combine.and.reorder(modified.x, non.modified.x, logiset)
}
\arguments{
  \item{modified.x}{A vector of modified values (i.e. those
  tht were in the logiset.  Either categorical or
  continuous.}

  \item{non.modified.x}{A vector of non-modified values of
  the same variable (i.e. those tht were not in the
  logiset.  Either categorical or continuous.}

  \item{logiset}{A TRUE/FALSE vetor defining which units
  are in the logical subset on which modifyProps was
  called.}
}
\value{
  A combined and reordered vector.  Contains evereyone in
  the population in the correct order.
}
\description{
  Combines and reorders (so correct original order) after
  modifyProps has been called on a logiset. Called in
  adjust.proportions().
}

