\name{remove.elements}
\alias{remove.elements}
\title{Remove named elements from list}
\usage{
  remove.elements(xlist, names_of_elements_to_remove)
}
\arguments{
  \item{xlist}{list}

  \item{names_of_elements_to_remove}{character vector}
}
\description{
  Remove named elements from list
}
\examples{
xlist <- list(a=1,b=2,c=3,d=4)
names_of_elements_to_remove <- c("c","b")
names_of_elements_to_remove <- c("e")
names_of_elements_to_remove <- c("e","a")
remove.elements(xlist, names_of_elements_to_remove)
}

