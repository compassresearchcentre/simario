\name{create2CategoryPropensityArray}
\alias{create2CategoryPropensityArray}
\title{Create a propensity array from a dataframe, for a variable with only 2 categories
across multiple iterations.}
\usage{
  create2CategoryPropensityArray(df)
}
\arguments{
  \item{df}{dataframe containing 1 column per iteration.
  This column represents the propensity to change from the
  1st category to the 2nd category.}
}
\value{
  propensity array with rows - the values for each
  individual micro-unit cols - propensity to change from
  the 1st category to the 2nd category. z dim -
  iterations/years
}
\description{
  Create a propensity array from a dataframe, for a
  variable with only 2 categories across multiple
  iterations.
}
\examples{
df <- data.frame(year1 = 1:10/100, year2 = 11:20/100)
create2CategoryPropensityArray(df)
}

