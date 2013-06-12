\name{identify_zero_category_cols_bygrp}
\alias{identify_zero_category_cols_bygrp}
\title{Identify and return the indices of columns that
are for the zero category where grouping has been employed.
Identifies the outcome variable's zero categories but not the grouping variable's zero categories.
Zero category column names begin with a "0" or, for flatten column names, contain " 0".}
\usage{
  identify_zero_category_cols_bygrp(mx)
}
\arguments{
  \item{mx}{matrix with column names}
}
\value{
  vector of zero column positions
}
\description{
  Identify and return the indices of columns that are for
  the zero category where grouping has been employed.
  Identifies the outcome variable's zero categories but not
  the grouping variable's zero categories. Zero category
  column names begin with a "0" or, for flatten column
  names, contain " 0".
}

