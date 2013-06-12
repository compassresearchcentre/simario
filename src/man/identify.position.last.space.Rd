\name{identify.position.last.space}
\alias{identify.position.last.space}
\title{Identify the position of the last space in each element in a character vector.
Used to identify the last space in the column names in label_flattened_mx_grping.and.CIs().
Returns a list of 2 elements.
The first element, pos.last.space.vec, is a vector identifying the position of the last space
in each element of the input vector.
The second element, num.spaces, states the number of spaces in each element of the input
vector.}
\usage{
  identify.position.last.space(col.names)
}
\arguments{
  \item{col.names}{a character vector.}
}
\description{
  Identify the position of the last space in each element
  in a character vector. Used to identify the last space in
  the column names in label_flattened_mx_grping.and.CIs().
  Returns a list of 2 elements. The first element,
  pos.last.space.vec, is a vector identifying the position
  of the last space in each element of the input vector.
  The second element, num.spaces, states the number of
  spaces in each element of the input vector.
}

