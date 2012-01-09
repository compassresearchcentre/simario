\name{chart.bar.err}
\alias{chart.bar.err}
\title{Draw a side-by-side bar plot with error bars and a legend to the right
of the plot.}
\usage{
  chart.bar.err(y, y.err, legend.text, col, ...)

  chart.bar.err(y, y.err, legend.text, col, ...)
}
\arguments{
  \item{y}{vector/matrix of y values. Each row of matrix is
  a separate series.}

  \item{y.err}{amount above and below y to draw error bar}

  \item{col}{a vector of colors for the bars or bar
  components. By default, grey is used if height is a
  vector, and a gamma-corrected grey palette if height is a
  matrix.}

  \item{legend.text}{vector of text to appear in legend}

  \item{col}{colours used for bars}

  \item{...}{additional params to pass to barplot}

  \item{y}{vector/matrix of y values. Each row of matrix is
  a separate series.}

  \item{y.err}{amount above and below y to draw error bar}

  \item{col}{a vector of colors for the bars or bar
  components. By default, grey is used if height is a
  vector, and a gamma-corrected grey palette if height is a
  matrix.}

  \item{legend.text}{vector of text to appear in legend}

  \item{col}{colours used for bars}

  \item{...}{additional params to pass to barplot}
}
\description{
  Draw a side-by-side bar plot with error bars and a legend
  to the right of the plot.

  Draw a side-by-side bar plot with error bars and a legend
  to the right of the plot.
}
\examples{
y <- me$means
y.err <- me$errs
col <- c("cornflowerblue", "chocolate1")
legend.text <- c("Base", "Scenario 1")
chart.bar.err(y, y.err, legend.text, col)
y <- me$means
y.err <- me$errs
col <- c("cornflowerblue", "chocolate1")
legend.text <- c("Base", "Scenario 1")
chart.bar.err(y, y.err, legend.text, col)
}

