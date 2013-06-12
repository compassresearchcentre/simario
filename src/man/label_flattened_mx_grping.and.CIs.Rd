\name{label_flattened_mx_grping.and.CIs}
\alias{label_flattened_mx_grping.and.CIs}
\title{Extension of label_flattened_mx() to label a flattened matrix that contains confidence
intervals.  Can handle grouping or no grouping.
Labels flattened columns according to dictionary codings,
and applies the specified row and col dimension label (if any).}
\usage{
  label_flattened_mx_grping.and.CIs(mx.flattened, dict,
    row.dim.label = "", col.dim.label = "", CI = TRUE,
    num.runs, binbreaks = NULL)
}
\arguments{
  \item{mx.flattened}{a flattened matrix, ie: a matrix that
  has rows that contain groups of values.}

  \item{dict}{dictionary object}

  \item{row.dim.label}{name of the entire row dimension}

  \item{col.dim.label}{name of the entire col dimension}
}
\description{
  Extension of label_flattened_mx() to label a flattened
  matrix that contains confidence intervals.  Can handle
  grouping or no grouping. Labels flattened columns
  according to dictionary codings, and applies the
  specified row and col dimension label (if any).
}

