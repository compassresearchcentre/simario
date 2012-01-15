\name{chart.2series.bar.err}
\alias{chart.2series.bar.err}
\title{Draw a 2-series (base & scenario) side-by-side bar graph with
error bars on the current device.}
\usage{
  chart.2series.bar.err(title, xlab, ylab, result.row.base,
    result.row.scenario, scenario.name)
}
\arguments{
  \item{title}{chart title. Also used in navigator node
  name.}

  \item{xlab}{x axis label.}

  \item{ylab}{y axis label}

  \item{result.row.base}{the base result row, ie: a vector
  with values named Mean and Lower eg:

  > envs$`Scenario
  1`$years1_5$results$means$all$kids["Total",] Mean Lower
  Upper 10.99488 10.62256 11.36721

  if there are no values named Mean, then it will be
  assumed that all values are Means and that Lower is 0.}

  \item{result.row.scenario}{the scenario result row,
  formatted the same as result.row.base}

  \item{scenario.name}{name of scenario. Used to great
  navigator node path and label series.}
}
\description{
  Draw a 2-series (base & scenario) side-by-side bar graph
  with error bars on the current device.
}
\seealso{
  result.as.means.and.errs

  varname <- "kids" result.row.base <-
  env.base$modules$years1_5$results$means$all.by.ethnicity[[varname]]["All
  Years",] result.row.scenario <- envs$`Scenario
  1`$modules$years1_5$results$means$all.by.ethnicity[[varname]]["All
  Years",] scenario.name <- "Scenario 1" title <-
  paste(dictLookup(varname), "all years") xlab="Ethnicity"
  ylab=paste("Mean")

  varname <- "cond" result.row.base <-
  env.base$modules$years6_13$results$means$all.by.SESBTH[[varname]]["All
  Years",] result.row.scenario <- envs$`Scenario
  1`$modules$years6_13$results$means$all.by.SESBTH[[varname]]["All
  Years",] scenario.name <- "Scenario 1" title <-
  paste(dictLookup(varname), "all years") xlab="SES at
  Birth" ylab=paste("Mean")

  varname <- "gptotvis" result.row.base <-
  env.base$modules$years1_5$results$histo[[varname]]
  result.row.scenario <- envs$`Scenario
  1`$modules$years1_5$results$histo[[varname]]
  scenario.name <- "Scenario 1" title <-
  paste(dictLookup(varname), "all years") xlab="Value"
  ylab="Freq"

  varname <- "cond" result.row.base <-
  env.base$modules$years6_13$results$histo[[varname]]["All
  Years",] result.row.scenario <- envs$`Scenario
  1`$modules$years6_13$results$histo[[varname]]["All
  Years",] scenario.name <- "Scenario 1" title <-
  paste(dictLookup(varname), "all years") xlab="Value"
  ylab="Freq"

  chart.2series.bar.err(title, xlab, ylab, result.row.base,
  result.row.scenario, scenario.name)
}

