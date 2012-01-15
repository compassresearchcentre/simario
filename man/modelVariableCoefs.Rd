\name{modelVariableCoefs}
\alias{modelVariableCoefs}
\title{Return the coefficients used in the supplied model.}
\usage{
  modelVariableCoefs(model, combineMultipleLevels = TRUE,
    ignoreMultiplicativeTerms = TRUE, directionOnly = TRUE)
}
\arguments{
  \item{model}{a glm object with a coef variable, or a
  numeric vector of coefficients}

  \item{combineMultipleLevels}{variables that have multiple
  levels (eg: SESBTHLvl1, SESBTHLvl2) are combined into a
  single variable (eg: SESBTH) by summing the coefficients}

  \item{ignoreMultiplicativeTerms}{remove terms with * in
  them}

  \item{directionOnly}{show "+" or "-" instead of
  coefficent value}
}
\value{
  chr named vector of coefficients
}
\description{
  Return the coefficients used in the supplied model.
}
\examples{
model <- models$z1homeownLvl1
model <- c(coef(models$logit_gp0totvis), coef(models$gptotvis))
model <- models.to.show$burt
model <- models$gpmorb
model <- models$cond7_8
model <- models$welfareLvl1
combineMultipleLevels = TRUE; ignoreMultiplicativeTerms = TRUE; directionOnly = TRUE
modelVariableCoefs(model)
modelVariableCoefs(model, strip.Lvl = FALSE)
}

