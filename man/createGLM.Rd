\name{createGLM}
\alias{createGLM}
\title{Create a generalized linear model
from a model dataframe that contains the model variables
and their coefficients
NB: the variable "_Alpha" will not be added to the model formula
    but will be added as the model component m.glm$alpha
NB: the variable "_sd" will not be added to the model formula
    but will be added as the model component m.glm$sd}
\usage{
  createGLM(modeldf)
}
\arguments{
  \item{modeldf}{model dataframe containing the rows:
  "Variable" = variable name. can be an expression like
  MAGE*MAGE "ClassVal0" = variable level. If present is
  appended to the variable name, eg: MAGELvl1 "Estimate" =
  variable coeffient}
}
\description{
  Create a generalized linear model from a model dataframe
  that contains the model variables and their coefficients
  NB: the variable "_Alpha" will not be added to the model
  formula but will be added as the model component
  m.glm$alpha NB: the variable "_sd" will not be added to
  the model formula but will be added as the model
  component m.glm$sd
}
\examples{
m.glm <- createGLM(model_msmoke)
}

