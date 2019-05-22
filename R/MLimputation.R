#' ---
#' title: "MLimputation - Machine learning techniques for imputation""
#' author: "wbau"
#' date: "May 22, 2019"
#' output: github_document
#' ---
#'
#'
## Local function definition: install packages
ipak <- function(pkg) {
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

## This will install all the libraries:
ipak(c("caret"))




## A short introduction to ceret (...)
library(caret)
library(mlbench)
data(Sonar)

set.seed(107)
inTrain <- createDataPartition(
  y = Sonar$Class,
  ## the outcome data are needed
  p = .75,
  ## The percentage of data in the
  ## training set
  list = FALSE
)
## The format of the results

## The output is a set of integers for the rows of Sonar
## that belong in the training set.
str(inTrain)



## Examples
library("simputation")
irisNA <- iris
iris[1:3,1] <- NA
my_model <- lm(Sepal.Length ~ Sepal.Width + Species, data=iris)
impute(irisNA, Sepal.Length ~ my_model)





