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
ipak(c("caret", "mlbench", "here"))

## A short introduction to caret (...)
set.seed(107)

## Load example datatset and functions
source(here("fun", "MLinputationNum.R"))
data(iris)

## Add some (5%) random missing values to the selected param (numeric)
iris[sample.int(nrow(iris), floor(nrow(iris) * .25)), 1] <- NA
iris[sample.int(nrow(iris), floor(nrow(iris) * .05)), 2] <- NA
iris[sample.int(nrow(iris), floor(nrow(iris) * .05)), 5] <- NA

## Run imputation fun
imputedDb <-
  MLimputationNum(
    data = iris,
    y = iris$Sepal.Length,
    formula = Sepal.Length ~ .,
    training_size = .8,
    method = "pls",
    tuneLength = 15,
    replace = T
  )
