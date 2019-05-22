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

## Add some (30%) random missing values to the 1st param (numeric)
iris[sample.int(nrow(iris), floor(nrow(iris) * .3)), 1] <- NA

## Run imputation fun
imputedDb <-
  MLimputationNum(
    data = iris,
    y = iris$Sepal.Length,
    formula = Sepal.Length ~ .,
    training_size = .8,
    im.method = "pls"
  )
