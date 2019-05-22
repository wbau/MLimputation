# I would like to consider about the imputation using a Naive Bayes.
#


#
# preperations
options(warn=-1) # turning "warnings" off
default_par = par() # save default par
# pararell computing options
install.packages("doParallel"); library(doParallel)
cl <- makePSOCKcluster(4); registerDoParallel(cl)


# install the package caret
install.packages("caret"); library("caret")
# for Naive Bayes
library("klaR")
