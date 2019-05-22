# I would like to consider about the imputation using a Naive Bayes.
############

# preperations
options(warn=-1) # turning "warnings" off
default_par = par() # save default par
# pararell computing options
install.packages("doParallel"); library(doParallel)
cl <- makePSOCKcluster(4); registerDoParallel(cl)

# install the package caret
install.packages("caret"); library("caret")
# the package for Naive Bayes
library("klaR")
library("e1071")
# for kappa coefficients
install.packages("irr");library("irr")

# data with rackings
dat <- iris
dat_r <- dat
dat[c(1,56,100),5] <- NA; dat[1:100,]

####
#If column 5, which values are categorical, is the racking data
a<-5

exp <- dat[,-a]
rsp <- dat[, a]

model <- train(x,y,'nb',
               trControl=trainControl(method='cv',number=10),
               metric = "Kappa")
res<- predict(model$finalModel,exp)
dat[,a][ which(is.na(dat[,a])) ] <- res$class[ which(is.na(dat[,a])) ]


###
#the correct rate
kappa2( cbind(dat[,a],dat_r[,a]) )

#######################
