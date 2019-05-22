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


####
#If column 5, which values are categorical, is the racking data
a<-5

# data with rackings
data(iris); dat <- iris; dat_r <- dat
dat[c(1,2,55,56,99,100),a] <- NA; dat[1:100,]

# listwise approach
exp <- dat[!is.na(dat[,a]),-a]; rsp <- dat[!is.na(dat[,a]),a]
# leave it to the processing in the caret
exp <- dat[,-a]; rsp <- dat[,a]

model <- train(exp,rsp,
               'nb',
               trControl=trainControl(method='cv',number=10),
               metric = "Kappa")
res<- predict(model$finalModel,exp)
dat[ is.na(dat[,a]),a ] <- res$class[ is.na(dat[,a]) ]

###
#the correct rate
kappa2( cbind(dat[,a],dat_r[,a]) )

#######################

#######################

#######################
iris[sample.int(nrow(iris), floor(nrow(iris) * .05)), 5] <- NA


MLimputationNum <- function(data,
                            y,
                            formula,
                            training_size = .8,
                            method = "pls",
                            tuneLength = 15,
                            replace = T) {
  
  # calculate percentage of missing values
  m.perc <- sum(complete.cases(data)/nrow(data))
  # display worning if over some limit?
  warning(paste0((1-m.perc)*100,"% of outcome variable is missing!"))
  # get missing values index
  missing.idx <- is.na(y)
  # split into missing and complete cases
  c.dat <- data[!missing.idx, ]
  m.dat <- data[missing.idx, ]
  # create data partition
  inTrain <- createDataPartition(y = y[!missing.idx],
                                 p = training_size,
                                 list = FALSE)
  training <- c.dat[inTrain, ]
  testing  <- c.dat[-inTrain, ]
  # fit predictive model
  plsFit <- train(
    eval(formula),
    data = training,
    method = method,
    preProc = c("center", "scale"),
    tuneLength = tuneLength
  )
  # model prediction
  plsPredTrain <- predict(plsFit, newdata = training)
  plsPredTest <- predict(plsFit, newdata = testing)
  # collect metrics
  if (is.numeric(y)){
    print(postResample(pred = plsPredTest, obs = y[!missing.idx][-inTrain]))
  } else {
    print(confusionMatrix(data = y[!missing.idx][-inTrain], reference = plsPredTest))
  }
  plsPredMissing <- predict(plsFit, newdata = data)
  # replace missing values with predicted values
  if (replace){
    idx <- which(colnames(data) == eval(formula)[[2]])
    data[is.na(data[,idx]),idx] <- plsPredMissing[is.na(data[,idx])]
    return(data)
  } else {
    return(data.frame(data,prediction = plsPredMissing))
  }
}