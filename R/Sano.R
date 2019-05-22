library(caret)

library(simputation)


dat <- iris

dat[1:3,1]  <- NA



set.seed(107)

inTrain <- createDataPartition(

  y = dat$Sepal.Length[is.na(dat$Sepal.Length)==F],

  ## the outcome data are needed

  p = .75,

  ## The percentage of data in the

  ## training set

  list = FALSE

)



training <- dat[ inTrain,]

testing  <- dat[-inTrain,]



nnetFit <- train(

  Sepal.Length ~ .,

  data = training,

  method = "nnet",

  ## Center and scale the predictors for the training

  ## set and all future samples.

  preProc = c("center", "scale"), 
  tuneGrid = expand.grid(size=c(1:10), decay=seq(0.1, 1, 0.1)),
  linout = TRUE,

  ##tuneLength = 15,
  na.action = na.pass
)
