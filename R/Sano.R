library(caret)
##library(simputation)

dat <- iris
ind.inp <- 1:3
dat[ind.inp,1]  <- NA
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

ans.inp <- iris[ind.inp,1]
pred.inp <- predict(nnetFit)[ind.inp]
RMSE.inp <- sqrt(mean((pred.inp-ans.inp)^2))

pred.test <- predict(nnetFit, newdata = testing)
ans.test  <- dat[-inTrain,1]
RMSE.test <- sqrt(mean((pred.test-ans.test)^2))

