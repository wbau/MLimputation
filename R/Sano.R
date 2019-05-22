library(caret)
##library(simputation)
set.seed(107)



Wrapper.inp.nnet <-function(training){
ind.inp <- which(is.na(training[,1]))  
  
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

pred.inp <- predict(nnetFit)[ind.inp]  
out <- list(ind.inp,pred.inp,nnetFit)
names(out) <- c("ind.inp","pred.inp","nnetFit")
return(out)
}

Wrapper.inp.perform <- function(ans.inp,inp.val){
  RMSE.inp <- sqrt(mean((inp.val-ans.inp)^2))
  return(RMSE.inp)
}

Wrapper.test.perform <- function(nnetFit,testing){
  pred.test <- predict(nnetFit, newdata = testing)
  ans.test  <- dat[-inTrain,1]  
  RMSE.test <- sqrt(mean((pred.test-ans.test)^2))
  return(RMSE.test)
}

# Start
dat <- iris

inTrain <- createDataPartition(
  y = dat$Sepal.Length[is.na(dat$Sepal.Length)==F],
  ## the outcome data are needed
  p = .75,
  ## The percentage of data in the
  ## training set
  list = FALSE
)



ind.inp <- 1:3
ans.inp <- iris[ind.inp,1]
dat[ind.inp,1]  <- NA
training <- dat[ inTrain,]
testing  <- dat[-inTrain,]

out.inp <- Wrapper.inp.nnet(training)
Wrapper.inp.perform(ans.inp,out.inp[[2]])
Wrapper.test.perform(out.inp[[3]],testing)
