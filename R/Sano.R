library(caret)
##library(simputation)
set.seed(107)



Wrapper.inp.nnet <-function(training,ind.val){
flag.cont <- !is.factor(training[,ind.val])   
ind.inp <- which(is.na(training[,ind.val]))  
  
nnetFit <- train(
  eval(parse(text=paste0(colnames(training)[ind.val],"~."))),
  ##Sepal.Length ~ .,
  data = training,
  method = "nnet",
  ## Center and scale the predictors for the training
  ## set and all future samples.
  preProc = c("center", "scale"), 
  tuneGrid = expand.grid(size=c(5:6), decay=seq(0.1, 1, 0.1)),
  linout = flag.cont,
  ##tuneLength = 15,
  na.action = na.pass
)

pred.inp <- predict(nnetFit)[ind.inp]  
out <- list(ind.inp,pred.inp,nnetFit)
names(out) <- c("ind.inp","pred.inp","nnetFit")
return(out)
}

Wrapper.inp.perform <- function(ans.inp,inp.val,categ){
  if(categ==T) {perform <- sum(ans.inp==inp.val)/length(ans.inp)
                measure <- "accuracy"}
  else   {perform <- sqrt(mean((inp.val-ans.inp)^2))
          measure <- "RMSE"}
  out <- c(measure,perform)
  return(out)
}

Wrapper.test.perform <- function(nnetFit,testing,ind.val){
  categ <- is.factor(testing[,ind.val])
  pred.test <- predict(nnetFit, newdata = testing)
  ans.test  <- dat[-inTrain,ind.val]
  if (categ ==T) {perform <- sum(pred.test==ans.test)/length(ans.test)
                  measure <- "accuracy"}
  else  {perform <- sqrt(mean((pred.test-ans.test)^2))
         measure <- "RMSE"}
  out <- c(measure, perform)
  return(out)
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


N.NA <- 10
ind.inp <- sample(nrow(dat),N.NA)
ind.val <- 5
categ <- is.factor(dat[,ind.val])

ans.inp <- iris[ind.inp,ind.val]
dat[ind.inp,ind.val]  <- NA
training <- dat[ inTrain,]
testing  <- dat[-inTrain,]

out.inp <- Wrapper.inp.nnet(training,ind.val)
Wrapper.inp.perform(ans.inp,out.inp[[2]],categ)
Wrapper.test.perform(out.inp[[3]],testing,ind.val)
