MLimputationNum <- function(data,
                            y,
                            formula,
                            training_size = .8,
                            im.method = "pls") {
  
  # get outcome variable as column number
  if(is.character(y))
    y <- data[ ,which(colnames(data)==y)]
  # calculate percentage of missing values
  m.perc <- sum(complete.cases(data)/nrow(data))
  # display worning if over some limit?
  paste0(m.perc*100,"% of outcome variable is missing!")
  # get missing values index
  missing.idx <- is.na(y.vector)
  # split into missing and complete cases
  c.dat <- iris[!missing.idx, ]
  m.dat <- iris[missing.idx, ]
  # create data partition
  inTrain <- createDataPartition(y = y[!missing.idx],
                                 p = training_size,
                                 list = FALSE)
  training <- c.dat[inTrain, ]
  testing  <- c.dat[-inTrain, ]
  # train the basic ML model
  plsFit <- train(
    eval(formula),
    data = training,
    method = im.method,
    preProc = c("center", "scale"),
    tuneLength = 15
  )
  # predict
  plsPredTrain <- predict(plsFit, newdata = NULL)
  plsPredTest <- predict(plsFit, newdata = testing)
  # collect metrics
  # (...)
  # end of Part No.1
  ## Part No.2 - impute data
  plsPredMissing <- predict(plsFit, newdata = m.dat)
  ## Merge datasets
  df <- rbind(training, testing, m.dat)
  predVector <- c(plsPredTrain, plsPredTest, plsPredMissing)
  df <- cbind(df, predVector)
  ## end of temp function
  return(df)
}