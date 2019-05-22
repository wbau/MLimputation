MLimputationNum <- function(data,
                            y,
                            formula,
                            training_size = .8,
                            method = "pls",
                            tuneLength = 15) {
  
  # get outcome variable as column number
  if(is.null(y))
    return(data)
  if(is.character(y))
    y <- data[ ,which(colnames(data)==y)]
  if (length(y) != nrow(data)) 
    warning("Number of outcome vector values is not equal to number of rows in data.")
  # calculate percentage of missing values
  m.perc <- sum(complete.cases(data)/nrow(data))
  # display worning if over some limit?
  warning(paste0(m.perc*100,"% of outcome variable is missing!"))
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
  plsPredTrain <- predict(plsFit, newdata = NULL)
  plsPredTest <- predict(plsFit, newdata = testing)
  # collect metrics
  # (...)
  plsPredMissing <- predict(plsFit, newdata = m.dat)
  # Merge datasets
  df <- rbind(training, testing, m.dat)
  predVector <- c(plsPredTrain, plsPredTest, plsPredMissing)

  cbind(df, predVector)

  # for (var in imputed) {
  #   ina <- is.na(dat[var])
  #   dat[ina, var] <- pred_val[ina]
  # }
  
  ## end of temp function
}

