MLimputation <- function(data,
                            y,
                            formula,
                            training_size = .8,
                            method = "pls",
                            tuneLength = 15,
                            replace = T,
                            useFitControl = TRUE,
                            tuningMethod = "boot",
                            repeat.times = 10,
                            foldRep = ifelse(grepl("[d_]cv$", method), 1, NA)) {
  # calculate percentage of missing values
  m.perc <- sum(complete.cases(y) / length(y))
  # display worning if over some limit?
  warning(paste0(round((1 - m.perc) * 100, 2), "% of outcome variable is missing!"))
  # check for the missing-values
  keep.column <- NULL
  for (column.id in which(colnames(data) != formula[[2]])) {
    if (any(is.na(data[, column.id]))) {
      warning(
        paste0(
          "Missing values at ",
          colnames(data)[column.id],
          ". Removing ",
          colnames(data)[column.id],
          " from the predictive model formula."
        )
      )
    } else {
      keep.column <- c(keep.column, colnames(data)[column.id])
    }
  }
  formula <-
    as.formula(paste0(formula[[2]], "~", paste(keep.column, collapse = "+")))
  
  # get missing values index
  missing.idx <- is.na(y)
  # split into missing and complete cases
  c.dat <- data[!missing.idx, ]
  m.dat <- data[missing.idx, ]
  # create data partition
  inTrain <- createDataPartition(y = y[!missing.idx],
                                 p = training_size,
                                 list = FALSE)
  training <- c.dat[inTrain,]
  testing  <- c.dat[-inTrain,]
  if (useFitControl) {
    fitControl <- trainControl(method = tuningMethod,
                               number = repeat.times,
                               ## repeated ten times
                               repeats = foldRep)
    
    predictionModel <- train(
      eval(formula),
      data = training,
      method = method,
      preProc = c("center", "scale"),
      trControl = fitControl,
      tuneLength = tuneLength
    )
  } else {
    predictionModel <- train(
      eval(formula),
      data = training,
      method = method,
      preProc = c("center", "scale"),
      tuneLength = tuneLength,
      metric = "ROC"
    )
  }
  # model prediction
  predictionTesting <- predict(predictionModel, newdata = testing)
  # collect metrics
  if (is.numeric(y)) {
    print(postResample(pred = predictionTesting, obs = y[!missing.idx][-inTrain]))
  } else {
    print(confusionMatrix(data = y[!missing.idx][-inTrain], reference = predictionTesting))
  }
  imputedData <- predict(predictionModel, newdata = data)
  # replace missing values with predicted values
  if (replace) {
    idx <- which(colnames(data) == formula[[2]])
    data[is.na(data[, idx]), idx] <-
      imputedData[is.na(data[, idx])]
    return(data)
  } else {
    return(data.frame(data, prediction = imputedData))
  }
  
}
