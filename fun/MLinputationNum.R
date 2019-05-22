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
    method = method,
    preProc = c("center", "scale"),
    tuneLength = tuneLength
  )
  # predict
  plsPredTrain <- predict(plsFit, newdata = NULL)
  plsPredTest <- predict(plsFit, newdata = testing)
  # collect metrics
  # (...)
  plsPredMissing <- predict(plsFit, newdata = m.dat)
  ## Merge datasets
  df <- rbind(training, testing, m.dat)
  predVector <- c(plsPredTrain, plsPredTest, plsPredMissing)
  
  cbind(df, predVector)
  ## end of temp function
}













function (dat, formula, predictor = foretell, ...) 
{
  model <- eval(formula[[3]])
  imputed <- get_imputed(formula, dat)
  args <- list(object = model, newdata = dat, ...)
  pred_val <- tryCatch(do.call(predictor, args), error = function(e) {
    warnf("Could not compute predictions:\n%s\nReturning original data.", 
          e$message)
    NULL
  })
  if (is.null(pred_val)) 
    return(dat)
  if (length(pred_val) != nrow(dat)) 
    warnf("Numberof values returned by the predictor is not equal to number of rows in data")
  for (var in imputed) {
    ina <- is.na(dat[var])
    dat[ina, var] <- pred_val[ina]
  }
  dat
}
