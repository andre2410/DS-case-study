library(randomForest)
# model.R
# Simple linear model using a training/test split
# This is just an example to remind you how to define the 
# Root Relative Squared Error and how to use replicate (as we did for Assn 2)
#
#############
# rrse
###########################################################
# IN: y - the actual values of the response
#    yhat - the predicted values
# OP: Calculate rrse
# OUT: The rrse
# NOTE: Because we may have NA's in the data, have protected the
# mean and sum functions to ignore NA's
##########################################################
rrse <- function(y, yhat) sqrt( sum((y - yhat)^2,na.rm=T) / sum((y - mean(y,na.rm=T))^2))


##################
# test.lm
#####################################################################
#
# IN: data - the table of data used for modelling
#   formula - the formula specifying the response and explanatories
#  prec.train - the percentage of the data used for training
# OP: Build a linear model using the training data, and predict using the
#   remaining test data.
# ASSUMES: That we are predicting a numeric value (and can therefore use rrse)
# OUT: The rrse for the test predictions.
#####################################################################
test.lm <- function(data,formula,perc.train=0.9)
{
  #sample 30% of the data
  sampled <- data[sample(nrow(data), nrow(data) * 0.3), ]
  # First find out which column is the response from the formula
  #
  resp.str <- as.character(formula)[2] # Second entry is the response
  resp.col <- which(colnames(data)==resp.str) # Use this for rrse call 
  
  train.row <- sample(nrow(data),
                      perc.train*nrow(data),
                      replace=FALSE)
  train.data <- data[train.row,]  # Create training and testing data
  test.data <- data[-train.row,]
  
  # And now have to deal with the issue that factored variables may not be
  # represented in both training and test data....
  factor.names <- names(train.data)[ sapply(train.data, is.factor) ]
  factor.cols <- which(colnames(train.data) %in% factor.names)
  if (length(factor.cols) > 0)
  {
    for (i in 1:length(factor.cols))
    {
      test.data[,factor.cols[i]][which(!(test.data[,factor.cols[i]] %in% 
                                           unique(train.data[,factor.cols[i]])))] <- NA 
    
    }
  }
  
  # Build the model and test
  lm.mod <- lm(formula,data=train.data)
  yhat <- predict(lm.mod, newdata=test.data)
  
  # Return error
  rrse(test.data[,resp.col],yhat)
}


test.lm2 <- function(data,formula,perc.train=0.9)
{
  #sample 30% of the data
  sampled <- data[sample(nrow(data), nrow(data) * 0.3), ]
  # First find out which column is the response from the formula
  #
  resp.str <- as.character(formula)[2] # Second entry is the response
  resp.col <- which(colnames(data)==resp.str) # Use this for rrse call 
  
  train.row <- sample(nrow(data),
                      perc.train*nrow(data),
                      replace=FALSE)
  train.data <- data[train.row,]  # Create training and testing data
  test.data <- data[-train.row,]
  
  # And now have to deal with the issue that factored variables may not be
  # represented in both training and test data....
  factor.names <- names(train.data)[ sapply(train.data, is.factor) ]
  factor.cols <- which(colnames(train.data) %in% factor.names)
  if (length(factor.cols) > 0)
  {
    for (i in 1:length(factor.cols))
    {
      test.data[,factor.cols[i]][which(!(test.data[,factor.cols[i]] %in% 
                                           unique(train.data[,factor.cols[i]])))] <- NA 
      
    }
  }
  
  # Build the model and test
  lm.mod <- lm(formula,data=train.data)
  yhat <- predict(lm.mod, newdata=test.data)
  
  # Return a list containing test(y) and predicted yhat values
  result <- data.frame(test = test.data[, resp.col], predicted = yhat)
  return(result)
}

#Using random forest
test.rf <- function(data, formula, perc.train = 0.9) 
{
  #sample 30% of the data
  sampled <- data[sample(nrow(data), nrow(data) * 0.3), ]
  # First find out which column is the response from the formula
  #
  resp.str <- as.character(formula)[2] # Second entry is the response
  resp.col <- which(colnames(data)==resp.str) # Use this for rrse call 
  
  train.row <- sample(nrow(data),
                      perc.train*nrow(data),
                      replace=FALSE)
  train.data <- data[train.row,]  # Create training and testing data
  test.data <- data[-train.row,]
  
  # And now have to deal with the issue that factored variables may not be
  # represented in both training and test data....
  factor.names <- names(train.data)[ sapply(train.data, is.factor) ]
  factor.cols <- which(colnames(train.data) %in% factor.names)
  if (length(factor.cols) > 0)
  {
    for (i in 1:length(factor.cols))
    {
      test.data[,factor.cols[i]][which(!(test.data[,factor.cols[i]] %in% 
                                           unique(train.data[,factor.cols[i]])))] <- NA 
      
    }
  }
  
  # Build the model and test
  rf.mod <- randomForest(formula,data=train.data, ntree = 500)
  yhat <- predict(rf.mod, newdata=test.data)
  
  # Return error
  rrse(test.data[,resp.col],yhat)
}


