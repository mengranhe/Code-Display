######### Boosted Tree Final Model#########
load("WeatherData.RData")
load("xgbFit.RData")

# Combine training and testing data to make final predictions
X.full = rbind(X.train, X.test)
dim(X.full)

y.full = as.factor(c(as.character(y.train), 
                     as.character(y.test)))
length(y.full)

X.guess = model.matrix(DEP_DEL15 ~ ., data = guess16NonM)[,-1]
dim(X.guess)

library(dplyr)
library(magrittr)
library(glmnet)
library(pROC)
library(caret)
library(xgboost)

# Fit boosted tree
xgb_fit_full = xgboost(data = X.full, label = as.numeric(levels(y.full))[y.full], nrounds = 1000,
                  params = best_xgb_params, print_every_n = 1, early_stopping_rounds = 100)

y_hat_xgb_full = predict(xgb_fit_full, newdata = X.guess)

#save(xgb_fit_full, y_hat_xgb_full, file = "xgbFit_Full.RData")
