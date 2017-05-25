######### Random Forest #########
load("WeatherData.RData")

library(dplyr)
library(magrittr)
library(randomForest)
library(pROC)
library(ggplot2)

# Combine training and testing data to make final predictions
X.full = rbind(X.train, X.test)
dim(X.full)

y.full = as.factor(c(as.character(y.train), 
                     as.character(y.test)))
length(y.full)

X.guess = model.matrix(DEP_DEL15 ~ ., data = guess16NonM)[,-1]
dim(X.guess)


RF_fit_full = randomForest(X.full, y.full, ntree = 1000)
y_hat_RF_full = predict(RF_fit_full, newdata = X.guess, type = "prob")[,2]

save(RF_fit_full, y_hat_RF_full, file = "RFFit_ful.RData")
