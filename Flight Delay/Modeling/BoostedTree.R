######### Boosted Tree #########
load("WeatherData.RData")

# Combine training and testing data to make final predictions
X.full = rbind(X.train, X.test)
dim(X.full)

y.full = as.factor(c(as.character(y.train), 
                     as.character(y.test)))
length(y.full)

library(dplyr)
library(magrittr)
library(glmnet)
library(pROC)
library(caret)
library(xgboost)

#### Hyperparameter search
# Cross-validated hyper-parameter search
xgb_grid1 = expand.grid(
  nrounds = 1000,
  eta = c(0.01, 0.001, 0.0005),
  max_depth = seq(2, 10, 2),
  gamma = c(0.5, 1),
  colsample_bytree = 1,    # default = 1
  min_child_weight = 1,    # default = 1
  subsample = c(0.5, 1)      # default = 1
)

# Pack the training control parameters
xgb_trainControl1 = trainControl(
  method = "cv",
  number = 5,
  verboseIter = TRUE,   # print iteration messages or not
  returnData = FALSE,
  returnResamp = "all",             # save losses across all models
  classProbs = TRUE,                # set to TRUE for AUC to be computed
  summaryFunction = twoClassSummary,
  allowParallel = TRUE
)

load("xgb_fit.RData")

y.trainX = y.train
levels(y.trainX) %<>% paste0("X", .)

y.fullX = y.full
levels(y.fullX) %<>% paste0("X", .)
# Fit xgb with hyper-parameter search
xgb_fit1 = train(x = X.train, y = y.trainX, 
                 trControl = xgb_trainControl1,
                 tuneGrid = xgb_grid1,
                 method = "xgbTree",
                 save_period = 50,
                 save_name = "xgboost_")

# save(xgb_fit1, file = "xgb_fit.RData")

# Scatter plot of the AUC against max_depth and eta
ggplot(xgb_fit1$results, aes(x = as.factor(eta), y = max_depth, size = ROC, color = ROC)) + 
  geom_point() + 
  theme_bw() + 
  scale_size_continuous(guide = "none")

# # Joyce xgboost
# load("xgBoost_result.RData")
# 
# xgb_fit1 = xgb_1

# Extract best model
best_xgb_params = lapply(subset(xgb_fit1$results, ROC == max(ROC))[,1:7], 
                         function(x) x)
best_xgb_params %<>% c(objective = "binary:logistic", eval_metrix = "auc")

# # xgboost fitting with arbitrary parameters
# xgb_params0 = list(
#   objective = "binary:logistic",    # binary classification
#   eta = 0.01,                       # learning rate
#   max.depth = 3,                    # max tree depth
#   eval_metric = "auc"               # evaluation/loss metric
# )

# Fit boosted tree
xgb_fit = xgboost(data = X.train, label = as.numeric(levels(y.train))[y.train], nrounds = 1000,
                  params = best_xgb_params, print_every_n = 1, early_stopping_rounds = 100)

y_hat_xgb = predict(xgb_fit, newdata = X.test)


# Plot overlayed predicted probability histogram 
plotDF_xgb = as.data.frame(y_hat_xgb)
names(plotDF_xgb) = "probs"

ggplot(plotDF_xgb, aes(x = probs)) + 
  geom_histogram(binwidth = 0.02, 
                 aes(fill = as.factor(y.test)),
                 position = "identity", alpha = 0.65) +
  labs(title = "Overlayed Histogram of Predicted Probabilities",
       x = "Predicted Probability") +
  theme_bw() +
  scale_fill_discrete(name = "Observed Y") +
  theme(axis.title.x = element_text(family = "Didot",
                                    face = "bold", size = 15),
        axis.title.y = element_text(family = "Didot",
                                    face = "bold", size = 15),
        plot.title = element_text(family = "Didot", # Trebuchet MS
                                  face = "bold", size = 20, hjust = 0.5),
        axis.line = element_line(color="black", size = 0.5))

# Function to find best threshold
bestThresh = function(y_hats, y_test){
  seqTry = seq(0, 1, 0.001)
  
  current_misclass = 2
  current_thresh = NA
  for(ii in seqTry){
    y_hat01 = factor(ifelse(y_hats >= ii, 1, 0),
                     levels = c(0, 1))
    misclass = mean(y_hat01 != y_test)
    thresh = ii
    if(misclass < current_misclass){
      current_misclass = misclass
      current_thresh = thresh
    }
  }
  return(c(thresh = current_thresh, misclass = current_misclass))
}

best_xgb = bestThresh(y_hat_xgb, y.test)

# Construct y_hat01
prob_thresh_xgb = best_xgb[1]

y_hat_xgb01 = factor(ifelse(y_hat_xgb > prob_thresh_xgb, 1, 0),
                    levels = c(0, 1))

# Construct roc values
ROC_xgb = roc(y.test, y_hat_xgb)

# Plot ROC curves
plot(ROC_xgb, main = "ROC curves", xlim = c(1, 0), col = "blue")
abline(v = c(1, 0))
legend(0.5, 0.3, legend = c("Gradient Boosted Tree"), 
       col = c("blue"), lty = 1, cex = 0.5, bty = "n", lwd = 3)

# Construct Confusion Matrix
confuseMat_xgb = table(y_hat = y_hat_xgb01, y_test = y.test)

#### Model Performance Measures (function)
ModPerfMeasures = function(confuseMat, trBaseRate, teBaseRate, threshold = 0.5){
  
  # Misclassification rate
  misclassRates = 1 - sum(diag(confuseMat))/sum(confuseMat)
  
  # Sensitivity
  sensitivity = confuseMat[2,2]/sum(confuseMat[,2])
  
  # Specificity
  specificity = confuseMat[1,1]/sum(confuseMat[,1])
  
  # Precision (Positive Predictive Value)
  PPV = confuseMat[2,2]/sum(confuseMat[2,])
  
  # Negative Predictive Value
  NPV = confuseMat[1,1]/sum(confuseMat[1,])
  
  return(t(data.frame(threshold, trBaseRate, teBaseRate, misclassRates, 
                    sensitivity, specificity, PPV, NPV)))
}

# Model Performance Dataframe
xgb_ModPerf = ModPerfMeasures(confuseMat_xgb, trBaseRate, teBaseRate, prob_thresh_xgb)

# save(xgb_fit1, best_xgb_params, xgb_fit, y_hat_xgb, ROC_xgb, xgb_ModPerf,
#      best_xgb, file = "xgbFit.RData")
