######### Logistic Regression #########
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
library(ggplot2)

load("lassoFits.RData") # To save time

lasso_fit = glmnet(X.train, y.train, family = "binomial", alpha = 1)
lasso_cv = cv.glmnet(X.train, y.train, family = "binomial", alpha = 1, nfolds = 10)

ridge_fit = glmnet(X.train, y.train, family = "binomial", alpha = 0)
ridge_cv = cv.glmnet(X.train, y.train, family = "binomial", alpha = 0, nfolds = 10)

# save(lasso_fit, lasso_cv,
#      ridge_fit, ridge_cv, file = "lassoRidgeFits.RData")
# save(lasso_fit, lasso_cv, file = "lassoFits.RData")

# Extract lambda1SE
lasso_lambdaMIN = lasso_cv$lambda.min
lasso_lambda1SE = lasso_cv$lambda.1se
ridge_lambdaMIN = ridge_cv$lambda.min
ridge_lambda1SE = ridge_cv$lambda.1se

# Plot binomial deviance against lambda's
plot(lasso_fit, xvar = "lambda", main = "binomial deviance vs. log lambda for")

# Count the number of nonzero coefficients
coefWhich = which(coef(lasso_fit, lasso_lambdaMIN)!=0)

length(coefWhich)

# Variables with nonzero coefficients
colnames(X.train)[coefWhich]

# Make predictions
y_hat_lasso = predict(lasso_fit, newx = X.test, type = "response", s = lasso_lambdaMIN)
y_hat_ridge = predict(ridge_fit, newx = X.test, type = "response", s = ridge_lambdaMIN)

y_hat_lasso_1SE = predict(lasso_fit, newx = X.test, type = "response", s = lasso_lambda1SE)
y_hat_ridge_1SE = predict(ridge_fit, newx = X.test, type = "response", s = ridge_lambda1SE)

# Plot overlayed predicted probability histogram 
plotDF_lasso = as.data.frame(y_hat_lasso)
names(plotDF_lasso) = "probs"

ggplot(plotDF_lasso, aes(x = probs)) + 
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

# Construct roc values
ROC_lasso = roc(y.test, y_hat_lasso)
ROC_ridge = roc(y.test, y_hat_ridge)

# Plot ROC curves
plot(ROC_lasso, main = "ROC curve of Logistic Lasso", xlim = c(1, 0), col = "red")
plot(ROC_ridge, add = TRUE, xlim = c(1, 0), col = "blue")
abline(v = c(1, 0))

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

best_lasso = bestThresh(y_hat_lasso, y.test)

# Construct y_hat01
prob_thresh_lasso = best_lasso[1]

y_hat_lasso01 = factor(ifelse(y_hat_lasso > prob_thresh_lasso, 1, 0),
                 levels = c(0, 1))
y_hat_ridge01 = factor(ifelse(y_hat_ridge > prob_thresh_ridge, 1, 0),
                       levels = c(0, 1))

# Construct Confusion Matrix
confuseMat_lasso = table(y_hat = y_hat_lasso01, y_test = y.test)

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
lasso_ModPerf = ModPerfMeasures(confuseMat_lasso, trBaseRate, teBaseRate, prob_thresh_lasso)

# save(lasso_fit, lasso_cv, ROC_lasso, best_lasso,
#      y_hat_lasso, lasso_ModPerf, file = "lassoFit.RData")

######### End Logistic Regression #########

