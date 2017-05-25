######### Random Forest #########
load("WeatherData.RData")

# Combine training and testing data to make final predictions
X.full = rbind(X.train, X.test)
dim(X.full)

y.full = as.factor(c(as.character(y.train), 
                     as.character(y.test)))
length(y.full)

library(dplyr)
library(magrittr)
library(randomForest)
library(pROC)
library(ggplot2)

load("fitRF1000.RData")

RF_fit = randomForest(X.train, y.train, ntree = 1000)

# save(RF_fit, file = "fitRF.RData")
# save(RF_fit, file = "fitRF1000.RData")

# Construct y_hats
y_hat_RF = predict(RF_fit, newdata = X.test, type = "prob")[,2]

# Plot overlayed predicted probability histogram 
plotDF_RF = as.data.frame(y_hat_RF)
names(plotDF_RF) = "probs"

ggplot(plotDF_RF, aes(x = probs)) + 
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

best_RF = bestThresh(y_hat_RF, y.test)

# Construct y_hat01
prob_thresh_RF = best_RF[1]

y_hat_RF01 = factor(ifelse(y_hat_RF > prob_thresh_RF, 1, 0),
                    levels = c(0, 1))

# Construct roc values
ROC_RF = roc(y.test, y_hat_RF)

# Plot ROC curves
plot(ROC_RF, main = "ROC curves", xlim = c(1, 0), col = "blue")
abline(v = c(1, 0))
legend(0.5, 0.3, legend = c("Random Forest"), 
       col = c("blue"), lty = 1, cex = 0.5, bty = "n", lwd = 3)

# Variable Importance Plot
varImpPlot(RF_fit)

# Get 0-1 predictions with threshold = 0.5
# y_hat_RF01 = predict(RF_fit, newdata = cbind(y.test, X.test), type = "response")

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

# Construct Confusion Matrix
confuseMat_RF = table(y_hat = y_hat_RF01, y_test = y.test)

# Model Performance Measures 
RF_ModPerf = ModPerfMeasures(confuseMat_RF, trBaseRate, teBaseRate, prob_thresh_RF)

# save(RF_fit, y_hat_RF, ROC_RF, RF_ModPerf, 
#      best_RF, file = "RFFit.RData")

######### End Random Forest #########
