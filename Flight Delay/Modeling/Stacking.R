######### Stacking ########
stackedDF = data.frame(RF = y_hat_RF, xgb = y_hat_xgb, KNN = knnfit[[10]],
                       lasso = y_hat_lasso, y.test)

# Fit logistic on stacked predictions
stackedFit = glm(y.test ~ ., data = stackedDF, family = "binomial")

y_hat_stacked = predict(stackedFit, newdata = stackedDF, type = "response")

# Plot overlayed predicted probability histogram 
plotDF_stack = as.data.frame(y_hat_stacked)
names(plotDF_stack) = "probs"

ggplot(plotDF_stack, aes(x = probs)) + 
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
ROC_stacked = roc(y.test, y_hat_stacked)

# Plot ROC curves
plot(ROC_stacked, main = "ROC curves", xlim = c(1, 0), col = "green")
abline(v = c(1, 0))
plot(ROC_lasso, add = TRUE, col = "red")
plot(ROC_RF, add = TRUE, col = "blue")
plot(ROC_xgb, add = TRUE, col = "black")
# plot(ROC_knn, add = TRUE, col = "pink")
legend(0.5, 0.4, legend = c(paste0("Stacked|AUC = ", ROC_stacked$auc),
                            paste0("Gradient Boosted Tree|AUC = ", ROC_xgb$auc),
                            paste0("Random Forest|AUC = ", ROC_RF$auc),
                            paste0("Logistic Lasso|AUC = ", ROC_lasso$auc)), 
       col = c("green", "black", "blue", "red"), lty = 1, cex = 0.5, bty = "n", lwd = 3)

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

best_stack = bestThresh(y_hat_stacked, y.test)

# Probability threshold
prob_thresh_stack = best_stack[1]

# Construct y_hat01
y_hat_stacked01 = ifelse(y_hat_stacked >= prob_thresh_stack, 1, 0)

# Construct Confusion Matrix
confuseMat_stack = table(y_hat = y_hat_stacked01, y_test = y.test)

# Model Performance Measures 
stack_ModPerf = ModPerfMeasures(confuseMat_stack, trBaseRate, teBaseRate, prob_thresh_stack)

## Combine Performance
all_ModPerf = data.frame(stack_ModPerf, xgb_ModPerf, 
                         RF_ModPerf, lasso_ModPerf)
names(all_ModPerf) = c("stacked", "xgboost", "randomForest", "lassoLogit") 



