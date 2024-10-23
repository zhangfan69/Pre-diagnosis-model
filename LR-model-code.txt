install.packages("openxlsx")
install.packages("readxl")
install.packages("dplyr")
install.packages("pROC")
install.packages("ggplot2")
install.packages("rms")
library(openxlsx)
library(readxl)
library(dplyr)
library(pROC)
library(ggplot2)
library(rms)


#The data set is divided into the training set and the test set in a 7:3 ratio


# Read Excel data
data <- read.xlsx("data.xlsx", sheet = 1)

# Calculate the index of the training set and the test set
set.seed(123)  
trainIndex <- sample(1:nrow(data), size = round(0.7 * nrow(data)), replace = FALSE)
testIndex <- setdiff(1:nrow(data), trainIndex)

# Extract training set and test set data
trainData <- data[trainIndex, ]
testData <- data[testIndex, ]

# Output training set data to Excel
write.xlsx(trainData, "trainData.xlsx", rowNames = FALSE)

# Output test set data to Excel
write.xlsx(testData, "testData.xlsx", rowNames = FALSE)



#The training set was used for univariate and multivariate logistic regression analysis


# Read Excel data
trainData <- read_excel("trainData.xlsx")

# Univariate logistic regression analysis
single_factor_models <- lapply(1:33, function(i) {
  formula <- as.formula(paste("Outcome ~", names(trainData)[i]))
  glm(formula, data = trainData, family = binomial)
})

# Print a summary of the single factor model
lapply(single_factor_models, summary)

# Multi-factor logistic regression analysis
multi_factor_model <- glm(Outcome ~ ., data = trainData[, 1:33], family = binomial)

# Print a summary of the multifactor model
summary(multi_factor_model)




#Construction and performance evaluation of logistic regression model



trainData$Outcome <- as.factor(trainData$Outcome)  # Make sure the target variable is a factor type

# Build logistic regression model
LRmodel <- glm(Outcome ~ BMI + Fever + Dyspnea + Patchy_shadow + 
               Pleural_thickening + Cavitation + Calcification + Tree_in_bud + N3, 
             data = trainData, family = binomial)

# View the model summary
summary(LRmodel)

# Training set results
predicted <- predict(LRmodel, newdata = trainData, type = "response")
predicted_class <- ifelse(predicted > 0.5, 1, 0)  # Convert probabilities to categories (binary classification threshold is usually 0.5)

# Training set evaluates model performance
confusion_matrix <- table(trainData$Outcome, predicted_class)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
precision <- confusion_matrix[2, 2] / sum(confusion_matrix[, 2])  # Correctly predict the rate of illness
recall <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])  # The proportion of real illnesses that are correctly predicted
f1_score <- 2 * precision * recall / (precision + recall)

# Output model evaluation indicators
cat(sprintf("Accuracy: %.2f\n", accuracy))
cat(sprintf("Precision: %.2f\n", precision))
cat(sprintf("Recall: %.2f\n", recall))
cat(sprintf("F1 Score: %.2f\n", f1_score))

# ROC curve and AUC of disease
library(pROC)
library(ggplot2)
roc <- roc(trainData$Outcome, predict(LRmodel, type = "response"))
auc <- auc(roc)
# Calculate the 95% confidence interval for the AUC
ci <- ci(roc)
# Print the AUC value and 95% confidence interval
cat(paste("AUC: ", format(round(auc, 3), nsmall = 3), "\n"))
cat("95% Confidence Interval: ", format(round(ci[1], 3), nsmall = 3), " - ", format(round(ci[3], 3), nsmall = 3), "\n")

# Extract ROC curve data
roc_data <- data.frame(
  specificity = 1 - roc$specificities,
  sensitivity = roc$sensitivities
)


# Constructs text that contains AUC and confidence interval information
auc_text <- paste("AUC =", format(round(auc, 3), nsmall = 3))
ci_text <- paste("95% CI: ", format(round(ci[1], 3), nsmall = 3), " - ", format(round(ci[3], 3), nsmall = 3), "\n")


# Plot ROC curve
roc_plot <- ggplot(roc_data, aes(x = specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  labs(title = "ROC Curve",
       x = "1 - Specificity",
       y = "Sensitivity") +
  theme_minimal() +
  # Add AUC and confidence interval text annotations
  geom_text(x = 0.8, y = 0.2, label = auc_text, hjust = 0, size = 4) +
  geom_text(x = 0.8, y = 0.12, label = ci_text, hjust = 0, size = 4)

# Show the ROC graph drawn
print(roc_plot)



# Test set results
test_data <- read_excel("testData.xlsx", sheet = 1, col_names = TRUE)

# Evaluate the model using test sets
predicted_probs2 <- predict(LRmodel, newdata = test_data, type = "response")

# Build the ROC curve of the test set
roc_curve2 <- roc(test_data$Outcome, predicted_probs2)

# Output ROC curve summary information
summary(roc_curve2)


# Calculate the AUC
auc_value2 <- auc(roc_curve2)
cat("AUC:", auc_value2, "\n")

# Calculate the 95% confidence interval for the AUC
ci2 <- ci(roc_curve2)
# Print the AUC value and 95% confidence interval
cat(paste("AUC: ", format(round(auc_value2, 3), nsmall = 3), "\n"))
cat("95% Confidence Interval: ", format(round(ci2[1], 3), nsmall = 3), " - ", format(round(ci2[3], 3), nsmall = 3), "\n")

# Predict test set results
predicted2 <- predict(LRmodel, newdata = test_data, type = "response")
predicted_class2 <- ifelse(predicted2 > 0.5, 1, 0)  # Convert probabilities to categories (binary classification threshold is usually 0.5)

# Test sets evaluate model performance
confusion_matrix2 <- table(test_data$Outcome, predicted_class2)
accuracy2 <- sum(diag(confusion_matrix2)) / sum(confusion_matrix2)
precision2 <- confusion_matrix2[2, 2] / sum(confusion_matrix2[, 2])  
recall2 <- confusion_matrix2[2, 2] / sum(confusion_matrix2[2, ])  
f1_score2 <- 2 * precision2 * recall2 / (precision2 + recall2)

# Output model evaluation indicators
cat(sprintf("Accuracy: %.2f\n", accuracy2))
cat(sprintf("Precision: %.2f\n", precision2))
cat(sprintf("Recall: %.2f\n", recall2))
cat(sprintf("F1 Score: %.2f\n", f1_score2))

#Test set extracts ROC curve data
roc_data2 <- data.frame(
  specificity = 1 - roc$specificities,
  sensitivity = roc$sensitivities
)

# Test set constructs text that contains AUC and confidence interval information
auc_text2 <- paste("AUC =", format(round(auc_value, 3), nsmall = 3))
ci_text2 <- paste("95% CI: ", format(round(ci2[1], 3), nsmall = 3), " - ", format(round(ci2[3], 3), nsmall = 3), "\n")


# Plot the ROC curve of the test set
roc_plot2 <- ggplot(roc_data2, aes(x = specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  labs(title = "ROC Curve",
       x = "1 - Specificity",
       y = "Sensitivity") +
  theme_minimal() +
  # Add AUC and confidence interval text annotations
  geom_text(x = 0.8, y = 0.2, label = auc_text2, hjust = 0, size = 4) +
  geom_text(x = 0.8, y = 0.12, label = ci_text2, hjust = 0, size = 4)

# Displays the ROC graph drawn with the test set
print(roc_plot2)


#PR curves
# Calculate prediction tags
predictions <- ifelse(probabilities > 0.5, 1, 0)

# Calculate ROC curve
roc_obj <- roc(trainData$Outcome, probabilities)

# Calculate recall and accuracy
precision <- roc_obj$sensitivities
recall <- roc_obj$specificities

# Reverse recall and precision
recall <- rev(recall)
precision <- rev(precision)

# Draw PR curve
plot(recall, precision, col = "red", main = "Precision-Recall Curve", xlab = "Recall", ylab = "Precision", type = "l")

# Predict probabilities on the training set
predicted_scores <- predict(LRmodel, newdata = trainData, type = "response")

# Get true_labels from trainData
true_labels <- trainData$Outcome

# Calculate the area under the PR Curve (PR AUC)
pr_auc <- auc(roc_obj)

# Output PR AUC
cat("PR AUC:", pr_auc, "\n")

# Add PR AUC values and confidence intervals to the chart
text(0.5, 0.1, paste("PR AUC =", round(pr_auc, 5)), adj = 0)

# Test set PR curve

# Calculate the prediction probability
probabilities2 <- predict(model, type = "response")

# Calculate prediction tags
predictions2 <- ifelse(probabilities > 0.5, 1, 0)

# Calculate ROC curve
roc_obj2 <- roc(test_data$Outcome, probabilities2)

# Calculate recall rates and accuracy rates
precision2 <- roc_obj2$sensitivities
recall2 <- roc_obj2$specificities

# Reverse recall and precision
recall2 <- rev(recall2)
precision2 <- rev(precision2)

# Draw PR curve
plot(recall2, precision2, col = "red", main = "Precision-Recall Curve", xlab = "Recall", ylab = "Precision", type = "l")

# Predict probabilities on test data sets
predicted_scores2 <- predict(LRmodel, newdata = test_data, type = "response")

# Get true_labels from test_data
true_labels2 <- test_data$Outcome

# Calculate the area under the PR Curve (PR AUC)
pr_auc2 <- auc(roc_obj2)

# Output PR AUC
cat("PR AUC:", pr_auc2, "\n")

# Add PR AUC values and confidence intervals to the chart
text(0.5, 0.1, paste("PR AUC =", round(pr_auc2, 5)), adj = 0)

# Create a Nomogram
nom <- nomogram(LRmodel, fun = plogis, funlabel = "Probability of Outcome")
plot(nom)