library(tidyverse)
library(dplyr)
library(caret)
library(MASS)
library(pROC)
library(ggplot2)
library(car)
library(RColorBrewer)
library(randomForest)
library(rpart)

# Load dataset
df <- read.csv("WA_Fn-UseC_-Telco-Customer-Churn.csv")
df <- na.omit(df)

convert_to_binary <- function(column){
  ifelse(column == "Yes", as.numeric(1), as.numeric(0))
}

# Data preprocessing: Converting binary variables that are Yes/No into 0 and 1s
columns_to_convert <- c("Partner", "Dependents", "PhoneService", "OnlineSecurity", 
                        "OnlineBackup", "DeviceProtection", "TechSupport", 
                        "StreamingTV", "StreamingMovies", "PaperlessBilling", 
                        "MultipleLines", "Churn")

# Use lapply to apply the convert_to_binary function to each column in the list
df[columns_to_convert] <- lapply(df[columns_to_convert], convert_to_binary)
df$gender = ifelse(df$gender == "Female", 1, 0)

factors_to_convert <- c("InternetService", "Contract", "PaymentMethod")
df[, factors_to_convert] <- lapply(df[, factors_to_convert], as.factor)

# Getting rid of customer ID
df = df[, -1]

##### Split into Test/Train Split
set.seed(123)
trainIndex <- createDataPartition(df$Churn, p = 0.7, list = FALSE)
train_df <- df[trainIndex, ]
test_df <- df[-trainIndex, ]

############# Building Models

### Classification Tree
classification_tree <- rpart(Churn ~ ., data = train_df, method = "class")
summary(classification_tree)

### Random Forest

# Train the random forest model
rf_model <- randomForest(Churn ~ ., data = train_df, ntree = 100, mtry = 2)

# Create a variance importance plot
varImpPlot(rf_model)

### Boosting

# Hyperparameter tuning using a loop
idv = c(5, 10, 20)
ntv = c(7000, 10000)
lamv = c(0.001, 0.0001)
parmb = expand.grid(idv, ntv, lamv)
colnames(parmb) = c('tdepth', 'ntree', 'lam')
print(parmb)
nset = nrow(parmb)

for (i in 1:nset) {
  cat('doing boost ', i, ' out of ', nset, '\n')
  tempboost = gbm(Churn ~ . , data = train_df, distribution = 'bernoulli',
                  interaction.depth = parmb[i, 1], n.trees = parmb[i, 2], shrinkage = parmb[i, 3])
  ifit = predict(tempboost, n.trees = parmb[i, 2], newdata = train_df)
  ofit = predict(tempboost, n.trees = parmb[i, 2], newdata = test_df)
  
  round_ifit <- ifelse(ifit >= 0.5, 1, 0)
  round_ofit <- ifelse(ofit >= 0.5, 1, 0)
  
  # Checking for true positives (tp) and true negatives (tn)
  tp = ifelse(round_ofit == 1, ifelse(test_df$Churn == 1, 1, 0), 0)
  tn = ifelse(round_ofit == 0, ifelse(test_df$Churn == 0, 1, 0), 0)
  tp_sum = sum(tp)
  tn_sum = sum(tn)
  
  # Print accuracy based on test data for each model
  acc = (tp_sum + tn_sum) / nrow(test_df)
  print(paste("Accuracy for model", i, ":", acc))
}

# Final Model with Best Hyperparameters
final_boost_model <- gbm(Churn ~ ., data = train_df, distribution = 'bernoulli',
                         interaction.depth = 20,
                         n.trees = 7000,
                         shrinkage = 0.001)

# Plotting Relative Importance
var_importance <- summary(final_boost_model, plotit = FALSE)
var_importance <- var_importance[order(-var_importance$rel.inf), ]

# Create a custom color palette with 19 shades of orange
n_colors <- nrow(var_importance)  # Exclude the response variable
colors <- colorRampPalette(brewer.pal(9, "Oranges"))(n_colors)

# Code for plotting relative importance with custom color
ggplot(var_importance, aes(x = var, y = rel.inf, fill = var)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(values = colors) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Variable", y = "Relative Importance", title = "Variable Importance")

### Logistic Regression

# Creates a model with all variables
full_model <- glm(Churn ~ ., data = train_df, family = binomial)

# Backward Selection was best
backward_model <- step(full_model, direction = "backward")

# Forward Selection
null_model <- glm(Churn ~ 1, data = train_df, family = binomial)
forward_model <- step(null_model, scope=formula(full_model), direction = "forward")

# Stepwise Selection
stepwise_model <- step(backward_model, scope=formula(full_model), direction="both")

# Summaries
summary(full_model)
summary(backward_model)
summary(forward_model)
summary(stepwise_model)

# Since backward selection had the lowest AIC, we will continue with that

# Checking for multicolinearity - will likely be a problem
vif(backward_model)

# Try backwards again, without TotalCharges & MontlyCharges (high Multicolinearity)
full_model <- glm(Churn ~ . - TotalCharges - MonthlyCharges, data = train_df, family = binomial)
backward_model2 <- step(full_model, direction = "backward")
vif(backward_model2) #Significant decrease in multicolinearity, now we can move forward

# Printing coefficients for interpretation

model_summary <- summary(backward_model2)
coefficients <- coef(model_summary)
coefficients <- coefficients[, 1]  # Extract coefficients from summary
standard_errors <- coef(model_summary)[, 2]  # Extract standard errors from summary
p_values <- coef(summary(backward_model2))[, 4]  # Extract p-values from summary

variable_names <- names(coefficients)

coefficients_df <- data.frame(
  variable = variable_names,
  coefficient = coefficients,
  standard_Error = standard_errors,
  p_value = p_values
)

coefficients_df <- coefficients_df %>% 
  mutate(interpret = exp(coefficient) - 1)

###### Compare ROC AUCs

### Classification Trees
predictions <- predict(classification_tree, test_df, type = "prob")
roc_curve <- roc(test_df$Churn, predictions[,1])
roc_auc <- auc(roc_curve)
roc_df <- data.frame(FPR = 1 - roc_curve$specificities,
                     TPR = roc_curve$sensitivities)

# Plot ROC Curve
ggplot(roc_df, aes(x = FPR, y = TPR)) +
  geom_line(color = "darkorange", size = 1.5) +
  geom_point(color = "darkorange", size = 3, shape = 16) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey") +
  theme_minimal() +
  labs(x = "False Positive Rate (1 - Specificity)", y = "True Positive Rate (Sensitivity)",
       title = paste("ROC Curve (AUC =", round(roc_auc, 3), ")")) +
  annotate("text", x = 0.7, y = 0.3, label = paste("AUC =", round(roc_auc, 3)),
           color = "darkorange", size = 5) +
  annotate("text", x = 0.1, y = 0.9, label = "Perfect Classifier",
           color = "grey", size = 4, angle = 45) +
  theme(plot.background = element_rect(fill = "lightgrey"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 18, face = "bold"))

### Random Forest Model
predictions <- predict(rf_model, newdata = test_df, type = "response")
roc_curve <- roc(test_df$Churn, predictions)
roc_auc <- auc(roc_curve)
roc_df <- data.frame(FPR = 1 - roc_curve$specificities,
                     TPR = roc_curve$sensitivities)

# Plot ROC Curve
ggplot(roc_df, aes(x = FPR, y = TPR)) +
  geom_line(color = "darkorange", size = 1.5) +
  geom_point(color = "darkorange", size = 3, shape = 16) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey") +
  theme_minimal() +
  labs(x = "False Positive Rate (1 - Specificity)", y = "True Positive Rate (Sensitivity)",
       title = paste("ROC Curve (AUC =", round(roc_auc, 3), ")")) +
  annotate("text", x = 0.7, y = 0.3, label = paste("AUC =", round(roc_auc, 3)),
           color = "darkorange", size = 5) +
  annotate("text", x = 0.1, y = 0.9, label = "Perfect Classifier",
           color = "grey", size = 4, angle = 45) +
  theme(plot.background = element_rect(fill = "lightgrey"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 18, face = "bold"))

### Boosting

# Calculate ROC_AUC
test_prob <- predict(final_boost_model, n.trees = 7000, newdata = test_df, type = "response")
roc_curve <- roc(test_df$Churn, test_prob)
roc_auc <- auc(roc_curve)

# Extract ROC curve data points
roc_df <- data.frame(FPR = 1 - roc_curve$specificities,
                     TPR = roc_curve$sensitivities)

# Plot ROC Curve
ggplot(roc_df, aes(x = FPR, y = TPR)) +
  geom_line(color = "darkorange", size = 1.5) +
  geom_point(color = "darkorange", size = 3, shape = 16) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey") +
  theme_minimal() +
  labs(x = "False Positive Rate (1 - Specificity)", y = "True Positive Rate (Sensitivity)",
       title = paste("ROC Curve (AUC =", round(roc_auc, 3), ")")) +
  annotate("text", x = 0.7, y = 0.3, label = paste("AUC =", round(roc_auc, 3)),
           color = "darkorange", size = 5) +
  annotate("text", x = 0.1, y = 0.9, label = "Perfect Classifier",
           color = "grey", size = 4, angle = 45) +
  theme(plot.background = element_rect(fill = "lightgrey"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 18, face = "bold"))

### Logistic Regression

roc_curve <- roc(test_df$Churn, predict(backward_model, newdata = test_df, type = 'response'))
auc(roc_curve)

roc_curve <- roc(test_df$Churn, predict(backward_model2, newdata = test_df, type = 'response'))
auc(roc_curve)

# Removing MonthlyCharges & TotalCharges marginally decreases AUC while greatly improving interpretability
# This is a good trade off because, while we want accurate predictions,
# we more want to understand what is causing churn and how to stop it

# Extract ROC curve data points
roc_df <- data.frame(FPR = 1 - roc_curve$specificities,
                     TPR = roc_curve$sensitivities)

# Plot ROC Curve
ggplot(roc_df, aes(x = FPR, y = TPR)) +
  geom_line(color = "darkorange", size = 1.5) +
  geom_point(color = "darkorange", size = 3, shape = 16) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey") +
  theme_minimal() +
  labs(x = "False Positive Rate (1 - Specificity)", y = "True Positive Rate (Sensitivity)",
       title = paste("ROC Curve (AUC =", round(auc(roc_curve), 3), ")")) +
  annotate("text", x = 0.7, y = 0.3, label = paste("AUC =", round(auc(roc_curve), 3)),
           color = "darkorange", size = 5) +
  annotate("text", x = 0.1, y = 0.9, label = "Perfect Classifier",
           color = "grey", size = 4, angle = 45) +
  theme(plot.background = element_rect(fill = "lightgrey"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 18, face = "bold"))

##### Lift Charts (Only Logistic Regression)

# Build Lift chart for Training Data

lift_df_train <- data.frame(
  target = as.numeric(train_df$Churn),
  predicted_prob = predict(backward_model2, newdata = train_df, type = 'response'),
  monthlyCharges = as.numeric(train_df$MonthlyCharges)
)

lift_df_train <- lift_df_train[order(-lift_df_train$predicted_prob), ]

total_positives <- sum(lift_df_train$target)

lift_df_train$lift <- cumsum(lift_df_train$target) / total_positives
lift_df_train$percentile <- seq(1, nrow(lift_df_train)) / nrow(lift_df_train) * 100

#lift_values_train <- lift_df_train$lift

# Plot the lift chart (train)
lift_plot_train <- ggplot(data = lift_df_train, aes(x = percentile, y = lift)) +
  geom_line() +
  labs(x = "Percentile", y = "Lift", title = "Train Lift Chart") +
  theme_minimal()

lift_plot_train

# Build Lift Chart for Testing Data
lift_df_test <- data.frame(
  target = as.numeric(test_df$Churn),
  predicted_prob = predict(backward_model2, newdata = test_df, type = 'response'),
  monthlyCharges = as.numeric(test_df$MonthlyCharges)
)

lift_df_test <- lift_df_test[order(-lift_df_test$predicted_prob), ]

total_positives <- sum(lift_df_test$target)

lift_df_test$lift <- cumsum(lift_df_test$target) / total_positives
lift_df_test$percentile <- seq(1, nrow(lift_df_test)) / nrow(lift_df_test) * 100

# Plot the lift chart using ggplot2
lift_plot_test <- ggplot(data = lift_df_test, aes(x = percentile, y = lift)) +
  geom_line() +
  labs(x = "Percentile", y = "Lift", title = "Test Lift Chart") +
  theme_minimal()

lift_plot_test

####### Profit Charts

# Define the fixed cost & probability of converting 
cost <- 5 #cost of intervention
prob <- 0.25 #this will the retention rate of targets that would churn with no intervention

##### Random Training Plot (this will change every time it is ran, prob% chance that a target will not churn)
lift_df_train <- lift_df_train %>%
  rowwise() %>%
  mutate(profit = (target * monthlyCharges * ifelse(runif(1) <= (1 - prob), 0, 1)) - cost) %>% 
  ungroup()

# Create a cumulative sum of profits to visualize the profit chart over time
lift_df_train <- lift_df_train %>%
  arrange((1-predicted_prob)) %>%
  mutate(cumulative_profit = cumsum(profit))

# Plot the profit chart
ggplot(lift_df_train, aes(x = predicted_prob, y = cumulative_profit)) +
  geom_line(color = "darkorange", size = 1.5) +
  scale_x_reverse() +
  geom_point(color = "darkorange", 
             size = 3, shape = 16) +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "lightgrey"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 18, face = "bold")) +
  xlab("Probability Threshold") +
  ylab("Cumulative Profit") +
  annotate("text", x = 0.3, y = 0.3, label = paste("Ideal Threshold =", round(lift_df_train[which.max(lift_df_train$cumulative_profit), ]$predicted_prob, 4)),
           color = "darkorange", size = 5) +
  ggtitle(paste("Random Training Profit Chart (Max Profit = $", round(max(lift_df_train$cumulative_profit), 2), ")",sep = ""))

##### Random Testing Plot (this will change every time it is ran, prob% chance that a target will not churn)
lift_df_test <- lift_df_test %>%
  rowwise() %>%
  mutate(profit = (target * monthlyCharges * ifelse(runif(1) <= (1 - prob), 0, 1)) - cost) %>% 
  ungroup()

# Create a cumulative sum of profits to visualize the profit chart over time
lift_df_test <- lift_df_test %>%
  arrange((1-predicted_prob)) %>%
  mutate(cumulative_profit = cumsum(profit))

# Plot the profit chart
ggplot(lift_df_test, aes(x = predicted_prob, y = cumulative_profit)) +
  geom_line(color = "darkorange", size = 1.5) +
  scale_x_reverse() +
  geom_point(color = "darkorange", 
             size = 3, shape = 16) +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "lightgrey"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 18, face = "bold")) +
  xlab("Probability Threshold") +
  ylab("Cumulative Profit") +
  annotate("text", x = 0.3, y = 0.3, label = paste("Ideal Threshold =", round(lift_df_test[which.max(lift_df_test$cumulative_profit), ]$predicted_prob, 4)),
           color = "darkorange", size = 5) +
  ggtitle(paste("Random Training Profit Chart (Max Profit = $", round(max(lift_df_test$cumulative_profit), 2), ")",sep = ""))


##### Estimated value training plot rather than randomness to seek an average (takes prob% of revenue for each targeted customer)

# Calculate the profit for each row using expected value (to find mean)
lift_df_train <- lift_df_train %>%
  mutate(profit = (target * monthlyCharges * prob) - cost)

# Create a cumulative sum of profits to visualize the profit chart over time
lift_df_train <- lift_df_train %>%
  arrange((1-predicted_prob)) %>%
  mutate(cumulative_profit = cumsum(profit))

# Plot the profit chart
ggplot(lift_df_train, aes(x = predicted_prob, y = cumulative_profit)) +
  geom_line(color = "darkorange", size = 1.5) +
  scale_x_reverse() +
  geom_point(color = "darkorange", 
             size = 3, shape = 16) +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "lightgrey"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 18, face = "bold")) +
  xlab("Probability Threshold") +
  ylab("Cumulative Profit") +
  annotate("text", x = 0.3, y = 0.3, label = paste("Ideal Threshold =", round(lift_df_train[which.max(lift_df_train$cumulative_profit), ]$predicted_prob, 4)),
           color = "darkorange", size = 5) +
  ggtitle(paste("Random Training Profit Chart (Max Profit = $", round(max(lift_df_train$cumulative_profit), 2), ")",sep = ""))

##### Estimated value testing plot rather than randomness to seek an average (takes prob% of revenue for each targeted customer)

# Calculate the profit for each row using expected value (to find mean)
lift_df_test <- lift_df_test %>%
  mutate(profit = (target * monthlyCharges * prob) - cost)

# Create a cumulative sum of profits to visualize the profit chart over time
lift_df_test <- lift_df_test %>%
  arrange((1-predicted_prob)) %>%
  mutate(cumulative_profit = cumsum(profit))

# Plot the profit chart
ggplot(lift_df_test, aes(x = predicted_prob, y = cumulative_profit)) +
  geom_line(color = "darkorange", size = 1.5) +
  scale_x_reverse() +
  geom_point(color = "darkorange", 
             size = 3, shape = 16) +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "lightgrey"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 18, face = "bold")) +
  xlab("Probability Threshold") +
  ylab("Cumulative Profit") +
  annotate("text", x = 0.3, y = 0.3, label = paste("Ideal Threshold =", round(lift_df_test[which.max(lift_df_test$cumulative_profit), ]$predicted_prob, 4)),
           color = "darkorange", size = 5) +
  ggtitle(paste("Random Training Profit Chart (Max Profit = $", round(max(lift_df_test$cumulative_profit), 2), ")",sep = ""))

#### Confusion Matrix Calculations

# Set the threshold (use training threshold)
predicted.model <- ifelse(predict(backward_model2,newdata=test_df,type='response') >= 0.3092, 1, 0)

# Calculate values of...
TP <- sum(test_df$Churn == predicted.model & test_df$Churn == 1)
TN <- sum(test_df$Churn == predicted.model & test_df$Churn == 0)
FN <- sum(test_df$Churn != predicted.model & test_df$Churn == 1)
FP <- sum(test_df$Churn != predicted.model & test_df$Churn == 0)

TPR <- TP/(TP + FN)
TNR <- TN/(TN + FP)
FPR <- FP/(FP + TN)
FNR <- FN/(FN + TP)

# Confusion Matrix
xtabs(~predicted.model + test_df$Churn)

