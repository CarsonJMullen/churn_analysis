library(tidyverse)
library(dplyr)
library(caret)
library(MASS)
library(pROC)
library(ggplot2)

# Read data, make sure you set your directory
df <- read.csv('WA_Fn-UseC_-Telco-Customer-Churn.csv')
df <- na.omit(df)

# Create a function to convert "Yes" to 1 and "No" to 0
convert_to_binary <- function(column){
  ifelse(column == "Yes", as.numeric(1), as.numeric(0))
}

# Locate columns to convert to binary numbers 
columns_to_convert <- c("Partner", "Dependents", "PhoneService", "OnlineSecurity", 
                        "OnlineBackup", "DeviceProtection", "TechSupport", 
                        "StreamingTV", "StreamingMovies", "PaperlessBilling", 
                        "MultipleLines", "Churn")

# Use lapply to apply the convert_to_binary function to each column in the list
df[columns_to_convert] <- lapply(df[columns_to_convert], convert_to_binary)
df$gender = ifelse(df$gender == "Female", 1, 0)
df$Churn <- factor(df$Churn)

##### Split into Test/Train Split
set.seed(123)
trainIndex <- createDataPartition(df$Churn, p = 0.7, list = FALSE)
train_df <- df[trainIndex, ]
test_df <- df[-trainIndex, ]

##### Building Models

#Creates a model with all variables
full_model <- glm(Churn ~ . - customerID - TotalCharges+ tenure*StreamingMovies, data = train_df, family = binomial)
#Creates a model with all variables & interactions b/t all variables
full_interaction_model <- glm(Churn ~ (. - customerID)^2, data = train_df, family = binomial)

#Summaries
summary(full_model)
summary(full_interaction_model)

#Backward Selection
backward_model <- step(full_model, direction = "backward")

#Forward Selection
null_model <- glm(Churn ~ 1, data = df, family = binomial)
forward_model <- stepAIC(null_model, direction = "forward", scope = formula(full_model), trace = FALSE)

#Stepwise Selection
stepwise_model <- step(forward_model, scope=formula(full_model), direction="both")

summary(backward_model)
summary(forward_model)
summary(stepwise_model)

backward_predictions <- predict(backward_model,newdata=test_df,type='response')
sum(backward_predictions %>% round == test_df$Churn)/length(test_df$Churn)

####################

# Assuming 'predictions' is the vector of predicted probabilities or scores
# and 'labels' is the vector of true binary labels (0 or 1)

# Plot values of ROC curve
roc_curve <- roc(test_df$Churn, predict(backward_model,newdata=test_df,type='response'))
plot(roc_curve, main = "ROC Curve", col = "blue", lwd = 2)

# Area under the curve for ROC
auc(roc_curve)
ci.auc(roc_curve)

# Set the threshold
predicted.model <- ifelse(predict(backward_model,newdata=test_df,type='response') >= 0.5, 1, 0)
predicted.model <- factor(predicted.model, levels = c("0", "1"))

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
cf <- confusionMatrix(data=predicted.model, reference=test_df$Churn)
print(cf)

xtabs(~predicted.model + test_df$Churn)

backward_model$coefficients

################

predict(backward_model,newdata=test_df,type='response')
test_df$Churn

lift_df <- data.frame(
  target = as.numeric(test_df$Churn) - 1,
  predicted_prob = predict(backward_model, newdata = test_df, type = 'response'),
  monthlyCharges = as.numeric(test_df$MonthlyCharges)
)

lift_df <- lift_df[order(-lift_df$predicted_prob), ]

total_positives <- sum(lift_df$target)

lift_df$lift <- cumsum(lift_df$target) / total_positives
lift_df$percentile <- seq(1, nrow(lift_df)) / nrow(lift_df) * 100

lift_values <- lift_df$lift

# Plot the lift chart using ggplot2
lift_plot <- ggplot(data = lift_df, aes(x = percentile, y = lift)) +
  geom_line() +
  labs(x = "Percentile", y = "Lift", title = "Lift Chart") +
  theme_minimal()

lift_plot

#################

# Profit Chart

# Define the fixed cost & probability of converting
cost <- 15
prob <- 0.3

# Calculate the profit for each row using randomness
lift_df <- lift_df %>%
  rowwise() %>%
  mutate(profit = (target * monthlyCharges * ifelse(runif(1) <= (1 - prob), 0, 1)) - cost) %>% 
  ungroup()

# Calculate the profit for each row using expected value (to find mean)
lift_df <- lift_df %>%
  mutate(profit = (target * monthlyCharges * prob) - cost)

# Calculate the total profit by summing up all profits
total_profit <- sum(lift_df$profit)

# Create a cumulative sum of profits to visualize the profit chart over time
lift_df <- lift_df %>%
  arrange((1-predicted_prob)) %>%
  mutate(cumulative_profit = cumsum(profit))

# Plot the profit chart
ggplot(lift_df, aes(x = percentile, y = cumulative_profit)) +
  geom_line() +
  xlab("Percentile") +
  ylab("Cumulative Profit") +
  ggtitle("Profit Chart") +
  theme_minimal()

max(lift_df$cumulative_profit)

#Threshold for maximum profit
lift_df[which.max(lift_df$cumulative_profit), ]$predicted_prob


