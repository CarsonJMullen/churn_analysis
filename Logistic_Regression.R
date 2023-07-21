library(tidyverse)
library(dplyr)
library(caret)
library(MASS)
library(pROC)

# Read data, make sure you set your directory
df <- read.csv('WA_Fn-UseC_-Telco-Customer-Churn.csv')
df <- na.omit(df)

# Create a function to convert "Yes" to 1 and "No" to 0
convert_to_binary <- function(column){
  ifelse(column == "Yes", as.numeric(1), as.numeric(0))
}

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

predicted.model <- ifelse(predict(backward_model,newdata=test_df,type='response') >= 0.2, 1, 0)

xtabs(~predicted.model + test_df$Churn)

####################

# Assuming 'predictions' is the vector of predicted probabilities or scores
# and 'labels' is the vector of true binary labels (0 or 1)

roc_curve <- roc(test_df$Churn, predict(backward_model,newdata=test_df,type='response'))
auc(roc_curve)
plot(roc_curve, main = "ROC Curve", col = "blue", lwd = 2)

TP <- sum(test_df$Churn == predicted.model & test_df$Churn == 1)
TN <- sum(test_df$Churn == predicted.model & test_df$Churn == 0)
FN <- sum(test_df$Churn != predicted.model & test_df$Churn == 1)
FP <- sum(test_df$Churn != predicted.model & test_df$Churn == 0)

TPR <- TP/(TP + FN)
TNR <- TN/(TN + FP)
FPR <- FP/(FP + TN)
FNR <- FN/(FN + TP)


