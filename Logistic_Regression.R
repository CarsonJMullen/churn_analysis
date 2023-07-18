library(tidyverse)
library(dplyr)
library(caret)

df <- WA_Fn.UseC_.Telco.Customer.Churn

# Create a function to convert "Yes" to 1 and "No" to 0
convert_to_binary <- function(column){
  ifelse(column == "Yes", as.numeric(1), as.numeric(0))
}

df$Partner <- convert_to_binary(df$Partner)
df$Dependents <- convert_to_binary(df$Dependents)
df$PhoneService <- convert_to_binary(df$PhoneService)
df$OnlineSecurity <- convert_to_binary(df$OnlineSecurity)
df$OnlineBackup <- convert_to_binary(df$OnlineBackup)
df$DeviceProtection <- convert_to_binary(df$DeviceProtection)
df$TechSupport <- convert_to_binary(df$TechSupport)
df$StreamingTV <- convert_to_binary(df$StreamingTV)
df$StreamingMovies <- convert_to_binary(df$StreamingMovies)
df$PaperlessBilling <- convert_to_binary(df$PaperlessBilling)
df$MultipleLines <- convert_to_binary(df$MultipleLines)
df$Churn <- convert_to_binary(df$Churn)
df$gender = ifelse(df$gender == "Female", 1, 0)

df$InternetService <- as.factor(df$InternetService)
df$Contract <- as.factor(df$Contract)
df$PaymentMethod <- as.factor(df$PaymentMethod)

test <- glm(Churn ~ TotalCharges, data = df, family = binomial)

summary(test)


add_dummys <- df[, c("InternetService", "Contract", "PaymentMethod")]

dummy_data <- dummyVars(~ ., data = add_dummys)
df_with_dummies <- data.frame(predict(dummy_data, newdata = add_dummys))
df_without_dummies <- df %>% 
  select(-any_of(names(add_dummys)))

full_df <- cbind(df_without_dummies, df_with_dummies)

model1 <- glm(Churn ~ . - customerID - MonthlyCharges, data = full_df, family = binomial)



backward_model <- step(model1, direction = "backward")

# Cross Validate

# Assuming you have the 'caret' package installed
library(caret)

# Assuming you have a data frame 'full_df' with predictor variables (excluding Churn and customerID) and a response variable 'Churn'.
predictors <- full_df %>% select(-c(Churn, customerID))
response <- full_df$Churn

# Create the trainControl object for 10-fold cross-validation
ctrl <- trainControl(method = "cv", number = 10)  # 10-fold cross-validation

# Fit the logistic regression model using cross-validation
model <- train(predictors, response, method = "glm", trControl = ctrl)

# Access the cross-validation results
print(summary(model))
conf_matrix <- confusionMatrix(predict(model, predictors), response)
print(conf_matrix)

df_without_dummies <- df_without_dummies %>% 
  select(-customerID)

cor(df_without_dummies)

# Load necessary libraries
library(caret)

# Assuming you have a data frame 'full_df' with predictor variables (excluding Churn and customerID) and a response variable 'Churn'.
predictors <- full_df %>% select(-c(Churn, customerID))
response <- full_df$Churn

# Create the trainControl object for 10-fold cross-validation
ctrl <- trainControl(method = "cv", number = 10)  # 10-fold cross-validation


