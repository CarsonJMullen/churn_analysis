library(tidyverse)
library(dplyr)
library(caret)
library(MASS)

# Read data, make sure you set your directory
df <- read.csv('WA_Fn-UseC_-Telco-Customer-Churn.csv')
df <- na.omit(df)

# Create a function to convert "Yes" to 1 and "No" to 0
convert_to_binary <- function(column){
  ifelse(column == "Yes", as.numeric(1), as.numeric(0))
}

columns_to_convert <- c("Partner", "Dependents", "PhoneService", "OnlineSecurity", )

#Convert Yes/No columns to binary
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

df$Churn <- factor(df$Churn)

##### Building Models

#Creates a model with all variables
full_model <- glm(Churn ~ . - customerID + tenure*StreamingMovies, data = df, family = binomial)
#Creates a model with all variables & interactions b/t all variables
full_interaction_model <- glm(Churn ~ (. - customerID)^2, data = df, family = binomial)

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

###############

trainControl(method = "cv", 
             number = 10)

backward_model <- train(Churn ~ SeniorCitizen + Dependents+tenure+ MultipleLines+InternetService+OnlineSecurity+TechSupport+StreamingTV + StreamingMovies + Contract + PaperlessBilling + PaymentMethod + MonthlyCharges + TotalCharges, data = df,
                        method = "glm",
                        trControl = trainControl)


print(backward_model)

