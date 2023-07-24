# Load required libraries
library(randomForest)
library(pROC)  # For ROC curve

# Read data from CSV file
data <- read.csv("TelcoChurn.csv")
data <- na.omit(data)
data <- data[,-c(1)]

# Convert categorical variables to factors
categorical_vars <- c("Churn", "PaymentMethod", "PaperlessBilling", "Contract", "StreamingMovies",
                      "StreamingTV", "TechSupport", "DeviceProtection", "OnlineBackup", "OnlineSecurity",
                      "InternetService", "MultipleLines", "PhoneService", "Dependents", "Partner", "gender")
for (var in categorical_vars) {
  data[[var]] <- as.factor(data[[var]])
}

# Split the data into training and test sets
set.seed(123)
train_indices <- sample(1:nrow(data), 0.7 * nrow(data))  # 70% for training
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

# Train the random forest model
rf_model <- randomForest(Churn ~ ., data = train_data, ntree = 100, mtry = 2)

# Make predictions on the test data (class predictions and probabilities)
predictions <- predict(rf_model, newdata = test_data)
probabilities <- as.numeric(predict(rf_model, newdata = test_data, type = "response"))

# Evaluate model performance (accuracy)
accuracy <- sum(predictions == test_data$Churn) / nrow(test_data)
cat("Accuracy: ", accuracy, "\n")

# Create ROC curve
roc_curve <- roc(test_data$Churn, probabilities)

# Plot the ROC curve
plot(roc_curve, main = "ROC Curve", col = "blue", lwd = 2,
     xlab = "False Positive Rate (1 - Specificity)",  # Custom x-label
     ylab = "True Positive Rate (Sensitivity)")

# Add AUC value to the plot
auc_value <- auc(roc_curve)
text(0.6, 0.2, paste("AUC =", round(auc_value, 2)), col = "black", cex = 1.2)
