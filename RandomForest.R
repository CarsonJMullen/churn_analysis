# Load required libraries
library(randomForest)

# Read data from CSV file
data <- read.csv("TelcoChurn.csv")
data <- na.omit(data)

# Convert categorical variables to factors
categorical_vars <- c("Churn", "PaymentMethod", "PaperlessBilling", "Contract", "StreamingMovies",
                      "StreamingTV", "TechSupport", "DeviceProtection", "OnlineBackup", "OnlineSecurity",
                      "InternetService", "MultipleLines", "PhoneService", "Dependents", "Partner", "gender")
for (var in categorical_vars) {
  data[[var]] <- as.factor(data[[var]])
}

# Split the data into training and test sets
set.seed(1)
train_indices <- sample(1:nrow(data), 0.7 * nrow(data))  # 70% for training
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

# Train the random forest model
rf_model <- randomForest(Churn ~ ., data = train_data, ntree = 100, mtry = 2)

# Make predictions on the test data
predictions <- predict(rf_model, newdata = test_data)


# Evaluate model performance
accuracy <- sum(predictions == test_data$Churn) / nrow(test_data)
cat("Accuracy: ", accuracy, "\n")

# View variable importance
varImpPlot(rf_model)
