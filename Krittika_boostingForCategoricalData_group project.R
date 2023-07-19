library(caret)
library(gbm)

# Loading the data
data <- read.csv("Telco_Customer_Churn.csv")

# Data preprocessing: Converting categorical variables to factors, etc.
# (You can use a loop to simplify this process)
factor_cols <- c("Churn", "PaymentMethod", "PaperlessBilling", "Contract", "StreamingMovies",
                 "StreamingTV", "TechSupport", "DeviceProtection", "OnlineBackup", "OnlineSecurity",
                 "InternetService", "MultipleLines", "PhoneService", "Dependents", "Partner", "gender")

data[, factor_cols] <- lapply(data[, factor_cols], as.factor)

data$Churn <- ifelse(data$Churn == "Yes", 1, 0)

# Getting rid of customer ID
data = data[, -1]

# Splitting the data into training and testing sets
set.seed(123)  # For reproducibility
train_index <- createDataPartition(data$Churn, p = 0.7, list = FALSE)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# Hyperparameter tuning using a loop
idv = c(2, 5, 10)
ntv = c(1000)
lamv = c(0.01, 0.05)
parmb = expand.grid(idv, ntv, lamv)
colnames(parmb) = c('tdepth', 'ntree', 'lam')
print(parmb)
nset = nrow(parmb)
olb = rep(0, nset)
ilb = rep(0, nset)
bfitv = vector('list', nset)

for (i in 1:nset) {
  cat('doing boost ', i, ' out of ', nset, '\n')
  tempboost = gbm(Churn ~ ., data = train_data, distribution = 'bernoulli',
                  interaction.depth = parmb[i, 1], n.trees = parmb[i, 2], shrinkage = parmb[i, 3])
  ifit = predict(tempboost, n.trees = parmb[i, 2], newdata = train_data)
  ofit = predict(tempboost, n.trees = parmb[i, 2], newdata = test_data)
  
  round_ifit <- ifelse(ifit >= 0.5, 1, 0)
  round_ofit <- ifelse(ofit >= 0.5, 1, 0)
  
  # Checking for true positives (tp) and true negatives (tn)
  tp = ifelse(round_ofit == 1, ifelse(test_data$Churn == 1, 1, 0), 0)
  tn = ifelse(round_ofit == 0, ifelse(test_data$Churn == 0, 1, 0), 0)
  tp_sum = sum(tp)
  tn_sum = sum(tn)
  
  # Print accuracy based on test data for each model
  acc = (tp_sum + tn_sum) / nrow(test_data)
  print(paste("Accuracy for model", i, ":", acc))
}
