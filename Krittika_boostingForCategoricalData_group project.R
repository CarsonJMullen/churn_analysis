library(caret)
library(gbm)
library(ggplot2)
library(RColorBrewer)
library(pROC)

install.packages("pROC")

# Loading the data
df <- read.csv("WA_Fn-UseC_-Telco-Customer-Churn.csv")

# Data preprocessing: Converting categorical variables to factors, etc.
factor_cols <- c("Churn", "PaymentMethod", "PaperlessBilling", "Contract", "StreamingMovies",
                 "StreamingTV", "TechSupport", "DeviceProtection", "OnlineBackup", "OnlineSecurity",
                 "InternetService", "MultipleLines", "PhoneService", "Dependents", "Partner", "gender")

df[, factor_cols] <- lapply(df[, factor_cols], as.factor)

df$Churn <- ifelse(df$Churn == "Yes", 1, 0)

# Getting rid of customer ID
df = df[, -1]

# Splitting the data into training and testing sets
set.seed(123)  # For reproducibility
train_index <- caret::createDataPartition(df$Churn, p = 0.7, list = FALSE)
train_data <- df[train_index, ]
test_data <- df[-train_index, ]

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
  tempboost = gbm(Churn ~ . , data = train_data, distribution = 'bernoulli',
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


# Final Model with Best Hyperparameters
final_boost_model <- gbm(Churn ~ . - TotalCharges -Contract, data = train_data, distribution = 'bernoulli',
                         interaction.depth = 20,
                         n.trees = 7000,
                         shrinkage = 0.001)

# Plotting Relative Importance
var_importance <- summary(final_boost_model, plotit = FALSE)
var_importance <- var_importance[order(-var_importance$rel.inf), ]

# Create a custom color palette with 19 shades of orange
n_colors <- nrow(var_importance) - 1  # Exclude the response variable
colors <- colorRampPalette(brewer.pal(9, "Oranges"))(n_colors)

# Add a grey color for the response variable
colors <- c(colors, "grey")

# Code for plotting relative importance with custom color
ggplot(var_importance, aes(x = var, y = rel.inf, fill = var)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(values = colors) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Variable", y = "Relative Importance", title = "Variable Importance")


# Plotting ROC Curve
test_prob <- predict(final_boost_model, n.trees = 7000, newdata = test_data, type = "response")
roc_curve <- roc(test_data$Churn, test_prob)
roc_auc <- auc(roc_curve)

# Extract ROC curve data points
roc_df <- data.frame(FPR = 1 - roc_curve$specificities,
                     TPR = roc_curve$sensitivities)

# Code for plotting ROC curve with customizations
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
