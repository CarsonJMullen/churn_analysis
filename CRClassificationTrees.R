library(caret)
library(pROC)
library(rpart)

############################## READ DATASET!! ###################################
telecomChurn = read.csv('/Users/cassieren/Documents/MSBA/Summer 2023/STA 380/Group Project/telecomChurn.csv')
telecomChurn <- telecomChurn[, !colnames(telecomChurn) %in% 'customerID']
telecomChurn <- na.omit(telecomChurn)


# SPLIT dataset into training and testing sets (70% for training, 30% for test)
set.seed(123)
trainIndex <- createDataPartition(telecomChurn$Churn, p = 0.7, list = FALSE)
trainData <- telecomChurn[trainIndex, ]
testData <- telecomChurn[-trainIndex, ]


# CROSS VALIDATION!!!! 10 FOLD
ctrl <- trainControl(method = 'cv', number = 10) #verboseIter = TRUE)
model <- train(Churn ~., data = trainData, method = 'rpart', trControl = ctrl)

#print(model)

# make predictions on test set
predicted_probabilities <- predict(model, newdata = testData, type = 'prob')[, 'Yes']
true_labels <- ifelse(testData$Churn == 'Yes', 1, 0)

#predicted_probabilities <- predict(model, newdata = testData, type = 'prob')[, 'Yes']
#true_labels <- ifelse(testData$Churn == 'Yes', 1, 0)


######################################### ROC CURVE ##########################################
roc_obj <- roc(true_labels, predicted_probabilities)  # calculate roc curve
plot(roc_obj, main = 'ROC Curve', xlab = 'False Positive Rate', ylab = 'True Positive Rate', print.auc = TRUE)
abline(a = 0, b = 1, lty = 2) # add diagonal line that represents random classification
legend('bottomright', legend = paste('AUC =', round(auc(roc_obj), 2)), cex = 0.8, bty = 'n')  # legend

################################### PRECISION/RECALL VALUES #####################################
model <- train(Churn ~. , data = trainData, method = 'rpart', trControl = ctrl)   # train model
predictions <- predict(model, newdata = testData) # make predictions on test set (must be RAW! others don't work for some reason)

confusion <- table(predictions, testData$Churn)

TP <- confusion['Yes', 'Yes']
TN <- confusion['No', 'No']
FP <- confusion['Yes', 'No']
FN <- confusion['No', 'Yes']

precision <- TP/ (TP + FP)
recall <- TP / (TP + FN)

precision
recall

######################################## ACCURACY RATE ########################################
predictions <- predict(model, newdata = testData, type = 'raw') # make predictions on test set
accuracy <- sum(predictions == testData$Churn) / length(testData$Churn)
accuracy


###################################### CONFUSION MATRIX #######################################
predictions <- predict(model, newdata = testData)
predicted_labels <- factor(predictions, levels = c('No', 'Yes'))
true_labels <- factor(testData$Churn, levels = c('No', 'Yes'))
confusion_matrix <- confusionMatrix(data = predicted_labels, reference = true_labels)
print(confusion_matrix)
plot(confusion_matrix$table,
     col = c("#F8766D", "#00BFC4"),
     main = 'Confusion Matrix',
     xlab = 'True Labels',
     ylab = 'Predicted Labels')





###################################### CV #######################################
telecomChurn = read.csv('/Users/cassieren/Documents/MSBA/Summer 2023/STA 380/Group Project/telecomChurn.csv')
telecomChurn <- telecomChurn[, !colnames(telecomChurn) %in% 'customerID']
telecomChurn <- na.omit(telecomChurn)

ctrl <- trainControl(method = 'cv', number = 10) #verboseIter = TRUE)
model <- train(Churn ~., data = telecomChurn, method = 'rpart', trControl = ctrl)

predicted_probabilities <- predict(model, newdata = telecomChurn, type = 'prob')

roc_obj <- roc(response = telecomChurn$Churn, predictor = predicted_probabilities$Yes)

plot(roc_obj, main = "ROC Curve", col = "blue", print.auc = TRUE)
abline(a = 0, b = 1, lty = 2) # add diagonal line that represents random classification
legend('bottomright', legend = paste('AUC =', round(auc(roc_obj), 2)), cex = 0.8, bty = 'n')  # legend


predicted_labels <- factor(predicted_probabilities, levels = c("No", "Yes"))
true_labels <- factor(telecomChurn$Churn, levels = c("No", "Yes"))

confusion_matrix <- confusionMatrix(predicted_labels, true_labels)

precision <- confusionMatrix$byClass["Pos Pred Value"]
recall <- confusionMatrix$byClass["Sensitivity"]
