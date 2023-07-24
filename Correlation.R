library(stats)
library(corrplot)

#read-in data
df <- read.csv("WA_Fn-UseC_-Telco-Customer-Churn.csv",header=TRUE)

#----------Encoding-----------
df$gender <- as.numeric(factor(df$gender))
df$Partner <- as.numeric(factor(df$Partner))
df$PhoneService <- as.numeric(factor(df$PhoneService))
df$MultipleLines <- as.numeric(factor(df$MultipleLines))
df$InternetService <- as.numeric(factor(df$InternetService))
df$OnlineBackup <- as.numeric(factor(df$OnlineBackup))
df$DeviceProtection <- as.numeric(factor(df$DeviceProtection))
df$TechSupport <- as.numeric(factor(df$TechSupport))
df$StreamingTV <- as.numeric(factor(df$StreamingTV))
df$StreamingMovies <- as.numeric(factor(df$StreamingMovies))
df$Contract <- as.numeric(factor(df$Contract))
df$PaperlessBilling <- as.numeric(factor(df$PaperlessBilling))
df$PaymentMethod <- as.numeric(factor(df$PaymentMethod))
df$churn <- as.numeric(factor(df$Churn))
data_encode <- subset(df, select = c("gender","Partner","PhoneService",
                              "MultipleLines","InternetService","OnlineBackup",
                              "DeviceProtection","TechSupport","StreamingTV",
                              "StreamingMovies","Contract","PaperlessBilling",
                              "PaymentMethod","MonthlyCharges","TotalCharges","churn","SeniorCitizen"))


#--------------------------------------------------
df <- data_encode

#Cleaning TotalCHarges
df_1 <- na.omit(df[,"TotalCharges"])
count_column <- length(df$TotalCharges[!is.na(df$TotalCharges)])
count_column
glimpse(df)

#Correlation Matrix
cor_matrix <- cor(df)
print(cor_matrix)
#-------------------------------------
drop_column_index <- 15

# Drop the specified column from the correlation matrix
cor_matrix_without_column <- cor_matrix[-drop_column_index, -drop_column_index]

# Print the updated correlation matrix
print(cor_matrix_without_column)

neg_color <- "grey"
no_color <- "#0092B2"
pos_color <- "#CC7500"
# Create a correlation heatmap using the updated correlation matrix
corrplot(cor_matrix_without_column, method = "circle",
         col = c(neg_color, no_color, pos_color))

