library(stats)
library(corrplot)

#read-in data
df <- read.csv("WA_Fn-UseC_-Telco-Customer-Churn.csv",header=TRUE)

#----------Encoding-----------
df$gender <- as.numeric(factor(df$gender))
df$Partner <- as.numeric(factor(df$Partner))
df$Dependents <- as.numeric(factor(df$Dependents))
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
df$Churn <- as.numeric(factor(df$Churn))
data_encode <- subset(df, select = c("gender","SeniorCitizen","Partner","Dependents","PhoneService","PaperlessBilling","MonthlyCharges","tenure","Churn"))


#--------------------------------------------------
df <- data_encode
#Correlation Matrix
cor_matrix <- cor(df)
print(cor_matrix)


neg_color <- "grey"
no_color <- "#0092B2"
pos_color <- "#CC7500"

# Create a correlation heatmap using the correlation matrix
corrplot(cor_matrix, method = "circle",
         col = c(neg_color, no_color, pos_color),
         addgrid.col = "gray50",
         tl.col = "black" )


