
#Loading the libraries
library(corrplot)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(stringr)
library(tidyr)

#Reading the file
df <- read.csv("WA_Fn-UseC_-Telco-Customer-Churn.csv",header=TRUE)

#Churn Percentage
options(repr.plot.width = 6, repr.plot.height = 4)

df %>%
  group_by(Churn) %>%
  summarise(Count = n()) %>%
  mutate(percent = prop.table(Count) * 100) %>%
  ggplot(aes(reorder(Churn, -percent), percent, fill = Churn)) +
  geom_col(fill = c("#0092B2","#CC7500")) +
  geom_text(aes(label = sprintf("%.2f%%", percent)), hjust = 0.01, vjust = -0.5, size = 3) +
  theme_bw() +
  xlab("Churn") +
  ylab("Percent") +
  ggtitle("Churn Percent")

churn_colors <- c("#0092B2","#CC7500")

# Plot Gender
ggplot(df, aes(x = gender, fill = Churn)) +
  geom_bar() +
  scale_fill_manual(values = churn_colors)
  labs(title = "Gender vs. Churn", x = "Gender", y = "Count")

# Plot Senior Citizen
ggplot(df, aes(x = factor(SeniorCitizen), fill = Churn)) +
  geom_bar() +
  scale_fill_manual(values = churn_colors)
  scale_x_discrete(labels = c("No", "Yes")) +
  labs(title = "Senior Citizen vs. Churn", x = "Senior Citizen", y = "Count")

# Plot Partner
ggplot(df, aes(x = Partner, fill = Churn)) +
  geom_bar() +
  scale_fill_manual(values = churn_colors)
  labs(title = "Partner vs. Churn", x = "Partner", y = "Count")

# Plot Dependents
ggplot(df, aes(x = Dependents, fill = Churn)) +
  geom_bar() +
  scale_fill_manual(values = churn_colors)
  labs(title = "Dependents vs. Churn", x = "Dependents", y = "Count")

# Plot PhoneService
ggplot(df, aes(x = PhoneService, fill = Churn)) +
  geom_bar() +
  scale_fill_manual(values = churn_colors)
  labs(title = "Phone Service vs. Churn", x = "Phone Service", y = "Count")

# Plot MultipleLines
ggplot(df, aes(x = MultipleLines, fill = Churn)) +
  geom_bar() +
  scale_fill_manual(values = churn_colors)
  labs(title = "Multiple Lines vs. Churn", x = "Multiple Lines", y = "Count")

# Plot InternetService
ggplot(df, aes(x = InternetService, fill = Churn)) +
  geom_bar() +
  scale_fill_manual(values = churn_colors)
  labs(title = "Internet Service vs. Churn", x = "Internet Service", y = "Count")

# Plot OnlineSecurity
ggplot(df, aes(x = OnlineSecurity, fill = Churn)) +
  geom_bar() +
  scale_fill_manual(values = churn_colors)
  labs(title = "Online Security vs. Churn", x = "Online Security", y = "Count")

# Plot OnlineBackup
ggplot(df, aes(x = OnlineBackup, fill = Churn)) +
  geom_bar() +
  scale_fill_manual(values = churn_colors)
  labs(title = "Online Backup vs. Churn", x = "Online Backup", y = "Count")

# Plot DeviceProtection
ggplot(df, aes(x = DeviceProtection, fill = Churn)) +
  geom_bar() +
  scale_fill_manual(values = churn_colors)
  labs(title = "Device Protection vs. Churn", x = "Device Protection", y = "Count")

# Plot TechSupport
ggplot(df, aes(x = TechSupport, fill = Churn)) +
  geom_bar() +
  scale_fill_manual(values = churn_colors)
  labs(title = "Tech Support vs. Churn", x = "Tech Support", y = "Count")

# Plot StreamingTV
ggplot(df, aes(x = StreamingTV, fill = Churn)) +
  geom_bar() +
  scale_fill_manual(values = churn_colors)
  labs(title = "Streaming TV vs. Churn", x = "Streaming TV", y = "Count")

# Plot StreamingMovies
ggplot(df, aes(x = StreamingMovies, fill = Churn)) +
  geom_bar() +
  scale_fill_manual(values = churn_colors)
  labs(title = "Streaming Movies vs. Churn", x = "Streaming Movies", y = "Count")

# Plot Contract
ggplot(df, aes(x = Contract, fill = Churn)) +
  geom_bar() +
  scale_fill_manual(values = churn_colors)
  labs(title = "Contract vs. Churn", x = "Contract", y = "Count")

# Plot PaperlessBilling
ggplot(df, aes(x = PaperlessBilling, fill = Churn)) +
  geom_bar() +
  scale_fill_manual(values = churn_colors)
  labs(title = "Paperless Billing vs. Churn", x = "Paperless Billing", y = "Count")

# Plot PaymentMethod
ggplot(df, aes(x = PaymentMethod, fill = Churn)) +
  geom_bar() +
  scale_fill_manual(values = churn_colors)
  labs(title = "Payment Method vs. Churn", x = "Payment Method", y = "Count")

#BoxPlot
options(repr.plot.width = 6, repr.plot.height = 2)

# Create the boxplot for Tenure
ggplot(df, aes(x = "", y = tenure)) + 
  geom_boxplot(aes(fill = Churn)) + 
  scale_fill_manual(values = churn_colors)
  theme_bw() +
  xlab(" ") +
  ylab("Tenure") +
  labs(fill = "Churn")

# Create the boxplot for MonthlyCharges
ggplot(df, aes(x = "", y = MonthlyCharges, fill = Churn)) + 
  geom_boxplot() + 
  scale_fill_manual(values = churn_colors)
  theme_bw() +
  xlab(" ") +
  ylab("Monthly Charges") +
  labs(fill = "Churn")

#Cleaning TotalCHarges
df_1 <- na.omit(df[,"TotalCharges"])
count_column <- length(df$TotalCharges[!is.na(df$TotalCharges)])
count_column
glimpse(df)

# Create the boxplot for TotalCharges
ggplot(df, aes(x = "", y = TotalCharges, fill = Churn)) + 
  geom_boxplot() + 
  scale_fill_manual(values = churn_colors)
  theme_bw() +
  xlab(" ") +
  ylab("Total Charges") +
  labs(fill = "Churn")




