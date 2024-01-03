install.packages("caret")
install.packages("klaR")
install.packages("gains")
install.packages("e1071")
install.packages("pROC")
install.packages("readr")
install.packages("tidyverse")

data_src <- "D:/AIUB/11th Semester/Data Science/Final Project/car.csv"
CarAccept_Data <- read.csv(data_src, header = TRUE)
View(CarAccept_Data)

str(CarAccept_Data)

summary(is.na(CarAccept_Data))

null_check <- any(is.na(CarAccept_Data))
print(null_check)

my_data <- na.omit(CarAccept_Data)
summary(CarAccept_Data)

contingency_table1 <- table( CarAccept_Data$Car_Acceptability, CarAccept_Data$Buying_Price)
contingency_table2 <- table( CarAccept_Data$Car_Acceptability, CarAccept_Data$Maintenance_Price)
contingency_table3 <- table( CarAccept_Data$Car_Acceptability, CarAccept_Data$No_of_Doors)
contingency_table4 <- table( CarAccept_Data$Car_Acceptability, CarAccept_Data$Person_Capacity)
contingency_table5 <- table( CarAccept_Data$Car_Acceptability, CarAccept_Data$Size_of_Luggage)
contingency_table6 <- table( CarAccept_Data$Car_Acceptability, CarAccept_Data$Safety)

chi_square_result1 <- chisq.test(contingency_table1)
chi_square_result2 <- chisq.test(contingency_table2)
chi_square_result3 <- chisq.test(contingency_table3)
chi_square_result4 <- chisq.test(contingency_table4)
chi_square_result5 <- chisq.test(contingency_table5)
chi_square_result6 <- chisq.test(contingency_table6)

print(chi_square_result1)
print(chi_square_result2)
print(chi_square_result3)
print(chi_square_result4)
print(chi_square_result5)
print(chi_square_result6)

significance_level <- 0.05
keep_attributes <- c("Buying_Price", 
                     "Maintenance_Price",
                     "Person_Capacity",
                     "Size_of_Luggage",
                     "Safety",
                     "Car_Acceptability")

filtered_data <- CarAccept_Data[, keep_attributes]
View(filtered_data)


install.packages("caret")
library(caret)
set.seed(1)
split_CarIndex <- createDataPartition(filtered_data$Car_Acceptability, p = 0.8, list = FALSE)

train_data <- filtered_data[split_CarIndex, ]

test_data <- filtered_data[-split_CarIndex, ]

str(train_data)
str(test_data)
head(train_data)
head(test_data)


install.packages("caret")
library(caret)
library(e1071)
naive_bayes <- naiveBayes(Car_Acceptability ~ ., data = train_data)
predictions1 <- predict(naive_bayes, newdata = test_data)
predictions2<- predict(naive_bayes, newdata = train_data)
head(predictions1)
head(predictions2)
head(naive_bayes)
View(test_data)


set.seed(1)
ctrl <- trainControl(method = "cv", number = 10)

Cross_Validation <- train(Car_Acceptability ~ .,
                          data = CarAccept_Data,
                          method = "naive_bayes",
                          trControl = ctrl)
fold_accuracies <- Cross_Validation$resample$Accuracy
cat("K-Fold Accuracies:\n")
for (fold in 1:length(fold_accuracies)) {
  cat("Fold", fold, "Accuracy:", fold_accuracies[fold], "\n")
}




Confusion_Matrix <- table(predictions1, test_data$Car_Acceptability)
print("Confusion Matrix for Test Data:")
print(Confusion_Matrix)
accuracy <- sum(diag(Confusion_Matrix)) / sum(Confusion_Matrix)
print(paste("Accuracy: ", accuracy))
Confusion_Matrix2 <- table(predictions2, train_data$Car_Acceptability)
print("Confusion Matrix for Train Data:")
print(Confusion_Matrix2)
accuracy2 <- sum(diag(Confusion_Matrix2)) / sum(Confusion_Matrix2)
print(paste("Accuracy: ", accuracy2))




Confusion_Matrix <- table(predictions1, test_data$Car_Acceptability)
print("Confusion Matrix for Test Data:")
print(Confusion_Matrix)

precision <- Confusion_Matrix[2, 2] / sum(Confusion_Matrix[, 2])
print(paste("Precision:", precision))
recall <-Confusion_Matrix[2, 2] / sum(Confusion_Matrix[2, ])
print(paste("Recall:", recall))

f_measure <- 2 * (precision * recall) / (precision + recall)
print(paste("F-measure:", f_measure))



library(ggplot2)
ggplot(CarAccept_Data, aes(x = Car_Acceptability)) +
  geom_bar() +
  ggtitle("Bar Plot of Car Acceptability") +
  xlab("Acceptability") +
  ylab("Count") +
  theme_minimal()



CarAccept_Data$Buying_Price <- as.factor(CarAccept_Data$Buying_Price)
CarAccept_Data$Maintenance_Price <- as.factor(CarAccept_Data$Maintenance_Price)
CarAccept_Data$Person_Capacity <- as.factor(CarAccept_Data$Person_Capacity)
CarAccept_Data$Size_of_Luggage <- as.factor(CarAccept_Data$Size_of_Luggage)
CarAccept_Data$Safety <- as.factor(CarAccept_Data$Safety)
CarAccept_Data$Car_Acceptability <- as.factor(CarAccept_Data$Car_Acceptability)

par(mfrow = c(3, 2))  # Adjust the grid layout as needed
boxplot(Buying_Price ~ Car_Acceptability, data = CarAccept_Data, main = "Buying Price vs Acceptability")
boxplot(Maintenance_Price ~ Car_Acceptability, data = CarAccept_Data, main = "Maintenance Price vs Acceptability")
boxplot(Person_Capacity ~ Car_Acceptability, data = CarAccept_Data, main = "Person Capacity vs Acceptability")
boxplot(Size_of_Luggage ~ Car_Acceptability, data = CarAccept_Data, main = "Size of Luggage vs Acceptability")
boxplot(Safety ~ Car_Acceptability, data = CarAccept_Data, main = "Safety vs Acceptability")


ggplot(test_data, aes(x = Safety, y = Car_Acceptability, color = "Test Data")) +
  geom_point() +
  ggtitle("Scatter Plot of Test Data") +
  xlab("Sfety") +
  ylab("Car Acceptability") +
  theme_minimal()


ggplot(train_data, aes(x = Car_Acceptability)) +
  geom_bar() +
  ggtitle("Distribution of Car_Acceptability in Training Set") +
  xlab("Car_Acceptability") +
  ylab("Count") +
  theme_minimal()



ggplot(test_data, aes(x = Car_Acceptability)) +
  geom_bar() +
  ggtitle("Distribution of Car_Acceptability in Testing Set") +
  xlab("Car_Acceptability") +
  ylab("Count") +
  theme_minimal()

plot(Cross_Validation)
