library(rpart)
data <- read.csv("C:/Users/24ad026/Documents/eye_cancer_dataset.csv")
colnames(data)
data$Cancer <- factor(data$Cancer)
set.seed(123)

n <- nrow(data)
train_index <- sample(1:n, size = 0.7*n)

train_data <- data[train_index, ]
test_data  <- data[-train_index, ]
dt_model <- rpart(Cancer ~ ., data = train_data, method = "class")
dt_pred <- predict(dt_model, test_data, type="class")
accuracy <- mean(dt_pred == test_data$Cancer)
print(accuracy)

set.seed(123)
data <- data.frame(
  Age = sample(20:70, 100, replace=TRUE),
  Tumor_Size = round(runif(100, 0.5, 5),1),
  Vision_Loss = sample(0:1, 100, replace=TRUE),
  Redness = sample(0:1, 100, replace=TRUE),
  Family_History = sample(0:1, 100, replace=TRUE),
  Cancer = sample(0:1, 100, replace=TRUE)
)
data$Cancer <- factor(data$Cancer, levels=c(0,1), labels=c("No","Yes"))

n <- nrow(data)
train_index <- sample(1:n, size = 0.7*n)
train_data <- data[train_index, ]
test_data  <- data[-train_index, ]

library(rpart)
dt_model <- rpart(Cancer ~ ., data = train_data, method="class")
dt_pred <- predict(dt_model, test_data, type="class")

accuracy <- mean(dt_pred == test_data$Cancer)
print(paste("Accuracy:", round(accuracy*100,2), "%"))

library(rpart.plot)
rpart.plot(dt_model)