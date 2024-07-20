library(dplyr)
library(rpart)
library(rpart.plot)
# Load the dataset
dataset_path <- "C:/Users/Abc/Downloads/bank+marketing/bank-additional/bank-additional/bank-additional-full.csv"

# Load the dataset into R
data <- read.csv(dataset_path, sep = ";")

# Check the structure and summary of the dataset
head(bank_data)
summary(bank_data)

# View the first few rows and summary of the dataset
head(data)
summary(data)
str(data)
# Convert categorical variables to factors if needed
data$job <- as.factor(data$job)
data$marital <- as.factor(data$marital)
data$education <- as.factor(data$education)
data$default <- as.factor(data$default)
data$housing <- as.factor(data$housing)
data$loan <- as.factor(data$loan)
data$contact <- as.factor(data$contact)
data$month <- as.factor(data$month)
data$day <- as.factor(data$day)
data$poutcome <- as.factor(data$poutcome)
data$y <- as.factor(data$y)

# Split the data into features (X) and target variable (y)
X <- subset(data, select = -c(y))
y <- data$y

# Split data into training and testing sets
set.seed(42)
train_index <- sample(1:nrow(data), 0.8 * nrow(data))
X_train <- X[train_index, ]
y_train <- y[train_index]
X_test <- X[-train_index, ]
y_test <- y[-train_index]

# Build the decision tree model
model <- rpart(y ~ ., data = data.frame(X_train, y = y_train), method = "class")

# Make predictions
predictions <- predict(model, newdata = data.frame(X_test), type = "class")

# Evaluate model performance
accuracy <- mean(predictions == y_test)
print(paste("Accuracy:", accuracy))

# Generate a confusion matrix
table(predictions, y_test)

# Plot the decision tree
library(rpart.plot)
rpart.plot(model, main = "Decision Tree for Bank Marketing Data")

