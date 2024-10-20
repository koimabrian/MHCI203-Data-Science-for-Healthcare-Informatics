# Load the libraries
library(caret)
library(corrplot)
library(dplyr)
library(ggplot2)
library(readr)
library(pROC)
library(e1071) # For SVM
library(nnet)  # For Neural Network
library(rpart) # For Decision Tree
library(randomForest) # For Random Forest
library(xgboost) # For XGBoost
library(reshape2) # For data melting

# Step 1: Load the Dataset
data_url <- "https://raw.githubusercontent.com/koimabrian/Datasets/refs/heads/main/data.csv"
data <- read.csv(data_url)

# Drop the ID column and columns with NaN values
data <- data %>% select(-id)
data <- data[, colSums(is.na(data)) == 0]

# Convert the diagnosis column to a factor
data$diagnosis <- factor(data$diagnosis, levels = c("B", "M"))

# Step 3: Select only numeric columns for correlation
data_numeric <- data
data_numeric$diagnosis <- ifelse(data_numeric$diagnosis == "B", 0, 1)

# Step 4: Calculate the Correlation Matrix
cor_matrix <- cor(data_numeric, use = "complete.obs")

# Step 5: Create the Correlation Plot for the Lower Triangle
corrplot(cor_matrix, method = "color", type = "lower", 
         tl.col = "black", tl.srt = 45, 
         title = "Lower Correlation Plot", 
         mar = c(0, 0, 1, 0))

# Step 2: Split the Data into Training and Testing Sets
set.seed(123)
trainIndex <- createDataPartition(data$diagnosis, p = 0.8, list = FALSE)

# Create the training dataset
train_data <- data[trainIndex, ]
# Create the testing dataset
test_data <- data[-trainIndex, ]

# Step 3: Build and Evaluate Models
models <- list()
results <- list()

# 1. Support Vector Machine (SVM)
svm_model <- train(diagnosis ~ ., data = train_data, method = 'svmLinear')
models$svm <- svm_model
svm_predictions <- predict(svm_model, newdata = test_data)
results$svm <- confusionMatrix(svm_predictions, test_data$diagnosis)

# 2. Neural Network (NN)
nn_model <- train(diagnosis ~ ., data = train_data, method = 'nnet', trace = FALSE)
models$nn <- nn_model
nn_predictions <- predict(nn_model, newdata = test_data)
results$nn <- confusionMatrix(nn_predictions, test_data$diagnosis)

# 3. Decision Tree (DT)
dt_model <- train(diagnosis ~ ., data = train_data, method = 'rpart')
models$dt <- dt_model
dt_predictions <- predict(dt_model, newdata = test_data)
results$dt <- confusionMatrix(dt_predictions, test_data$diagnosis)

# 4. Random Forest (RF)
rf_model <- train(diagnosis ~ ., data = train_data, method = 'rf')
models$rf <- rf_model
rf_predictions <- predict(rf_model, newdata = test_data)
results$rf <- confusionMatrix(rf_predictions, test_data$diagnosis)

# 5. XGBoost (XGB)
xgb_model <- train(diagnosis ~ ., data = train_data, method = 'xgbTree')
models$xgb <- xgb_model
xgb_predictions <- predict(xgb_model, newdata = test_data)
results$xgb <- confusionMatrix(xgb_predictions, test_data$diagnosis)

# Step 4: Extract Accuracy Scores
accuracy_scores <- sapply(results, function(cm) cm$overall["Accuracy"])

# Step 5: Create a Data Frame for Accuracy Scores
accuracy_df <- data.frame(
  Model = names(accuracy_scores),
  Accuracy = as.numeric(accuracy_scores)
)

# Print the accuracy data frame
print(accuracy_df)

# Optional: Plot the accuracy scores
ggplot(accuracy_df, aes(x = Model, y = Accuracy, fill = Model)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Accuracy, 3)), vjust = -0.5) +
  labs(title = "Accuracy Scores of Different Models", y = "Accuracy", x = "Model") +
  theme_minimal()

# Step 6: Create a Combined Data Frame for Confusion Matrices
# Extract confusion matrix tables from each model's results
cm_list <- lapply(results, function(cm) as.data.frame(cm$table))

# Add a model column to each confusion matrix data frame
cm_list <- lapply(names(cm_list), function(model) {
  cm_df <- cm_list[[model]]
  cm_df$model <- model
  return(cm_df)
})

# Combine all confusion matrices into one data frame
cm_data_combined <- do.call(rbind, cm_list)

# Step 7: Plot the Confusion Matrices for All Models
ggplot(cm_data_combined, aes(x = Prediction, y = Reference)) +
  geom_tile(aes(fill = Freq), color = "white") +
  geom_text(aes(label = Freq), vjust = 1) +
  scale_fill_gradient(low = "white", high = "blue") +
  facet_wrap(~ model, ncol = 3) +
  labs(title = "Confusion Matrix for Multiple Models",
       x = "Predicted",
       y = "Actual") +
  theme_minimal() +
  theme(legend.position = "top")
