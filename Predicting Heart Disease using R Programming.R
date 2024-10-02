# Install necessary packages (including pROC)
install.packages('caret')
install.packages('dplyr')
install.packages('ggplot2')
install.packages('readr')
install.packages("corrplot")
install.packages("pROC")  # Added pROC package

# Load the libraries
library(caret)
library(corrplot)
library(dplyr)
library(ggplot2)
library(readr)
library(pROC)  # Load pROC

# Step 2: Load the Dataset
# Define the URL of the heart disease dataset
data_url <- 'https://raw.githubusercontent.com/sharmaroshan/Heart-UCI-Dataset/refs/heads/master/heart.csv'

# Read the dataset from the URL
heart_data <- read.csv(data_url)

# Display the first few rows of the dataset
head(heart_data)

# Step 3: Explore the Dataset
# Display the structure of the dataset
str(heart_data)

# Display summary statistics of the dataset
summary(heart_data)

# Check for missing values
sum(is.na(heart_data))

target_variable <- heart_data$target

# Select features (excluding target)
features <- heart_data[, 1:14] 

# Calculate correlation matrix (excluding target)
M <- cor(features)

# Print the correlation matrix
print(M)

# Create a simple correlation heatmap with values and a color scale
corrplot(M, method = 'color', addCoef.col = 'black', # Add correlation coefficients in black
         col = colorRampPalette(c("blue", "white", "red"))(200), # Color scale from blue to red
         tl.col = "black", tl.srt = 45, # Text label color and rotation
         tl.cex = 0.7, # Adjust font size for text labels (variables)
         number.cex = 0.7) # Adjust font size for correlation coefficients

# Step 4: Data Preprocessing
# Convert the target variable to a factor
heart_data$target <- factor(heart_data$target, levels = c(0, 1))

# Identify variables with near zero variance
nzv <- nearZeroVar(heart_data)

# Check if any NZV variables were found and remove them
if (length(nzv) > 0) {
  heart_data <- heart_data[, -nzv]
} else {
  print("No near zero variance predictors found.")
}

# Step 5: Split the Data into Training and Testing Sets
# Set a random seed for reproducibility
set.seed(123)

# Create a partition for the training data (80% of the data)
trainIndex <- createDataPartition(heart_data$target, p = 0.8, list = FALSE)

# Create the training dataset
train_data <- heart_data[trainIndex, ]

# Create the testing dataset
test_data <- heart_data[-trainIndex, ]

# Step 6: Build Logistic Regression Model
# Train a logistic regression model
model <- train(target ~ ., data = train_data, method = 'glm', family = 'binomial')

# Display the summary of the model
summary(model)

# Step 7: Evaluate the Model
# Make predictions on the testing dataset (class predictions)
predictions <- predict(model, newdata = test_data)

# Make predictions on the testing dataset (probabilities for ROC curve)
prob_predictions <- predict(model, newdata = test_data, type = "prob")

# Evaluate the model using a confusion matrix (for class predictions)
cm <- confusionMatrix(predictions, test_data$target)

# Print the confusion matrix to console
print(cm)

# Step to Plot Confusion Matrix for Logistic Regression
# Create a data frame from the confusion matrix
cm_data <- as.data.frame(cm$table)

# Plot the confusion matrix using ggplot2
ggplot(cm_data, aes(x = Prediction, y = Reference)) +
  geom_tile(aes(fill = Freq), color = "white") +
  geom_text(aes(label = Freq), vjust = 1) +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Confusion Matrix for Logistic Regression", 
       x = "Predicted", 
       y = "Actual") +
  theme_minimal()

# Extract feature importance from the logistic regression model
importance <- varImp(model)

# Plot feature importance
plot(importance)

# Step 8: Visualize Model Performance
# Create a bar plot to visualize the predicted vs actual heart disease cases
ggplot(test_data, aes(x = as.factor(target), fill = as.factor(predictions))) +
  geom_bar(position = "dodge") +
  labs(title = "Predicted vs Actual Heart Disease Cases (Logistic Regression)",
       x = "Actual", 
       y = "Count") +
  scale_fill_discrete(labels = c("Predicted 0", "Predicted 1"))

# Step 9: Try Another Algorithm
# Train a random forest model
rf_model <- train(target ~ ., data = train_data, method = 'rf')

# Make predictions on the testing dataset using the random forest model
rf_predictions <- predict(rf_model, newdata = test_data)

# Evaluate the random forest model using a confusion matrix
rf_cm <- confusionMatrix(rf_predictions, test_data$target)

# Print the confusion matrix to console
print(rf_cm)

# Step to Plot Confusion Matrix for Random Forest Model
# Create a data frame from the confusion matrix
rf_cm_data <- as.data.frame(rf_cm$table)

# Plot the confusion matrix using ggplot2
ggplot(rf_cm_data, aes(x = Prediction, y = Reference)) +
  geom_tile(aes(fill = Freq), color = "white") +
  geom_text(aes(label = Freq), vjust = 1) +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Confusion Matrix for Random Forest", 
       x = "Predicted", 
       y = "Actual") +
  theme_minimal()

# Step 10: Visualize Model Performance
# Create a bar plot to visualize the predicted vs actual heart disease cases for both models

# Combine predictions and actual values into a data frame for plotting
performance_data <- data.frame(
  Actual = test_data$target,
  Logistic_Regression_Predicted = predictions,
  Random_Forest_Predicted = rf_predictions
)

# Melt the data frame for ggplot2
library(reshape2)
performance_melted <- melt(performance_data, id.vars = "Actual")

# Plot for Logistic Regression predictions
ggplot(subset(performance_melted, variable == "Logistic_Regression_Predicted"), 
       aes(x = as.factor(Actual), fill = as.factor(value))) +
  geom_bar(position = "dodge") +
  labs(title = "Predicted vs Actual Heart Disease Cases (Logistic Regression)",
       x = "Actual", 
       y = "Count") +
  scale_fill_discrete(name = "Predicted", labels = c("No Disease", "Heart Disease")) +
  theme_minimal() +
  theme(legend.position = "top")

# Plot for Random Forest predictions
ggplot(subset(performance_melted, variable == "Random_Forest_Predicted"), 
       aes(x = as.factor(Actual), fill = as.factor(value))) +
  geom_bar(position = "dodge") +
  labs(title = "Predicted vs Actual Heart Disease Cases (Random Forest)",
       x = "Actual", 
       y = "Count") +
  scale_fill_discrete(name = "Predicted", labels = c("No Disease", "Heart Disease")) +
  theme_minimal() +
  theme(legend.position = "top")
