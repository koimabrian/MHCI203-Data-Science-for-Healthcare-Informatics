
# Install necessary packages
install.packages('caret')
install.packages('dplyr')
install.packages('ggplot2')
install.packages('readr')
install.packages("corrplot")

# Load the libraries
library(caret)
library(corrplot)
library(dplyr)
library(ggplot2)
library(readr)

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

# Create the correlation heatmap
wb = c('white', 'black')

par(ask = TRUE)

## different color scale and methods to display corr-matrix
corrplot(M, method = 'number', col = 'black', cl.pos = 'n')
corrplot(M, method = 'number')
corrplot(M)
corrplot(M, order = 'AOE')
corrplot(M, order = 'AOE', addCoef.col = 'grey')

corrplot(M, order = 'AOE',  cl.length = 21, addCoef.col = 'grey')
corrplot(M, order = 'AOE', col = COL2(n=10), addCoef.col = 'grey')

corrplot(M, order = 'AOE', col = COL2('PiYG'))
corrplot(M, order = 'AOE', col = COL2('PRGn'), addCoef.col = 'grey')
corrplot(M, order = 'AOE', col = COL2('PuOr', 20), cl.length = 21, addCoef.col = 'grey')
corrplot(M, order = 'AOE', col = COL2('PuOr', 10), addCoef.col = 'grey')

corrplot(M, order = 'AOE', col = COL2('RdYlBu', 100))
corrplot(M, order = 'AOE', col = COL2('RdYlBu', 10))


corrplot(M, method = 'color', col = COL2(n=20), cl.length = 21, order = 'AOE',
         addCoef.col = 'grey')
corrplot(M, method = 'square', col = COL2(n=200), order = 'AOE')

# Step 4: Data Preprocessing
# Convert the target variable to a factor
heart_data$target <-factor(heart_data$target, levels = c(0, 1))

# Identify variables with near zero variance
nzv <- nearZeroVar(heart_data)

# Remove variables with near zero variance from the dataset
heart_data <- heart_data[, -nzv]

# Step 5: Split the Data into Training and Testing Sets
# Set a random seed for reproducibility
set.seed(123)

# Create a partition for the training data (80% of the data)
trainIndex <-createDataPartition(heart_data$target, p = .8, list = FALSE)

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
# Make predictions on the testing dataset
predictions <- predict(model, newdata = test_data)

# Evaluate the model using a confusion matrix
confusionMatrix(predictions, test_data$target)

# Step 8: Visualize Model Performance
# Create a bar plot to visualize the predicted vs actual heart disease cases
ggplot(test_data, aes(x = as.factor(target), fill = predictions)) +
  geom_bar(position = 'dodge') +
  labs(title = 'Predicted vs Actual Heart Disease Cases', x = 'Actual', y = 'Count')

# Step 9: Try Another Algorithm
# Train a random forest model
rf_model <- train(target ~ ., data = train_data, method = 'rf')

# Make predictions on the testing dataset using the random forest model
rf_predictions <- predict(rf_model, newdata = test_data)

# Evaluate the random forest model using a confusion matrix
confusionMatrix(rf_predictions, test_data$target)