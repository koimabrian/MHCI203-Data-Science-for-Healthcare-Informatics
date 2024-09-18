# Create an empty character list
char_list <- character(length = 0) 

# Create a numeric list with length 10
num_list <- numeric(length = 10) 

# Create a logical list with length 3
log_list <- logical(length = 3) 

# Create a vector with TRUE and FALSE values
log_list_2 <- c(TRUE, FALSE, FALSE, TRUE, TRUE, TRUE) 

# Select the 5th element of char_list_2
char_list_2[5] 

# Select elements from the 2nd to the 4th position of log_list_2
log_list_2[2:4] 

# Select elements from the 3rd position to the end of num_list_2
num_list_2[3:length(num_list_2)] 

# Sort a vector in descending order using sort() function
sort(test, decreasing = T) 

# Sort a vector in descending order using order() function
test[order(test, decreasing = T)] 

# Concatenate two vectors
new_num_vect <- c(num_list, num_list_2) 

# Concatenate two vectors with different data types
new_combo_vect <- c(num_list_2, log_list) 

# Create a matrix
matr <- matrix(data = c(1,3,5,7,NA,11), nrow = 2, ncol = 3) 

# Get the class of the matrix
class(matr) 

# Get the data type of the matrix
typeof(matr) 

# Calculate the mean of a column in a data frame
mean(cars_data$mpg) 

# Calculate the median of a column in a data frame
median(cars_data$cyl) 

# Check if a data frame is a list
is.list(cars_data[1,]) 

# Check if a data frame is a list
is.list(mtcars) 

# Get the length of a data frame
length(mtcars) 

# Get the length of the column names of a data frame
length(colnames(mtcars)) 

# Subset a data frame by keeping specific columns
cars_data[c(1,3)] 

# Subset a data frame by removing specific columns
cars_data[-c(1,3)] 

# Subset a data frame by removing specific columns
cars_data[,-c(1,3)] 

# Subset a data frame by removing duplicate values in a column
cars_data[!duplicated(cars_data$mpg), ] 

# Subset a data frame based on a condition
subset(cars_data, mpg < 19) 

# Subset a data frame based on a condition
cars_data[cars_data$mpg < 19, ] 

# Subset a data frame based on a condition using which() function
cars_data[which(cars_data$mpg < 19), ] 

# Subset a data frame based on multiple conditions
cars_data[cars_data$mpg > 20 & cars_data$am == 1, ] 

# Subset a data frame based on a pattern match in row names
cars_data[grep("Merc", row.names(cars_data), value=T), ] 

# Create a new data frame based on a condition
low_mpg <- cars_data[cars_data$mpg < 15, ] 

# Create a new data frame based on a condition
high_mpg <- cars_data[cars_data$mpg >= 15, ] 

# Combine two data frames
mpg_join <- rbind(low_mpg, high_mpg) 

# Create a data frame with random values
car_condition <- data.frame(sample(c("old","new"), replace = T, size = 32)) 

# Set the name of a data frame
names(car_condition) <- "condition" 

# Set the column names of a data frame
colnames(car_condition) <- "condition" 

# Set the row names of a data frame
rownames(car_condition) <- rownames(cars_data) 

# Combine two data frames using cbind() function
mpg_join <- cbind(mpg_join, car_condition) 