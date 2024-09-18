library(ggplot2)

# Scatter Plot
ggplot(mtcars, aes(x = hp, y = mpg)) +
  geom_point() +
  labs(title = "Scatter Plot of Horsepower vs. MPG",
       x = "Horsepower",
       y = "Miles per Gallon")

# Bar Plot
ggplot(mtcars, aes(x = factor(cyl))) +
  geom_bar() +
  labs(title = "Bar Plot of Cylinder Counts",
       x = "Number of Cylinders",
       y = "Count")

# Scatter Plot with Color
ggplot(mtcars, aes(x = hp, y = mpg, color = factor(cyl))) +
  geom_point() +
  labs(title = "Horsepower vs. MPG with Cylinders Colored",
       x = "Horsepower",
       y = "Miles per Gallon",
       color = "Number of Cylinders")

# Bar Plot with Labels
ggplot(mtcars, aes(x = factor(cyl), fill = factor(cyl))) +
  geom_bar() +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
  labs(title = "Bar Plot of Cylinder Counts with Labels",
       x = "Number of Cylinders",
       y = "Count")

# Histogram of Miles per Gallon
ggplot(mtcars, aes(x = mpg)) +
  geom_histogram(binwidth = 2, fill = "blue", color = "black") +
  labs(title = "Histogram of Miles per Gallon",
       x = "Miles per Gallon",
       y = "Frequency")

# Box Plot of MPG by Cylinder Count
ggplot(mtcars, aes(x = factor(cyl), y = mpg)) +
  geom_boxplot() +
  labs(title = "Box Plot of MPG by Cylinder Count",
       x = "Number of Cylinders",
       y = "Miles per Gallon")

# Violin Plot
ggplot(mtcars, aes(x = factor(cyl), y = mpg)) +
  geom_violin() +
  labs(title = "Violin Plot of MPG by Cylinder Count",
       x = "Number of Cylinders",
       y = "Miles per Gallon")

# Line Plot
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_line() +
  labs(title = "Line Plot of Weight vs MPG",
       x = "Weight",
       y = "Miles per Gallon")

# Density Plot
ggplot(mtcars, aes(x = mpg)) +
  geom_density(fill = "lightblue") +
  labs(title = "Density Plot of Miles per Gallon",
       x = "Miles per Gallon",
       y = "Density")
