# R Data Analysis Practice

# Load libraries
library(ggplot2)
library(dplyr)

# Load built-in dataset
data(mtcars)

# View first rows
head(mtcars)

# Summary statistics
summary(mtcars)

# Correlation between mpg and horsepower
cor(mtcars$mpg, mtcars$hp)

# T-test comparing automatic vs manual transmission
t.test(mpg ~ am, data = mtcars)

# Scatter plot with regression line
ggplot(mtcars, aes(x = hp, y = mpg)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "MPG vs Horsepower",
       x = "Horsepower",
       y = "Miles per Gallon")