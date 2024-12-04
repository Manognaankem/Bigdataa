mtcars <- read.csv(file.choose())
print(mtcars) 
 

data(mtcars)


linear_model <- lm(mpg ~ hp, data = mtcars)
summary(linear_model)

# Linear regression: Predict mpg based on hp
linear_model <- lm(mpg ~ hp, data = mtcars)
summary(linear_model)

# Scatter plot with regression line
plot(mtcars$hp, mtcars$mpg, pch = 16, col = 'blue', main = "Linear Regression", xlab = "Horsepower", ylab = "Miles per Gallon")
abline(linear_model, col = 'red', lwd = 2)

# Example dataset: mtcars
data(mtcars)
# Polynomial regression: Predict mpg based on hp (2nd degree)
polynomial_model <- lm(mpg ~ poly(hp, 2), data = mtcars)
summary(polynomial_model)
# Scatter plot with polynomial regression line
plot(mtcars$hp, mtcars$mpg, pch = 16, col = 'blue', main = "Polynomial Regression", xlab = "Horsepower", ylab = "Miles per Gallon")
curve(predict(polynomial_model, data.frame(hp = x)), add = TRUE, col = 'red', lwd = 2)
# Scatter plot with polynomial regression line
# Example dataset: mtcars
# Let's convert mpg to a binary outcome: 1 if mpg > 20, otherwise 0
mtcars$mpg_binary <- ifelse(mtcars$mpg > 20, 1, 0)
# Logistic regression: Predict mpg_binary based on hp
logistic_model <- glm(mpg_binary ~ hp, data = mtcars, family = binomial)
summary(logistic_model)
# Plot with logistic regression curve
plot(mtcars$hp, mtcars$mpg_binary, pch = 16, col = 'blue', main = "Logistic Regression", xlab = "Horsepower", ylab = "Probability of mpg > 20")
curve(predict(logistic_model, data.frame(hp = x), type = "response"), add = TRUE, col = 'red', lwd = 2)


