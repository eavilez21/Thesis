#load dplyr library used for data manipulation
library(dplyr)

#load caret library used for partitioning data into training and test set
library(caret)

#make this example reproducible
set.seed(0)

#define the dataset
data <- mtcars[ , c("mpg", "disp", "hp", "drat")]

#split the dataset into a training set (80%) and test set (20%).
training_obs <- data$mpg %>% createDataPartition(p = 0.8, list = FALSE)

train <- data[training_obs, ]
test <- data[-training_obs, ]

# Build the linear regression model on the training set
model <- lm(mpg ~ ., data = train)

# Use the model to make predictions on the test set
predictions <- model %>% predict(test)

#Examine R-squared, RMSE, and MAE of predictions
data.frame(R_squared = R2(predictions, test$mpg),
           RMSE = RMSE(predictions, test$mpg),
           MAE = MAE(predictions, test$mpg))
