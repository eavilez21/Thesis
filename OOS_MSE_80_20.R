library(caret)
library(dplyr)

node_2 <- readRDS("node_2.rds")
data <- insurance

training_obs <- node_2$charges %>% createDataPartition(p = 0.8, list = FALSE)

train <- data[training_obs, ]
test <- data[-training_obs, ]

# Build the linear regression model on the training set
model <- lm(charges ~ ., data = train)

# Use the model to make predictions on the test set
predictions <- model %>% predict(test)

#Examine R-squared, RMSE, and MAE of predictions
data.frame(R_squared = R2(predictions, test$charges),
           RMSE = RMSE(predictions, test$charges), 
           MAE = MAE(predictions, test$charges))

