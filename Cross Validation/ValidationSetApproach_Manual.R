library(magrittr)

#make this example reproducible
set.seed(43898286)

#define the dataset
cars_data <- mtcars[ , c("mpg", "disp", "hp", "drat")]
cars_data <- cars_data[sample(nrow(cars_data)),]

#function to create partition
createDataPartition <- function(y, p) {
  no_of_rows <- 1:length(y)
  no_of_elements <- round(p * length(y))
  
  sample(no_of_rows, no_of_elements, replace=FALSE)
}

training_obs <- createDataPartition(cars_data$mpg, p = 0.8)

#dividing data into training and test
train <- cars_data[training_obs, ]
test <- cars_data[-training_obs, ]

# Build the linear regression model on the training set
oos_model <- lm(mpg ~ ., data = train)

# Use the model to make predictions on the test set
oos_predictions <- oos_model %>% predict(test)

#RMSE
vs_rmse <- sqrt(mean((oos_predictions - test$mpg)^2))


# In-sample fitting
is_model <- lm(mpg ~ ., data = cars_data)
is_fitted <- is_model %>% predict(test)
fit_error <- sqrt(mean((is_fitted - test$mpg)^2))
