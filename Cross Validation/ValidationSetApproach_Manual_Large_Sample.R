#make this example reproducible
set.seed(0)

#define the dataset
data <- read.csv("insurance.csv")

#function to create partition
createDataPartition <- function(y, times, p) {
  no_of_rows <- 1:length(y)
  no_of_elements <- round(p * length(y))
  
 sampling<- function(x){
   sample_output <- list()  
   indices <- sample(no_of_rows, no_of_elements, replace = FALSE)
   sample_output[[x]] <- indices}
  
 sapply(1:times,sampling)}

training_obs <- createDataPartition(data$charges, times = 1, p = 0.8)

#dividing data into training and test
train <- data[unlist(training_obs), ]
test <- data[-unlist(training_obs), ]

# Build the linear regression model on the training set
model <- lm(charges ~ age+bmi+children+smoker+region, data = train)

# Use the model to make predictions on the test set
predictions <- model %>% predict(test)

#RMSE
mean((test$charges- predictions)^2)