#load dplyr library used for data manipulation
library(dplyr)

#load caret library used for partitioning data into training and test set
library(caret)

#make this example reproducible
set.seed(0)

#define the dataset
data <- mtcars[ , c("mpg", "disp", "hp", "drat")]

#specify that we want to use LOOCV
train_control <- trainControl(method = "LOOCV")

#train the model
model <- train(mpg ~ ., data = data, method = "lm", trControl = train_control)

#summarize the results
print(model)