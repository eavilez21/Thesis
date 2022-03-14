#load dplyr library used for data manipulation
library(dplyr)

#load caret library used for partitioning data into training and test set
library(caret)

#make this example reproducible
set.seed(0)

#define the dataset
data <- mtcars[ , c("mpg", "disp", "hp", "drat")]

#define the number of subsets (or "folds") to use
train_control <- trainControl(method = "cv", number = 5)

#train the model
model <- train(mpg ~ ., data = data, method = "lm", trControl = train_control)

#Summarize the results
print(model)