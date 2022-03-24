
#define the dataset
cars_data <- mtcars[ , c("mpg", "disp", "hp", "drat")]

#make this example reproducible
set.seed(349872347)
cars_data <- cars_data[sample(nrow(cars_data)),]

KFold_sq_err <- function(i, nfold) {
  #create 10 folds
  folds <- cut(1:nrow(cars_data), breaks=nfold, labels=FALSE)
 
  test_indices <- which(folds==i, arr.ind=TRUE)
  test  <- cars_data[test_indices, ]
  train <- cars_data[-test_indices, ]
  model <- lm(mpg ~ ., data = train)
  predictions <- predict(model, test)
  (test$mpg-predictions)^2
}

kfold_rmse <- function(nfold) {
  sq_err <- unlist(sapply(1:nfold, KFold_sq_err, nfold=nfold))
  sqrt(mean(sq_err))
}

kfold_rmse(5)
kfold_rmse(10)
kfold_rmse(32) # LOOCV
