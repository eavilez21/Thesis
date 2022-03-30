
#define the dataset
cars_data <- mtcars[ , c("mpg", "disp", "hp", "drat")]

#make this example reproducible
set.seed(349872347)
cars_data <- cars_data[sample(nrow(cars_data)),]

KFold_sq_err <- function(i, nfold, model, dataset) {
  #create 10 folds
  folds <- cut(1:nrow(dataset), breaks=nfold, labels=FALSE)
  
  test_indices <- which(folds==i, arr.ind=TRUE)
  test  <- dataset[test_indices, ]
  train <- dataset[-test_indices, ]
  model <- lm(model, data = train)
  predictions <- predict(model, test)
  (test$mpg-predictions)^2
}

kfold_rmse <- function(model, dataset, nfold=NULL) {
  if (is.null(nfold)) {
    nfold <- nrow(dataset)
  }
  
  sq_err <- unlist(sapply(1:nfold, KFold_sq_err, nfold=nfold, 
                          model=model, dataset=dataset))
  sqrt(mean(sq_err))
}

kfold_rmse(mpg ~ ., outcome='mpg', cars_data, 5)
kfold_rmse(mpg ~ ., outcome='mpg', cars_data, 10)
kfold_rmse(mpg ~ ., outcome='mpg', cars_data) # LOOCV
