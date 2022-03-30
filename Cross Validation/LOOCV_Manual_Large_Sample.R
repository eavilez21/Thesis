KFold_sq_err <- function(i, nfold, model, outcome, dataset) {
  folds <- cut(1:nrow(dataset), breaks=nfold, labels=FALSE)
  test_indices <- which(folds==i, arr.ind=TRUE)
  test  <- dataset[test_indices, ]
  train <- dataset[-test_indices, ]
  trained <- lm(model, data = train)
  predictions <- predict(trained, test)

  (test[outcome]-predictions)^2
}

kfold_rmse <- function(model, outcome, dataset, nfold=NULL) {
  if (is.null(nfold)) {
    nfold <- nrow(dataset)
  }
  
  sq_err <- unlist(sapply(1:nfold, KFold_sq_err, nfold=nfold, 
                          model=model, outcome=outcome, dataset=dataset))
  sqrt(mean(sq_err))
}

set.seed(3498348)
insurance <- read.csv("data/insurance.csv")
insurance <- insurance[sample(nrow(insurance)),]
insurance$sex <- as.factor(insurance$sex)
insurance$smoker <- as.factor(insurance$smoker)
insurance$region <- as.factor(insurance$region)

lm(charges ~ ., data = insurance)

kfold_rmse(charges ~ ., outcome='charges', dataset = insurance, nfold=5)
kfold_rmse(charges ~ ., outcome='charges', dataset = insurance, nfold=10)
kfold_rmse(charges ~ ., outcome='charges', dataset = insurance) # LOOCV
