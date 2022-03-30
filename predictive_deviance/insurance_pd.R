set.seed(3498348)
insurance <- read.csv("data/insurance.csv")
insurance <- insurance[sample(nrow(insurance)),]
insurance$sex <- as.factor(insurance$sex)
insurance$smoker <- as.factor(insurance$smoker)
insurance$region <- as.factor(insurance$region)

KFold_pd <- function(i, nfold, model, outcome, dataset, fitted_scores) {
  folds <- cut(1:nrow(dataset), breaks=nfold, labels=FALSE)
  test_indices <- which(folds==i, arr.ind=TRUE)
  test  <- dataset[test_indices, ]
  train <- dataset[-test_indices, ]
  trained <- lm(model, data = train)
  predictions <- predict(trained, test)

  fitted_scores[test_indices] - predictions # PD
}

kfold_pd <- function(model, outcome, dataset, nfold=NULL) {
  if (is.null(nfold)) {
    nfold <- nrow(dataset)
  }

  fitted_model <- lm(model, data = dataset)
  fitted_scores <- predict(fitted_model, dataset)
  unlist(sapply(1:nfold, KFold_pd, nfold=nfold, 
                model=model, outcome=outcome, dataset=dataset, 
                fitted_scores=fitted_scores))
}

pd_5fold <- kfold_pd(charges ~ ., outcome='charges', dataset = insurance, nfold=5)
pd_10fold <- kfold_pd(charges ~ ., outcome='charges', dataset = insurance, nfold=10)
pd_loocv <- kfold_pd(charges ~ ., outcome='charges', dataset = insurance)

