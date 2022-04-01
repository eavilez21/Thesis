set.seed(3498348)
insurance <- read.csv("data/insurance.csv")
insurance <- insurance[sample(nrow(insurance)),]
insurance$sex <- as.factor(insurance$sex)
insurance$smoker <- as.factor(insurance$smoker)
insurance$region <- as.factor(insurance$region)

pd_fold <- function(i, nfold, model, outcome, dataset, fitted_scores) {
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
  unlist(sapply(1:nfold, pd_fold, nfold=nfold, 
                model=model, outcome=outcome, dataset=dataset, 
                fitted_scores=fitted_scores))
}

pd_5fold <- kfold_pd(charges ~ ., outcome='charges', dataset = insurance, nfold=5)
pd_10fold <- kfold_pd(charges ~ ., outcome='charges', dataset = insurance, nfold=10)
pd_loocv <- kfold_pd(charges ~ ., outcome='charges', dataset = insurance)

# VISUALIZATIONS

# PD Plot
sorted_pd <- sort(pd_loocv)
pd_95 <- quantile(sorted_pd, probs = c(0.025, 0.975))
plot(sorted_pd, col=rgb(0.7, 0.7, 0.7, 0.5), pch=19)
abline(h=pd_95)

# Deviance Tree
library(rpart)
library(rattle)
library(rpart.plot)
# library(RColorBrewer)
insurance_tree <- rpart(pd_loocv~., data=insurance, minsplit=2, minbucket=1)
fancyRpartPlot(insurance_tree,caption=NULL)

# TODO
# - 1. Use "tree" instead of "rpart" to do the deviance tree
#   - see if we can "traverse" the tree easily from the root
#     e.g. can we find node 7 from the root
# - 2. Try splitting the tree until there is only 1 case in each leaf node (try with "tree" package)
#   - might have to play with "cp" or complexity parameter
#   - might have to find a non-visual way of confirming there is only 1 case in each leaf
# - 3. Deviant Groups: Find nodes whose ultimate leaf cases have average PD in top/bottom 5% extreme
# - 4. Let's remove the deviant groups and rerun the deviance tree
# - 5. Let's see if the resulting deviance tree has natural large "subgroups"