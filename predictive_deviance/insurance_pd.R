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
library(data.tree)
library(tree)
# library(RColorBrewer)
insurance_rpart <- rpart(pd_loocv~., data=insurance, minsplit=2, minbucket=1,cp=.00000000000001)
fancyRpartPlot(insurance_rpart,caption=NULL)

insurance_data_tree <- as.Node(insurance_rpart, digits = getOption("digits") - 1, use.n=TRUE)

# library(tree)
# insurance_tree <- tree(pd_loocv~., data=insurance, mindev = 0.000000001)

frame_deviants <- (insurance_rpart$frame$yval <= pd_95["2.5%"]) | (insurance_rpart$frame$yval >= pd_95["97.5%"])
# nodes_only <- insurance_rpart$frame[insurance_rpart$frame$var != "<leaf>", ]
all_deviants <- insurance_rpart$frame[frame_deviants, ]
deviant_nodes <- all_deviants[all_deviants$var != "<leaf>", ]


# Using semcoa package to find deviant groups
library(semcoa)
insurance_pd <- cbind(insurance, PD=pd_loocv)
predictions <- list(
  pd_data = insurance_pd,
  PD = pd_loocv
)

dtree <- deviance_tree(predictions)

grouped_deviants <- unname(unlist(dtree$deviant_groups))
unique_deviants <- dtree$unique_deviants
all_deviants <- c(grouped_deviants, unique_deviants)


#Model less deviants
new_insurance<-insurance[-all_deviants]
new_pd_loocv <- kfold_pd(charges ~ ., outcome='charges', dataset = new_insurance)
new_insurance_rpart<-rpart(new_pd_loocv~., data=new_insurance, minsplit=2, minbucket=1,cp=0)
fancyRpartPlot(new_insurance_rpart, caption= NULL)
# TODO
# - 4. Let's remove the deviant groups and rerun the deviance tree
#   - 4.1. Make a new dataset without all_deviants
#   - 4.2. Compute PD values for the new dataset
#   - 4.3. Fit a new rpart tree on it and examine the structure (can use deviance_tree function)
# - 5. Let's see if the resulting deviance tree has natural large "subgroups"

# DONE
# - 1. Use "tree" instead of "rpart" to do the deviance tree
#   - see if we can "traverse" the tree easily from the root
#     e.g. can we find node 7 from the root
#   result: no
# - 2. Try splitting the tree until there is only 1 case in each leaf node (try with "tree" package)
#   - might have to play with "cp" or complexity parameter
#   - might have to find a non-visual way of confirming there is only 1 case in each leaf
#   result: no, can only do with rpart
# - 3. Deviant Groups: Find nodes whose ultimate leaf cases have average PD in top/bottom 5% extreme
#   - Find the "major" deviant groups (ignore any deviant subgroups)
#   result: done!