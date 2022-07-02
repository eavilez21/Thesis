# This is an implementation of predictive and predictive-deviance oriented segmentation and is intended to be executedin Rstudio

# Data Preparation
set.seed(3498348)
insurance <- read.csv("data/insurance.csv")
insurance <- insurance[sample(nrow(insurance)),]
insurance <- insurance[sample(nrow(insurance)),]
insurance$sex <- as.factor(insurance$sex)
insurance$smoker <- as.factor(insurance$smoker)
insurance$region <- as.factor(insurance$region)

# Find predictive deviants using LOOCV
pd_fold <- function(i, nfold, model, outcome, dataset, fitted_scores){
  folds <- cut(1:nrow(dataset), breaks=nfold, labels = FALSE)
  test_indices <- which(folds == i, arr.ind = TRUE)
  test  <- dataset[test_indices, ]
  train <- dataset[-test_indices, ]
  trained <- lm(model, data = train)
  predictions <- predict(trained, test)

  fitted_scores[test_indices] - predictions # PD
}


kfold_pd <- function(model, outcome, dataset, nfold=NULL) {
  if (is.null(nfold)) {
    nfold <- nrow(dataset)}

  fitted_model <- lm(model, data = dataset)
  hetero<-lmtest::bptest(fitted_model)
  fitted_scores <- predict(fitted_model, dataset)
  unlist(sapply(1:nfold, pd_fold, nfold=nfold, 
                model = model, outcome=outcome, dataset = dataset, 
                fitted_scores = fitted_scores))
}

pd_loocv <- kfold_pd(charges ~ ., outcome='charges', dataset = insurance)

# PD Plot
sorted_pd <- sort(pd_loocv)
pd_95 <- quantile(sorted_pd, probs = c(0.025, 0.975))
plot(sorted_pd,col=rgb(0.7, 0.7, 0.7, 0.5), pch=19, xlab = "Cases", ylab = "Predictive Deviance", main = "Sorted Predictive Deviance")
abline(h=pd_95)

# Deviance Tree
library(rpart)
library(rattle)
library(rpart.plot)
library(data.tree)

# Find deviant nodes
insurance_rpart <- rpart(pd_loocv ~ ., data = insurance, minsplit = 2, minbucket = 1, cp = 0)

frame_deviants <- (insurance_rpart$frame$yval <= pd_95["2.5%"]) | (insurance_rpart$frame$yval >= pd_95["97.5%"])
all_deviants <- insurance_rpart$frame[frame_deviants, ]
deviant_nodes <- all_deviants[all_deviants$var != "<leaf>", ]
head(deviant_nodes)

# Use semcoa package to find deviant groups
library(semcoa)
library(purrr)
insurance_pd <- cbind(insurance, PD = pd_loocv)
predictions <- list(pd_data = insurance_pd, PD = pd_loocv)

dtree <- deviance_tree(predictions)
grouped_deviants <- unname(unlist(dtree$deviant_groups))
unique_deviants <- dtree$unique_deviants
all_deviants <- c(grouped_deviants, unique_deviants)

# Find total number of cases for a set of groups
no_deviant_group_cases <- function(groups,original_rpart_dataset,deviance_tree_object){

  groups<-toupper(groups)
  no_by_group<-sapply(groups, function(x){group_nodes_index<-deviance_tree_object$deviant_groups[x]
    group_cases<-sapply(group_nodes_index,function(i) original_rpart_dataset$frame[i, ]$n)
    reduce(group_cases, sum)}) 
  
    print(no_by_group)
    cat("Total number of cases for this set of groups is:", reduce(no_by_group,sum))
}

no_deviant_group_cases(c("A", "B", "C", "D", "E"),insurance_rpart, dtree)


# Model less deviants
new_insurance <- insurance[-all_deviants, ]
new_pd_loocv <- kfold_pd(charges ~ ., outcome = 'charges', dataset = new_insurance)

new_sorted_pd <- sort(new_pd_loocv)
new_pd_95 <- quantile(new_sorted_pd, probs = c(0.025, 0.975))
plot(new_sorted_pd, col = rgb(0.7, 0.7, 0.7, 0.5), pch = 19, main = "Sorted Model without Deviants")
abline(h = new_pd_95)

library(rpart)
new_insurance_rpart <- rpart(new_pd_loocv ~ ., data = new_insurance, minsplit = 2, minbucket = 1, cp = 0)
node_leaves <- semcoa:::tree_node_cases(new_insurance_rpart) # WARNING: long-running
length(node_leaves$`2`)

# Get average PD of all cases under a node
node_pd <- function(node_name, node_leaves, cases_pd){
  mean(na.omit(new_pd_loocv[as.character(na.omit(node_leaves[[node_name]]))]))
}

# Find the number of cases that belong to a node
cases_per_node<-function(x){
  z<-c()
  number_of_cases<-sapply(1:length(x),function(y){
    z[[y]]<<-data.frame("NodeIndex" = match(x[y], node_leaves), "NodeName" = names(x[y]), "TotalNodeCases" = length(x[[y]]))} )
    return(z)
}

cases_per_node(c(node_leaves[900], node_leaves[1000]))

# Run regression on cases(subset) of a node
library(broom)
subsets_models <- sapply(1:length(node_leaves), function(x){
  if(length(node_leaves[[x]]) > 6){
  node_subset_elements <- insurance[node_leaves[[x]],]
  node_subset_elements_number <- sapply(lapply(node_subset_elements, unique), length)
  node_model <- lm(charges~., data = node_subset_elements[, node_subset_elements_number>1])
  model_results <- data.frame(names(node_leaves[x]), t(node_model$coefficients), summary(node_model)$r.squared)
  
}})

node_lm_results <- data.table::rbindlist(subsets_models, fill = TRUE)
colnames(node_lm_results)[1] <- "nodename"
colnames(node_lm_results)[2] <- "intercept"
colnames(node_lm_results)[11]<- "r-squared"
write.csv(node_lm_results,"node_lm_results.csv", row.names = FALSE)

# Pruned insurance tree with PD
simple_new_insurance_rpart <- rpart(new_pd_loocv ~ ., data = new_insurance)
fancyRpartPlot(simple_new_insurance_rpart, caption = NULL)
simple_node_leaves <- semcoa:::tree_node_cases(simple_new_insurance_rpart)

library(data.tree)
simple_new_datatree <- as.Node(simple_new_insurance_rpart)

# Find the nodes that belong to a level
library(dplyr)
level_combinations <- function(data_tree_object){  
  library(stringr)
  return_level<-c()
  
  # Convert the data.tree object into a dataframe and sort the dataframe
  to_dataframe <- ToDataFrameTree(data_tree_object,'name', 'level', 'rpart.id', 'isLeaf', children = function(x){sapply(x$children, function(child)child$rpart.id)})
  sorted_levels <- to_dataframe[order(to_dataframe$level), ]
 
  total_level_children <- list()
  leaves <- NULL
  
  # Loop through levels of a tree
  for(i in 1:(max(sorted_levels$level) - 1)){
    
    # Current level's children
    level_children <- na.omit(sorted_levels[sorted_levels$level == i, ]$children)
    level_children <- str_trim(unlist(strsplit(level_children,split = ",")))
    
    # Array of all children in a tree
    total_level_children <- append(total_level_children,list(level_children))
    
    
    # Ensure that there is a previous level
    if(length(total_level_children) >= 1)
    {
      # Loop through previous level
      sapply(total_level_children[i-1],function(x){
             
        # Loop through children in previous level
        sapply(x, function(y){
            
        # Loop through sorted levels to get rpart.id of leaves
          for (j in 1:nrow(sorted_levels)){
            if(sorted_levels[j, ]$rpart.id == y && sorted_levels[j, ]$isLeaf == TRUE){
              leaves <<- c(leaves, sorted_levels[j, ]$rpart.id)}
        }
      })})}  
    
  return_level[[i+1]] <- noquote(c(level_children, leaves))
  return_level[[1]] <- sorted_levels[1, ]$rpart.id
  }
  
  return(return_level)
}

all_levels <- level_combinations(simple_new_datatree)

# Find average r-squared per level
levels_rsquared <- sapply(all_levels, function(x){ sapply(x, function(y){node_lm_results[node_lm_results$nodename == y, ]$`r-squared`})} )
average_levels_rsquared <- sapply(levels_rsquared, function(z)(reduce(z, sum))/length(z))
plot(average_levels_rsquared, type = "o", xlab = "Level", ylab = "Average R-squared", col = "blue", main = "Average R-squared per Level")
 
# Find average PD per level
levels_pd <- sapply(all_levels, function(x){ sapply(x, function(y) {node_pd(y, node_leaves, new_pd_loocv)})} )
average_levels_pd <- sapply(levels_pd, mean)
plot(average_levels_pd, type = "o", xlab = "Level", ylab = "Average PD", col = "green",main = "Average PD per Level")
abline(h = 0)

# Find out-of-sample predictive error per node
node_pe <- function(node_name, node_leaves, cases_pe){
  mean(na.omit(new_pe_loocv[as.character(na.omit(node_leaves[[node_name]]))]))
}

# Find predictive error using LOOCV
pe_fold <- function(i, nfold, model, outcome, dataset){
  folds <- cut(1:nrow(dataset), breaks=nfold, labels = FALSE)
  test_indices <- which(folds == i, arr.ind = TRUE)
  test  <- dataset[test_indices, ]
  train <- dataset[-test_indices, ]
  trained <- lm(model, data = train)
  print(test[, outcome][1])
  predicted_outcome <- predict(trained, test)

  pe <- (test[, outcome] - predicted_outcome)^2
}

kfold_pe <- function(model, outcome, dataset, nfold = NULL) {
  if (is.null(nfold)) {
    nfold <- nrow(dataset)
  }
  unlist(sapply(1:nfold, pe_fold, nfold = nfold, 
                model = model, outcome =outcome, dataset = dataset))
}

new_pe_loocv <- kfold_pe(charges ~ ., outcome = 'charges', dataset = new_insurance)


node_pe("2",node_leaves,new_pe_loocv)

levels_pe <- sapply(all_levels, function(x){ sapply(x, function(y) {node_pe(y, node_leaves, new_pe_loocv)})} )
average_levels_pe <- sapply(levels_pe, function (x) mean(x))

plot(average_levels_pe, type = "o", xlab = "Level", ylab = "Average PE", col = "green", main="Average PE per Level")

# Run the model on predictive segments
PD_segments_model <- function(node_names,dataset,model){
  node_cases <- sapply(node_names, function (element)lm(model, data = dataset[node_leaves[[element]], ]))
  node_cases
}

PD_segments_model(c("3", "4", "5"), dataset = insurance, model = charges~.)

lm(charges~.,data = insurance)
lm(charges~.,data = insurance[node_leaves[["3"]], ])
lm(charges~.,data = insurance[node_leaves[["4"]], ])
lm(charges~.,data = insurance[node_leaves[["5"]], ])

node_2_data <- insurance[node_leaves[["2"]],]
saveRDS(node_2_data,file = "node_2.rds")
