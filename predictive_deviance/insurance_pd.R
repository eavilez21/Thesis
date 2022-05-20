set.seed(3498348)
insurance <- read.csv("data/insurance.csv")
insurance <- insurance[sample(nrow(insurance)),]
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

# pd_5fold <- kfold_pd(charges ~ ., outcome='charges', dataset = insurance, nfold=5)
# pd_10fold <- kfold_pd(charges ~ ., outcome='charges', dataset = insurance, nfold=10)
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

# library(RColorBrewer)
insurance_rpart <- rpart(pd_loocv~., data=insurance, minsplit=2, minbucket=1, cp=0)
# fancyRpartPlot(insurance_rpart,caption=NULL)

# insurance_data_tree <- as.Node(insurance_rpart, digits = getOption("digits") - 1, use.n=TRUE)

# library(tree)
# insurance_tree <- tree(pd_loocv~., data=insurance, mindev = 0.000000001)

frame_deviants <- (insurance_rpart$frame$yval <= pd_95["2.5%"]) | (insurance_rpart$frame$yval >= pd_95["97.5%"])
# nodes_only <- insurance_rpart$frame[insurance_rpart$frame$var != "<leaf>", ]
all_deviants <- insurance_rpart$frame[frame_deviants, ]
deviant_nodes <- all_deviants[all_deviants$var != "<leaf>", ]


# Using semcoa package to find deviant groups
library(semcoa)
library(purrr)
insurance_pd <- cbind(insurance, PD=pd_loocv)
predictions <- list(
  pd_data = insurance_pd,
  PD = pd_loocv
)

dtree <- deviance_tree(predictions)

grouped_deviants <- unname(unlist(dtree$deviant_groups))
unique_deviants <- dtree$unique_deviants
all_deviants <- c(grouped_deviants, unique_deviants)

#Find total number of cases for a set of groups
no_deviant_group_cases<-function(groups,original_rpart_dataset,deviance_tree_object){
   groups<-toupper(groups)
  no_by_group<-sapply(groups,function(x){group_nodes_index<-deviance_tree_object$deviant_groups[x]
               group_cases<-sapply(group_nodes_index,function(i) original_rpart_dataset$frame[i,]$n)
               reduce(group_cases,sum)}) 
  
  print(no_by_group)
  cat("Total number of cases for this set of groups is:", reduce(no_by_group,sum))}

no_deviant_group_cases(c("A","B","C","D","E"),insurance_rpart,dtree)


#Model less deviants
new_insurance <- insurance[-all_deviants, ]
new_pd_loocv <- kfold_pd(charges ~ ., outcome='charges', dataset = new_insurance)

#Finding predictive error
pe_fold <- function(i, nfold, model, outcome, dataset) {
  folds <- cut(1:nrow(dataset), breaks=nfold, labels=FALSE)
  test_indices <- which(folds==i, arr.ind=TRUE)
  test  <- dataset[test_indices, ]
  train <- dataset[-test_indices, ]
  trained <- lm(model, data = train)
  predicted_outcome <- predict(trained, test)
  
  #   # TODO: actual - prediction
  pe <- test[, outcome] - predicted_outcome
  #   # mean(pe^2) # mse_out?
}
kfold_pe <- function(model, outcome, dataset, nfold=NULL) {
  if (is.null(nfold)) {
    nfold <- nrow(dataset)
  }
  unlist(sapply(1:nfold, pe_fold, nfold=nfold, 
                model=model, outcome=outcome, dataset=dataset))
}
new_pe_loocv <- kfold_pe(charges ~ ., outcome='charges', dataset = new_insurance)


new_sorted_pd <- sort(new_pd_loocv)
new_pd_95 <- quantile(new_sorted_pd, probs = c(0.025, 0.975))
plot(new_sorted_pd, col=rgb(0.7, 0.7, 0.7, 0.5), pch=19)
abline(h=new_pd_95)


new_insurance_rpart <- rpart(new_pd_loocv~., data=new_insurance, minsplit=2, minbucket=1,cp=0)
node_leaves <- semcoa:::tree_node_cases(new_insurance_rpart) # WARNING: long-running




# Gets average PD of all cases under a node
# e.g., node_pd("1", node_leaves, new_pd_loocv)
node_pd <- function(node_name, node_leaves, cases_pd) {
  mean(na.omit(new_pd_loocv[as.character(na.omit(node_leaves[[node_name]]))]))
}

node_pe <- function(node_name, node_leaves, cases_pe) {
  mean(na.omit(new_pe_loocv[as.character(na.omit(node_leaves[[node_name]]))]))
}

#Find the number of cases that belong to a node
cases_per_node<-function(x){
  z<-c()
  number_of_cases<-sapply(1:length(x),function(y){
    z[[y]]<<-data.frame("NodeIndex"=match(x[y],node_leaves),"NodeName"=names(x[y]),"TotalNodeCases"=length(x[[y]]))} )
  
  return(z)}

cases_per_node(c(node_leaves[900],node_leaves[1000]))[1]

#Run regression on cases(subset) of a node
library(broom)
subsets_models<-sapply(1:length(node_leaves), function(x){
  if(length(node_leaves[[x]])>6){
  node_subset_elements<-insurance[node_leaves[[x]],]
  node_subset_elements_number<-sapply(lapply(node_subset_elements, unique), length)
  node_model<-lm(charges~.,data=node_subset_elements[,node_subset_elements_number>1])
  model_results<-data.frame(names(node_leaves[x]),t(node_model$coefficients),summary(node_model)$r.squared)
 }})


node_lm_results<-data.table::rbindlist(subsets_models, fill = TRUE)
colnames(node_lm_results)[1]<-"nodename"
colnames(node_lm_results)[2]<-"intercept"
colnames(node_lm_results)[11]<-"r-squared"
write.csv(node_lm_results,"node_lm_results.csv", row.names = FALSE)
# node_lm_results_list<-list(node_lm_results[,1],node_lm_results[,2:10],node_lm_results[,11])

node_lm_results

# Pruned insurance tree with PD
simple_new_insurance_rpart <- rpart(new_pd_loocv~.,data=new_insurance)
plot(simple_new_insurance_rpart)
# fancyRpartPlot(simple_new_insurance_rpart, caption= NULL)
simple_node_leaves <- semcoa:::tree_node_cases(simple_new_insurance_rpart)

library(data.tree)
simple_new_datatree <- as.Node(simple_new_insurance_rpart)

# save.image("insurance_pd_ln153.RData")
# load("insurance_pd_ln153.RData")

library(dplyr)
#Find the nodes that belong to a level
level_combinations<- function(data_tree_object){  
  library(stringr)
  return_level<-c()
  #Convert the data.tree object into a dataframe and sort the dataframe
  to_dataframe<-ToDataFrameTree(data_tree_object,'name','level','rpart.id','isLeaf',children= function(x){sapply(x$children,function(child)child$rpart.id)})
  sorted_levels<-to_dataframe[order(to_dataframe$level),]
 
  total_level_children<-list()
  leaves<-NULL
  
  #Loop through levels of a tree
  for(i in 1:(max(sorted_levels$level)-1)){
    
    #Current level's children
    level_children<- na.omit(sorted_levels[sorted_levels$level==i,]$children)
    level_children<-str_trim(unlist(strsplit(level_children,split=",")))
    
    #Array of all children in a tree
    total_level_children<-append(total_level_children,list(level_children))
    
    
    #Ensure that there is a previous level
    if(length(total_level_children)>=1)
    {
      #Loop through previous level
      sapply(total_level_children[i-1],function(x){
             
            #Loop through children in previous level
            sapply(x,function(y){
            
            #Loop through sorted levels to get rpart.id of leaves
            for (j in 1:nrow(sorted_levels)){
              if(sorted_levels[j,]$rpart.id==y && sorted_levels[j,]$isLeaf==TRUE)
                {
                    leaves<<-c(leaves,sorted_levels[j,]$rpart.id)
                }
            }
       })})}      
             return_level[[i+1]]<-noquote(c(level_children,leaves))
             return_level[[1]]<-sorted_levels[1,]$rpart.id
             #cat("Level",i+1,":",unique(level_children),unique(leaves), "\n")
             }
  return(return_level)}

all_levels <- level_combinations(simple_new_datatree)

levels_rsquared<-sapply(all_levels, function(x){ sapply(x,function(y){node_lm_results[node_lm_results$nodename==y,]$`r-squared`})} )
average_levels_rsquared<- sapply(levels_rsquared,function(z)(reduce(z,sum))/length(z))
plot(average_levels_rsquared,type="o",xlab = "Level", ylab = "Average R-squared",col="blue")
 
levels_pd <- sapply(all_levels, function(x){ sapply(x, function(y) {node_pd(y, node_leaves, new_pd_loocv)})} )
average_levels_pd <- sapply(levels_pd, mean)
# average_total_sorted_pd<-reduce(sorted_pd,sum)/length(sorted_pd)
plot(average_levels_pd, type="o", xlab = "Level", ylab = "Average PD", col="green")
abline(h=0)

levels_pe <- sapply(all_levels, function(x){ sapply(x, function(y) {node_pe(y, node_leaves, new_pe_loocv)})} )
average_levels_pe <- sapply(levels_pe, mean)
plot(average_levels_pe, type="o", xlab = "Level", ylab = "Average PE", col="green")
abline(h=l0)





# TODO: Heterogeneity
# 6. Automatically identify the combinations of 2, 3, 4, 5, 7 groups --> DONE
#   - write a function that, given the levels, will return child nodes
#     - e.g., level=2 --> nodes 2, 3
#     - e.g., level=3 --> nodes 4, 5, 3
#     - e.g., level=4 --> nodes 4, 10, 11, 3
# 8. Evaluate what makes these groups different --> DONE
#   - explanatory: coefficients, R-squares, etc.
#   - predictive: avg pd, etc. ?
# 9. How many groups are interesting? --> DOING
#   - create something like a screeplot with some metric to compare (R^2, PD, etc.) --> DONE
#   - compute kfold_mse_out and create plot
# 10. Compare our groupings with other standard heterogeneity methods
#   - ????


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
# - 4. Let's remove the deviant groups and rerun the deviance tree <NEW CONTRIBUTIONS START HERE>
#   - 4.1. Make a new dataset without all_deviants
#   - 4.2. Compute PD values for the new dataset
#   - 4.3. Fit a new rpart tree on it and examine the structure (can use deviance_tree function)
#   result: done!
# - 5. Let's see if the resulting deviance tree has natural large "subgroups"
#   result: done! finds 7 groups (N ~ 40-498)

# 7. Get the case numbers for any set of nodes from the rpart --> DONE
#     - e.g., nodes 4, 5, 3 --> list of 3 vectors (each has cases for its group) -- see semcoa
