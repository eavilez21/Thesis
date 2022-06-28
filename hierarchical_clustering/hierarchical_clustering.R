#Data Preparation
set.seed(3498348)
insurance <- read.csv("data/insurance.csv")
insurance <- insurance[sample(nrow(insurance)), ]
insurance$sex <- as.factor(insurance$sex)
insurance$smoker <- as.factor(insurance$smoker)
insurance$region <- as.factor(insurance$region)

#Construct and plot hierarchical clusters                                                                                                           
distance_mat <- dist(na.omit(insurance), method = 'euclidean')
hierarchical_cluster <- hclust(distance_mat, method = "average")
plot(hierarchical_cluster, cex = 0.6, hang = -1,main = "Insurance Hierarchical Cluster")
rect.hclust(hierarchical_cluster , k = 3, border = 2:6)

#Compute R-squared for each hierarchical cluster group
hierarchical_cluster_groups<- function (dataset, no_of_segments, model){
  
  create_segments<-cutree(hierarchical_cluster, k = no_of_segments)
  segments<-table(create_segments)
  segment_names<-noquote(names(segments))
  segment_cases<-NULL
  
  library(purrr)
  library(dplyr)
  
  if (no_of_segments>1)
  {
    segment_cases <- sapply(segment_names, function(cluster){
    segment_cases_names <- names(create_segments[which(create_segments[1:nrow(dataset)] == cluster)])})

  
    segment_cases_info <- lapply(names(segment_cases), function(segment_number){
    segment_dataframe <- insurance[segment_cases[[segment_number]], ]
    column_unique_length <- sapply(lapply(segment_dataframe, unique), length)
    segment_model <- lm(model, data = segment_dataframe[, column_unique_length > 1])
    segment_rsquared<-summary(segment_model)$r.squared})

  
  average_rsquared_segment_group <- (segment_cases_info %>% reduce(`+`))/length(segment_cases_info )
  average_rsquared_segment_group}
  
  else{
    segment_model <- lm(model,data = insurance)
    segment_rsquared <- summary(segment_model)$r.squared
    segment_rsquared}
}

all_groups <- hierarchical_cluster_groups(insurance, 3, model=charges~.)

#Calculating out-of-sample MSE
pe_fold <- function(i, nfold, model, outcome, dataset) {
  folds <- cut(1:nrow(dataset), breaks = nfold, labels = FALSE)
  test_indices <- which(folds == i, arr.ind = TRUE)
  test  <- dataset[test_indices, ]
  train <- dataset[-test_indices, ]
  trained <- lm(model, data = train)
  predicted_outcome <- predict(trained, test)

  pe <- test[, outcome] - predicted_outcome
 
}
kfold_pe <- function(model, outcome, dataset, nfold = NULL) {
  if (is.null(nfold)) {
    nfold <- nrow(dataset)
  }
  unlist(sapply(1:nfold, pe_fold, nfold = nfold, 
                model = model, outcome = outcome, dataset = dataset))
}
new_pe_loocv <- kfold_pe(charges ~ ., outcome ='charges', dataset = insurance)


pe_per_segment_group <- function(no_of_clusters){
  create_segments <- cutree(hierarchical_cluster, k = no_of_clusters)
  segments <- table(create_segments)
  segment_names <- names(segments)
  segment_cases <- NULL

  segment_cases <- lapply(segment_names, function(cluster){
    segment_cases_names <- names(create_segments[which(create_segments[1:nrow(insurance)] == cluster)])
    })
  
  segment_pe <- function(node_name, segment_cases, cases_pe) {
    mean(na.omit(new_pe_loocv[as.character(na.omit(segment_cases[[node_name]]))]))
    }

  segments_pe <- sapply(1:length(segment_names), function(x){segment_pe(x, segment_cases, new_pe_loocv)})
  segments_pe_squared <- sapply(segments_pe, function (x) (x*x))
  average_segments_pe <- mean(segments_pe_squared)
  average_segments_pe
}
pe_all_segment_groups <- sapply( c(1, 2, 3, 4, 5, 7), function (number_of_segments) pe_per_segment_group(number_of_segments))
plot(pe_all_segment_groups, type = "o", xlab = "Number of Segments", ylab = "Average PE", col = "green", main="Average PE per Segments Required")



