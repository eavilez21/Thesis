#make this example reproducible
set.seed(0)

#define the dataset
data <- mtcars[ , c("mpg", "disp", "hp", "drat")]


data<-data[sample(nrow(data)),]

KFold=function(){
  
  #create 10 folds
  folds <- cut(seq(1,nrow(data)),breaks=10,labels=FALSE)
  score=list()
 
  test_indices <- which(folds==i,arr.ind=TRUE)
  test<- data[test_indices, ]
  train<- data[-test_indices, ]
  model <- lm(mpg ~ ., data = train)
  predictions <- predict(model, test)
  score[[i]]<-((test$mpg-prediction)^2)}

MSE_per_i=sapply(1:10,KFold)
sqrt(mean(unlist(MSE_per_i)))