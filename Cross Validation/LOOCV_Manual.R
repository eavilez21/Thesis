#make this example reproducible
set.seed(0)


#define the dataset
data <- mtcars[ , c("mpg", "disp", "hp", "drat")]


LOOCV= function(i){
  score = list()
  train = data[-i,]
  test = data[i,]
   
  model = lm(mpg ~ ., data = train)
  
  prediction = predict(model, test)
  score[[i]] = ((test$mpg-prediction)^2)}
  
MSE_per_i=sapply(1:nrow(data),LOOCV)
  
sqrt(mean(unlist(MSE_per_i)))