library(dplyr)

#make this example reproducible
set.seed(90755)

#define the dataset
insurance_data <- read.csv("insurance.csv")


LOOCV<-function(i){
  #Calculate fitted construct score
  fitted_score<-list()
  train<-insurance_data[-i,]
  test<-insurance_data[i,]
  model<-lm(charges~age+bmi+children+smoker+region,data=train)
  prediction<-predict(model,test)
  fitted_score<-(test$charges-prediction)^2
  
  #Calculate predictive construct score
  predicted_score<-list()
  out_of_sample_data<-sample_n(train,500,replace=TRUE)
  out_of_sample_merge<-rbind(out_of_sample_data,test)
  out_of_sample_model<-lm(charges~age+bmi+children+smoker+region,data=out_of_sample_merge)
  out_prediction<-predict(out_of_sample_model)
  predicted_score<-(out_of_sample_merge$charges-out_prediction)^2
  
  #Calculate predictive deviance
  fitted_score-predicted_score
  }

sapply(1:nrow(insurance_data),LOOCV)

#plot(density(predictive_deviance))
#abline(v=quantile(predictive_deviance,probs = c(0.25,.975)))