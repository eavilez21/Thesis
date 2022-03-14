library(plyr)
library(readr)
library(dplyr)
library(caret)
library(rpart)
library(rpart.plot)

dat <-  MASS::Boston
glimpse(dat)

set.seed(100)
trainRowNumbers <- createDataPartition(dat$medv, p=0.7, times=1)

train <- dat[trainRowNumbers,]
test <- dat[-trainRowNumbers,]
dim(train); dim(test) 

cols=c('crim','zn','indus','chas','nox','rm','age','dis','rad','tax','ptratio','black','lstat','medv')
pre_proc_val <- preProcess(train[,cols], method = c("center", "scale"))
train[,cols] = predict(pre_proc_val, train[,cols])
test[,cols] = predict(pre_proc_val, test[,cols])
summary(train)

set.seed(100)
tree_model = rpart(medv ~., data = train, method="anova", minsplit = 10, minbucket=3) 
summary(tree_model)
prp(tree_model)

PredictCART_train = predict(tree_model, data = train)
table(train$medv, PredictCART_train)

PredictCART = predict(tree_model, newdata = test,)
table(test$medv, PredictCART)

