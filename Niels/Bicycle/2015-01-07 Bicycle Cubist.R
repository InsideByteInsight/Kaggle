library(sqldf)
library(dplyr)
library(rpart)
library(rpart.plot)
library(rms)
library(Metrics)
library(randomForest)
library(gbm)
library(Cubist)
#import the data
train <- read.csv("~/Dropbox/Machine Learning/Kaggle/Bicycle/train.csv")
test <- read.csv("~/Dropbox/Machine Learning/Kaggle/Bicycle/test.csv")

#combine the data
test$registered <- NA
test$casual <- NA
test$count <- NA
train$dataset <- as.factor("train")
test$dataset <- as.factor("test")

train$istrain = runif(nrow(train), 0, 1) < 0.6666
test$istrain <- NA

traintest <- rbind(train, test)

#data manipulations
traintest$datetime <- strptime(traintest$datetime, format="%Y-%m-%d %H:%M:%S")
traintest$hour <- traintest$datetime$hour
traintest$weekday <- traintest$datetime$wday
traintest$isWeekend <- sapply(traintest$weekday, function(x){if(x == 0 | x == 6) 1 else 0})


### SPLIT THE DATA AGAIN IN TRAIN AND TEST SET
train <- subset(traintest, dataset == "train")
test <- subset(traintest,dataset == "test")

#split the train in a train and validation set
train.ts <- subset(train, istrain == TRUE)
train.vs <- subset(train, istrain == FALSE)

predictors <- train.ts[,-c(1,10,11,12,13,14)]
dependent <- train.ts[,c(11)]
fit<-cubist(predictors, dependent,committees = 30)


summary(fit)

#predic the model on the validation set
train.vs$pred <- predict(fit, train.vs,neighbors = 5)
rmsle(train.vs$registered, train.vs$pred)
