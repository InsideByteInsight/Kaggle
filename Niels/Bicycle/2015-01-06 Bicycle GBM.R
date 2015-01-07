library(sqldf)
library(dplyr)
library(rpart)
library(rpart.plot)
library(rms)
library(Metrics)
library(randomForest)
library(gbm)

#import the data
train <- read.csv("~/Dropbox/Machine Learning/Kaggle/Bicycle/train.csv")
test <- read.csv("~/Dropbox/Machine Learning/Kaggle/Bicycle/test.csv")

# #combine the data
# test$registered <- NA
# test$casual <- NA
# test$count <- NA
# train$dataset <- as.factor("train")
# test$dataset <- as.factor("test")
# 
# train$istrain = runif(nrow(train), 0, 1) < 0.6666
# test$istrain <- NA
# 
# traintest <- rbind(train, test)

#data manipulations
traintest$datetime <- strptime(traintest$datetime, format="%Y-%m-%d %H:%M:%S")
traintest$hour <- traintest$datetime$hour

### SPLIT THE DATA AGAIN IN TRAIN AND TEST SET
# train <- subset(traintest, dataset == "train")
# test <- subset(traintest,dataset == "test")
allvars <- colnames(train)
illegalvars <- c("datetime", "count", "registered", "casual")
predictors <- allvars[!(allvars %in% illegalvars)]

set.seed(1111)
genmod<-gbm(registered~predictors
            ,data=train ## registered,casual,count columns
            ,var.monotone=NULL
            ,distribution="gaussian"
            ,n.trees=1200
            ,shrinkage=0.1
            ,interaction.depth=3
            #,bag.fraction = 0.5
            ,train.fraction = 1
            ,n.minobsinnode = 10
            ,cv.folds =10
            ,keep.data=TRUE
            ,verbose=TRUE)

best.iter <- gbm.perf(genmod,method="cv") ##the best iteration number

train$pred = predict(genmod, train,best.iter,type="response")

rmsle = rmsle(train$count, abs(train$pred))
