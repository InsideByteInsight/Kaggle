# datetime - hourly date + timestamp  
# season -  1 = spring, 2 = summer, 3 = fall, 4 = winter 
# holiday - whether the day is considered a holiday
# workingday - whether the day is neither a weekend nor holiday
# weather - 1: Clear, Few clouds, Partly cloudy, Partly cloudy 
# 2: Mist + Cloudy, Mist + Broken clouds, Mist + Few clouds, Mist 
# 3: Light Snow, Light Rain + Thunderstorm + Scattered clouds, Light Rain + Scattered clouds 
# 4: Heavy Rain + Ice Pallets + Thunderstorm + Mist, Snow + Fog 
# temp - temperature in Celsius
# atemp - "feels like" temperature in Celsius
# humidity - relative humidity
# windspeed - wind speed
# casual - number of non-registered user rentals initiated
# registered - number of registered user rentals initiated
# count - number of total rentals


#todo
#1. feature engineering
#2. hyperparameter tuning (search over best settings of the models)
#3. different models (svm, nn, etc.)

library(dplyr)
library(rpart)
library(rpart.plot)
library(rms)
library(Metrics)
train <- read.csv("~/Dropbox/Machine Learning/Kaggle/Bicycle/train.csv")
test <- read.csv("~/Dropbox/Machine Learning/Kaggle/Bicycle/test.csv")

#Combine the sets in one big data set traintest, to do some data manipulations immediately on both sets
test$registered <- NA
test$casual <- NA
test$count <- NA
train$dataset <- as.factor("train")
test$dataset <- as.factor("test")
traintest <- rbind(train, test)

#/START - DATA MODIFICATIONS#
#do some data transformations
#convert some integers to categorical variables

#pull the weekday and hour from the datetime variable with R's strptime() function
#traintest$datetime <- strptime(traintest$datetime, format="%Y-%m-%d %H:%M:%S")
#traintest$hour <- traintest$datetime$hour
traintest$hour <- as.factor(substr(traintest$datetime, 12,13))
traintest$weekday <- as.factor(weekdays(strptime(traintest$datetime, format="%Y-%m-%d %H:%M:%S")))
traintest$year <- as.factor(substr(traintest$datetime, 1,4))
traintest$month <- as.factor(substr(traintest$datetime, 6,7))
traintest$day <- as.factor(substr(traintest$datetime, 9,10))
traintest$date <- as.Date(substr(traintest$datetime, 1,10))
        
traintest$season <- as.factor(traintest$season)
traintest$holiday <- as.factor(traintest$holiday)
traintest$workingday <- as.factor(traintest$workingday)
traintest$weather <- as.factor(traintest$weather)

#split the sets again in train and test
train <- subset(traintest, dataset == "train")
test <- subset(traintest,dataset == "test")

#-END DATA MANIULATION-

#/START - DATA PLOTTING/

# Save average counts for each day/time in data frame
day_hour_counts <- as.data.frame(aggregate(train[,"count"], list(weekday=train$weekday, hour=train$hour), mean))
day_hour_counts$weekday <- factor(day_hour_counts$weekday, ordered=TRUE, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
day_hour_counts$hour <- as.numeric(as.character(day_hour_counts$hour))
day_month_counts <- as.data.frame(aggregate(train[,"count"], list(day=train$day, month=train$month), mean))
day_season_counts <- as.data.frame(aggregate(train[,"count"], list(day=train$day, season=train$season), mean))

# plot heat mat with ggplot
library(ggplot2)
require(stats)
ggplot(day_hour_counts, aes(x = hour, y = weekday))+ geom_tile(aes(fill = x)) + scale_fill_gradient(name="Average Counts", low="white", high="green") + theme(axis.title.y = element_blank())
ggplot(day_season_counts, aes(x = day, y = season)) + geom_tile(aes(fill = x)) + scale_fill_gradient(name="Average Counts", low="white", high="green") + theme(axis.title.y = element_blank())


#/START - MODELS/
#define the formula for the fit
formula <-count ~ 
  season +
  holiday +
  workingday +
  weather +
  temp +
  atemp +
  humidity +
  windspeed +
  hour +
  weekday

#//START - AVERAGE AS THE PREDICTOR//
  
train$predict.avg <- mean(train$count)

RMSLE_AVG <- rmsle(train$count, abs(train$predict.avg))
#--END AVERAGE AS THE PREDICTOR--

#/START - CART TREE
library(rpart)

fit.rpart <- rpart(formula,train)
printcp(fit.rpart) # display the results 
plotcp(fit.rpart) # visualize cross-validation results 
summary(fit.rpart) # detailed summary of splits

# plot tree, extra =2 -> number of correct/number of total observations
prp(fit.rpart, faclen = 20, type = 3, varlen = 20)

#calculate the mse of the training set
train$predict.rpart <- predict(fit.rpart,train)
RMSLE_RPART <- rmsle(train$count, abs(train$predict.rpart))

#--END - RPART

##START - CART CTREE##
library(party)
library(partykit)

fit.ctree <- ctree(formula,train)
plot(fit.ctree)
fit.ctree

#calculate the mse of the training set
train$predict.ctree <- predict(fit.ctree,train)
RMSLE_CTREE <- rmsle(train$count, abs(train$predict.ctree))

#validate(fit)

#--END - CART TREE--


##START - RANDOM FOREST##
library(randomForest)

#built a randomd Forest (for a random forest we first need to take care of missing values)
fit.rf <- randomForest(formula, train, importance=TRUE, ntree=20, type='supervised' )
#plot the importance of the variables of the random forest
varImpPlot(fit.rf,sort=TRUE)

#calculate the mse of the training set
train$predict.rf <- predict(fit.rf,train)
RMSLE_RF <- rmsle(train$count, abs(train$predict.rf))

#--END - RANDOM FOREST--

##START - GBM##
library("gbm")

fit.gbm<-gbm(formula, data = train, n.trees = 1000,shrinkage=0.02)

best.iter <- gbm.perf(fit.gbm,)
best.iter


train$predict.gbm <- predict(fit.gbm, train,best.iter)
RMSLE_GBM <- rmsle(train$count, abs(train$predict.gbm))
#-END - MODELS-


#START - PREDICT#
#for now we use the random forest to predict
  
test$predict.rf <- predict(fit.rf, test)

#-END - PREDICT-

#START - SUBMIT RESULTS#
#write an output csv to submit to kaggle

submit<-data.frame(datetime = test$datetime, count = test$predict.rf)
write.csv(submit, file = "~/Dropbox/Machine Learning/Kaggle/Bicycle/submission.csv", row.names = FALSE)
summary(submit)
#-END - SUBMIT RESULTS-

