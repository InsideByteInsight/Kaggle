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

#TODO

#More feature engineering
#1 implement adjusted seasons (spring = march, april may!) DONE
#2 do not loop over all hours but over sets of hours
#3 check for outliers in data
#4 Make some corrections on the humidity - e.g. a humidity of 0 or 100% is not possible! So what is "correct" data and what are errors?
# Maybe make a model based on temperature, month  and a temp to predict humidity.
# Windspeed has a lot of 0 values. Make a model to predict the correct winddspeed based on month, season and temp?
# Add the weather of the previous day as a predictor. People that observe nice weather today will think about renting a bike tomorrow.



library(sqldf)
library(dplyr)
library(rpart)
library(rpart.plot)
library(rms)
library(Metrics)
library(randomForest)
library(gbm)

set.seed(1234)
train <- read.csv("~/Dropbox/Machine Learning/Kaggle/Bicycle/train.csv")
test <- read.csv("~/Dropbox/Machine Learning/Kaggle/Bicycle/test.csv")
test$registered <- NA
test$casual <- NA
test$count <- NA
train$dataset <- as.factor("train")
test$dataset <- as.factor("test")

train$istrain = runif(nrow(train), 0, 1) < 0.6666
test$istrain <- NA

traintest <- rbind(train, test)
########## 
#DATA Modification
#do some data transformations
#convert some integers to categorical variables
#################


traintest$ID <- seq.int(nrow(traintest)) #add a column with ID's to use for joining the table lateron.
# pull the weekday and hour from the datetime variable with R's strptime() function
traintest$datetime2 <- strptime(traintest$datetime, format="%Y-%m-%d %H:%M:%S")
traintest$weekday <- as.factor(traintest$datetime2$wday)
traintest$hour <- traintest$datetime2$hour
traintest$day <- traintest$datetime2$mday
traintest$month <- traintest$datetime2$mon
traintest$year <- as.factor(traintest$datetime2$year+1900)
traintest$season <- as.factor(traintest$season)
traintest$holiday <- as.factor(traintest$holiday)
traintest$workingday <- as.factor(traintest$workingday)
traintest$weather <- as.factor(traintest$weather)
traintest$isWeekend <- sapply(traintest$weekday, function(x){if(x == 0 | x == 6) 1 else 0})
traintest$seasonAdj <- sapply(traintest$month, function(x){
  if(x<= 2) 1 
  else if(x<=5) 2 
  else if (x<=8) 3 
  else 4
})
traintest$month <- as.factor(traintest$datetime2$mon)

#function to goes back n rows and adds n columns to the dataframe
prevrows <- function(data,n) {sapply(1:n,function(x) c(rep(NA,x),head(data,-x)))}
traintest$weather.prev <- prevrows(traintest$weather,1)

#map weather 4 (this is an outlier) to weather 3 (the closest)
traintest$weather <- sapply(traintest$weather, function(x){if(x==4) 3 else x})

#the hours are the most important variables, therefore we construct different models for different groups of hours
traintest$hourgroup.reg <- as.factor(sapply(traintest$hour, function(x){if(x <= 6) 1 
                                                              else if (x == 7) 2
                                                              else if (x==8) 3
                                                              else if (x<=15) 4
                                                              else if (x<=17) 5
                                                              else if (x<=19) 6
                                                              else if (x<=23) 7
}))

traintest$hourgroup.cas <- as.factor(sapply(traintest$hour, function(x){if(x <= 7) 1 
                                                              else if (x <= 9) 2
                                                              else if (x<=19) 3
                                                              else if (x<=23) 4
}))

#create empty columns for the predicted variables
traintest$pred.registered <- NA
traintest$pred.casual <- NA
traintest$pred.count <- NA

### SPLIT THE DATA AGAIN IN TRAIN AND TEST SET
train <- subset(traintest, dataset == "train")
test <- subset(traintest,dataset == "test")

# #########PLOTTING######################
# #Some plotting to get a sense for the data 
# # Save average counts for each day/time in data frame
# day_hour_counts <- as.data.frame(aggregate(train[,"count"], list(weekday=train$weekday, hour=train$hour), mean))
# day_hour_counts$weekday <- factor(day_hour_counts$weekday, ordered=TRUE, levels=c(1, 2, 3, 4, 5, 6, 0))
# day_hour_counts$hour <- as.numeric(as.character(day_hour_counts$hour))
# 
# weather_hour_counts <- as.data.frame(aggregate(train[,"count"], list(weather=train$weather, hour=train$hour), mean))
# weather_hour_counts$weather <- factor(weather_hour_counts$weather)
# weather_hour_counts$hour <- as.numeric(as.character(weather_hour_counts$hour))
# 
# # plot heat mat with ggplot
library(ggplot2)
# require(stats)
# ggplot(day_hour_counts, aes(x = hour, y = weekday))+ geom_tile(aes(fill = x)) + scale_fill_gradient(name="Average Counts", low="white", high="green") + theme(axis.title.y = element_blank())
# ggplot(weather_hour_counts, aes(x = hour, y = weather))+ geom_tile(aes(fill = x)) + scale_fill_gradient(name="Average Counts", low="white", high="green") + theme(axis.title.y = element_blank())
# 
# 
library(plyr)
# 
# mm<-ddply(train,"season", summarise,meancount=mean(count))
# 
# c<-ggplot(data=mm,aes(season, meancount))
# c +geom_bar(stat="identity") +   
#   #coord_flip() + 
#   theme(axis.title.y = element_blank())
# 
# ddply(train,.(season, month), summarize,meancount=mean(count))

#plot some timeseries
# mm2<-ddply(subset(train,workingday==1 & month == 6 & isWeekend == 0),"day", summarise,meancount=sum(count))
# ggplot(data=mm2, aes(x=day, y=meancount))+geom_line(shape=1)+geom_smooth(method = lm)
##################END PLOTTING ############################


#Our dependent variables are the count of bicycles during each hour.
#Therefore we split the train set in 24 subsets, each containing only data concerning that hour
#We will loop over the hours and run any machine learning algorithm over the hours to train the differnt models
#After that we use the best model (in the end the best model for every hour) to predict the test set.

#set model and some other parameters
model = "RF"
submit = FALSE
submission<-data.frame(datetime=character(), count=integer())


dependents <- c("registered", "casual")


# # built a model to identify groups of hours that "behave similar"
# dependent = "casual"
# train$hour <- as.integer(train$hour)
# formula <- as.formula(paste(dependent, " ~ 
#                                 hour"))
# fit <- rpart(formula,train.ts,method="anova",model="true")
# printcp(fit) # display the results 
# 
# # plot tree, extra =2 -> number of correct/number of total observations
# prp(fit, faclen = 20, type = 3, varlen = 20)
train.reg.rs <- data.frame(traintest[0,])
test.reg.rs <- data.frame(traintest[0,])
for (h in unique(train$hour))
{
  train.reg.ts <- subset(traintest, hour == h & dataset == "train" & istrain == TRUE)
  train.reg.vs <- subset(traintest, hour == h & dataset == "train" & istrain == FALSE)
  test.reg.ss <- subset(traintest, hour == h & dataset == "test")

  formula <- registered ~ season +                             
                              workingday +
                              weather +
                              temp +
                              atemp +
                              humidity +                              
                              month +
                              year +
                              isWeekend +
                              seasonAdj +
                              weather.prev +
                              windspeed 

    #built a randomd Forest (for a random forest we first need to take care of missing values)
    fit <- randomForest(formula, train.reg.ts, importance=TRUE, ntree=200, type='supervised',na.action =  na.omit)
  fit <- gbm(formula, data = train.cas.ts, n.trees=200, shrinkage=0.08, train.fraction=0.5)  
  #plot the importance of the variables of the random forest
    #varImpPlot(fit,sort=TRUE)
    #fit.rf
  best.iter <- gbm.perf(fit,method="test")
  best.iter
    #predit on the train.vs
    train.reg.vs$pred.reg <- predict(fit,train.reg.vs)
    #predict ont he test.testset
    test.reg.ss$pred.reg <- predict(fit, test.reg.ss)
  
  
  ##############################################
  #Add the results of this set to the RESULT SET OF CASUAL AND REGISTERED SEPERATELY set
  #LATER WE WILL COMBINE THE 2 SETS AGAIN VIA A MERGE(DF1,DF2, BY="ID")
  ############################################
  train.reg.rs <-rbind(train.reg.rs,train.reg.vs)
  test.reg.rs<-rbind(test.reg.rs,test.reg.ss)
}

RMSLE.Reg<-rmsle(train.reg.rs$registered, abs(train.reg.rs$pred.reg))

#######Predict the casuals
train.cas.rs <- data.frame(traintest[0,])
test.cas.rs <- data.frame(traintest[0,])
for (h in unique(train$hour))
{
  train.cas.ts <- subset(traintest, hour == h & dataset == "train" & istrain == TRUE)
  train.cas.vs <- subset(traintest, hour == h & dataset == "train" & istrain == FALSE)
  test.cas.ss <- subset(traintest, hour == h & dataset == "test")
  
  formula <- casual ~ season +                             
    workingday +
    weather +
    temp +
    atemp +
    humidity +                              
    month +
    year +
    isWeekend +
    seasonAdj +
    weather.prev 

  
  #built a randomd Forest (for a random forest we first need to take care of missing values)
  #fit <- randomForest(formula, train.cas.ts, importance=TRUE, ntree=200, type='supervised',na.action =  na.omit)
  fit <- gbm(formula, data = train.cas.ts, n.trees=200, shrinkage=0.08, train.fraction=0.5)
 
  #plot the importance of the variables of the random forest
  #varImpPlot(fit,sort=TRUE)
  #fit.rf
  
  #predit on the train.vs
  train.cas.vs$pred.cas <- predict(fit,train.cas.vs)
  #predict ont he test.testset
  test.cas.ss$pred.cas <- predict(fit, test.cas.ss)

  train.cas.rs <-rbind(train.cas.rs,train.cas.vs)
  test.cas.rs<-rbind(test.cas.rs,test.cas.ss)
}
RMSLE.Cas<-rmsle(train.cas.rs$casual, abs(train.cas.rs$pred.cas))

#LATER WE WILL COMBINE THE 2 SETS AGAIN VIA A MERGE(DF1,DF2, BY="ID")
############################################
train.cas.rs <- data.frame(ID = train.cas.rs$ID, pred.cas = train.cas.rs$pred.cas)
test.cas.rs <- data.frame(ID = test.cas.rs$ID, pred.cas = test.cas.rs$pred.cas)

train.rs <- merge(train.reg.rs, train.cas.rs, by="ID")
test.rs <- merge(test.reg.rs, test.cas.rs, by="ID")

train.rs$pred.regcas <- train.rs$pred.reg + train.rs$pred.cas
test.rs$pred.regcas <- test.rs$pred.reg + test.rs$pred.cas


RMSLE.Reg<-rmsle(train.rs$registered, train.rs$pred.reg)
RMSLE.Cas<-rmsle(train.rs$casual, train.rs$pred.cas)
RMSLE.RegCas<-rmsle(train.rs$count, train.rs$pred.regcas)

###### Submit to Kaggle #######
if (submit ==TRUE){
  submission<-data.frame(datetime = test.rs$datetime, count = test.rs$pred.regcasual)
  write.csv(submission, file = "~/Dropbox/Machine Learning/Kaggle/Bicycle/submission.csv", row.names = FALSE)
}

