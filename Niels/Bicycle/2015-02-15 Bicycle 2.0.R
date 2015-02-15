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


library(sqldf)
library(dplyr)
library(rpart)
library(rpart.plot)
library(rms)
library(Metrics)

#set the seed to elimnate randomness in outcomes
set.seed(1234)

#read in the train and test set
train <- read.csv("~/Dropbox/Machine Learning/Kaggle/Bicycle/train.csv")
test <- read.csv("~/Dropbox/Machine Learning/Kaggle/Bicycle/test.csv")

#merge the train and test set to all data operations easily on the whole set
test$registered <- NA
test$casual <- NA
test$count <- NA
train$dataset <- as.factor("train")
test$dataset <- as.factor("test")

train$istrain = runif(nrow(train), 0, 1) < 0.6666
test$istrain <- NA

traintest <- rbind(train, test)

#DATA Modification
#do some data transformations
#convert some integers to categorical variables
#################

# pull the weekday and hour from the datetime variable with R's strptime() function
traintest$datetime2 <- strptime(traintest$datetime, format="%Y-%m-%d %H:%M:%S")

traintest$weekday <- as.factor(traintest$datetime2$wday)
traintest$hour <- as.factor(traintest$datetime2$hour)
traintest$day <- as.factor(traintest$datetime2$mday)
traintest$month <-traintest$datetime2$mon

traintest$year <- as.factor(traintest$datetime2$year+1900)
traintest$season <- as.factor(traintest$season)
traintest$holiday <- as.factor(traintest$holiday)
traintest$workingday <- as.factor(traintest$workingday)
traintest$weather <- as.factor(traintest$weather)
traintest$isWeekend <- as.factor(sapply(traintest$weekday, function(x){if(x == 0 | x == 6) 1 else 0}))
traintest$seasonAdj <- as.factor(sapply(traintest$month, function(x){
  if(x<= 2) 1 
  else if(x<=5) 2 
  else if (x<=8) 3 
  else 4
}))
traintest$month <- as.factor(traintest$datetime2$mon)

#map weather 4 (this is an outlier) to weather 3 (the closest)
traintest$weather <- sapply(traintest$weather, function(x){if(x==4) 3 else x})

#function to goes back n rows and adds n columns to the dataframe
prevrows <- function(data,n) {sapply(1:n,function(x) c(rep(NA,x),head(data,-x)))}
traintest$weather.prev <- as.factor(prevrows(traintest$weather,1))




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
mm2<-ddply(subset(train,workingday==1 & month == 6 & isWeekend == 0),"day", summarise,meancount=sum(count))
ggplot(data=mm2, aes(x=day, y=meancount))+geom_line(shape=1)+geom_smooth(method = lm)


#Plan de campagne:
#Voor casuals weekend of niet doet er niet toe, casuals komen altijd gelijk uitgesmeerd over het jaar
#Voor registerds die komen veel meer door week dan in het weekend
