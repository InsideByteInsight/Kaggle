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

library(dplyr)
library(rpart)
library(rpart.plot)
library(rms)

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
traintest$datetime <- strptime(traintest$datetime, format="%Y-%m-%d %H:%M:%S")
traintest$weekday <- weekdays(traintest$datetime)
traintest$hour <- traintest$datetime$hour
        
traintest$season <- as.factor(traintest$season)
traintest$holiday <- as.factor(traintest$holiday)
traintest$workingday <- as.factor(traintest$workingday)
traintest$weather <- as.factor(traintest$weather)

#split the sets again in train and test
train <- subset(traintest, dataset == "train")
test <- subset(traintest,dataset == "test")

#-END DATA MANIULATION-
  
#/START - MODELS/

#//START - AVERAGE AS THE PREDICTOR//
  
train$predict_avg <- mean(train$count)
MSE_Avg <- mean( (train$predict_avg - train$count)^2, na.rm = TRUE)
#--END AVERAGE AS THE PREDICTOR--


##START - CART TREE##

fit <- rpart(count ~ 
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
              ,train,method="anova",model="true")
printcp(fit) # display the results 
plotcp(fit) # visualize cross-validation results 
summary(fit) # detailed summary of splits

# plot tree, extra =2 -> number of correct/number of total observations
prp(fit, faclen = 20, type = 3, varlen = 20)

#calculate the mse of the training set
train$predict_cart <- predict(fit,train)
MSE_CART <- mean( (train$predict_cart - train$count)^2, na.rm = TRUE)
#validate(fit)

#--END - CART TREE--


##START - RANDOM FOREST##
library(randomForest)

#built a randomd Forest (for a random forest we first need to take care of missing values)
fit_rf <- randomForest(count ~ 
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
                          , train, importance=TRUE, ntree=20, type='supervised' )
#plot the importance of the variables of the random forest
varImpPlot(fit_rf,sort=TRUE)

#calculate the mse of the training set
train$predict_randomforest <- predict(fit_rf,train)
MSE_RF <- mean( (train$predict_randomforest - train$count)^2, na.rm = TRUE)

#--END - RANDOM FOREST--
#-END - MODELS-


#START - PREDICT#
#for now we use the random forest to predict
  
test$predict_count <- predict(fit_rf, test)

#-END - PREDICT-

#START - SUBMIT RESULTS#
#write an output csv to submit to kaggle

submit<-data.frame(datetime = test$datetime, count = round(test$predict_count,0))
write.csv(submit, file = "~/Dropbox/Machine Learning/Kaggle/Bicycle/submission.csv", row.names = FALSE)

#-END - SUBMIT RESULTS-
