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
test$registered <- NA
test$casual <- NA
test$count <- NA
train$dataset <- as.factor("train")
test$dataset <- as.factor("test")

traintest <- rbind(train, test)
########## 
#DATA Modification
#do some data transformations
#convert some integers to categorical variables
#################

# pull the weekday and hour from the datetime variable with R's strptime() function
traintest$datetime <- strptime(traintest$datetime, format="%Y-%m-%d %H:%M:%S")
traintest$weekday <- weekdays(traintest$datetime)
traintest$hour <- traintest$datetime$hour
        
traintest$season <- as.factor(traintest$season)
traintest$holiday <- as.factor(traintest$holiday)
traintest$workingday <- as.factor(traintest$workingday)
traintest$weather <- as.factor(traintest$weather)

#Our dependent variables are the count of bicycles during each hour.
#Therefore we split the train set in 24 subsets, each containing only data concerning that hour
#We will loop over the hours and run any machine learning algorithm over the hours to train the differnt models
#After that we use the best model (in the end the best model for every hour) to predict the test set.

submit<-data.frame(datetime=character(), count=integer())
for (i in 0:23)
{
  
  train_subset0 <- subset(traintest, hour == i & dataset == "train")
  summary(train_subset0$hour)
  
  
  ###############
  #Start with the average of the count as a predictor
  ################
  
  train_subset0$predict_avg <- mean(train_subset0$count)
  MSE_Avg <- mean( (train_subset0$predict_avg - train_subset0$count)^2, na.rm = TRUE)
  
  ############ 
  #Create an easy CART tree 
  ##########
  fit <- rpart(count ~ 
                 season +
                 holiday +
                 workingday +
                 weather +
                 temp +
                 atemp +
                 humidity +
                 windspeed
                ,train_subset0,method="anova",model="true")
  printcp(fit) # display the results 
  plotcp(fit) # visualize cross-validation results 
  summary(fit) # detailed summary of splits
  
  # plot tree, extra =2 -> number of correct/number of total observations
  prp(fit, faclen = 20, type = 3, varlen = 20)
  
  #calculate the mse of the training set
  train_subset0$predict_cart <- predict(fit,train_subset0)
  MSE_CART <- mean( (train_subset0$predict_cart - train_subset0$count)^2, na.rm = TRUE)
  validate(fit)
  
  
  ############  
  #Random forest 
  #####################
  
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
                           windspeed, train_subset0, importance=TRUE, ntree=20, type='supervised' )
  #plot the importance of the variables of the random forest
  varImpPlot(fit_rf,sort=TRUE)
  fit_rf
  
  #calculate the mse of the training set
  train_subset0$predict_randomforest <- predict(fit_rf,train_subset0)
  MSE_RF <- mean( (train_subset0$predict_randomforest - train_subset0$count)^2, na.rm = TRUE)
  
  ####################
  #predict the outcomes of the test set - for now we use the random forest as the leading model
  #####################
  
  test_subset0 <- subset(traintest, hour == i & dataset == "test")
  test_subset0$predict_count <- predict(fit_rf, test_subset0)
  
  #############################################
  #Add the results of this set to the total set
  ############################################
  submit<-rbind(submit,data.frame(datetime = test_subset0$datetime, count = round(test_subset0$predict_count,0)))
}

###### Submit to Kaggle #######
write.csv(submit, file = "~/Dropbox/Machine Learning/Kaggle/Bicycle/submission.csv", row.names = FALSE)
summary(submit)
