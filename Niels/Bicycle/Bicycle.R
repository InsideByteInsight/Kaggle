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
library(Metrics)

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

# pull the weekday and hour from the datetime variable with R's strptime() function
traintest$datetime <- strptime(traintest$datetime, format="%Y-%m-%d %H:%M:%S")
traintest$weekday <- weekdays(traintest$datetime)
traintest$hour <- traintest$datetime$hour
        
traintest$season <- as.factor(traintest$season)
traintest$holiday <- as.factor(traintest$holiday)
traintest$workingday <- as.factor(traintest$workingday)
traintest$weather <- as.factor(traintest$weather)
traintest$isWeekend <- sapply(traintest$weekday, function(x){if(x == "Saturday" | x == "Sunday") 1 else 0})

#Our dependent variables are the count of bicycles during each hour.
#Therefore we split the train set in 24 subsets, each containing only data concerning that hour
#We will loop over the hours and run any machine learning algorithm over the hours to train the differnt models
#After that we use the best model (in the end the best model for every hour) to predict the test set.

submit<-data.frame(datetime=character(), count=integer())
originalset <- data.frame(traintest[0,])

#define the formula here
dependents <- c("registered","casual")
dependents <- c("count")

for(dependent in dependents)
{
  pred <- paste("predict.",dependent, sep="")
  originalset[[pred]]=numeric()
  formula <- as.formula(paste(dependent, " ~ 
                             season +
                             holiday +
                             workingday +
                             weather +
                             temp +
                             atemp +
                             humidity +
                             windspeed +
                              isWeekend"))
  
  for (i in 0:23)
  {
    
    train.trainset <- subset(traintest, hour == i & dataset == "train" & istrain == TRUE)
    train.testset <- subset(traintest, hour == i & dataset == "train" & istrain == FALSE)
   
    
  #   ###############
  #   #Start with the average of the count as a predictor
  #   ################
  #   
  #   train.trainset$predict_avg <- mean(train.trainset$count)
  #   MSE_Avg <- mean( (train.trainset$predict_avg - train.trainset$count)^2, na.rm = TRUE)
  #   
  #   ############ 
  #   #Create an easy CART tree 
  #   ##########
  #   fit <- rpart(count ~ 
  #                  season +
  #                  holiday +
  #                  workingday +
  #                  weather +
  #                  temp +
  #                  atemp +
  #                  humidity +
  #                  windspeed
  #                 ,train.trainset,method="anova",model="true")
  #   printcp(fit) # display the results 
  #   plotcp(fit) # visualize cross-validation results 
  #   summary(fit) # detailed summary of splits
  #   
  #   # plot tree, extra =2 -> number of correct/number of total observations
  #   prp(fit, faclen = 20, type = 3, varlen = 20)
  #   
  #   #calculate the mse of the training set
  #   train.trainset$predict_cart <- predict(fit,train.trainset)
  #   MSE_CART <- mean( (train.trainset$predict_cart - train.trainset$count)^2, na.rm = TRUE)
  #   validate(fit)
  #   
  #   
  #   ############  
  #   #Random forest 
  #   #####################
    
    library(randomForest)
    
    #built a randomd Forest (for a random forest we first need to take care of missing values)
    fit.rf <- randomForest(formula, train.trainset, importance=TRUE, ntree=200, type='supervised' )
    #plot the importance of the variables of the random forest
    #varImpPlot(fit.rf,sort=TRUE)
    #fit.rf
    
    #calculate the mse of the train.testset
    train.testset[[pred]] <- predict(fit.rf,train.testset)
    MSE_RF <- rmsle(train.testset[[pred]],train.testset[[dependent]])
    
    ####################
    #predict the outcomes of the test set - for now we use the random forest as the leading model
    #####################
    
    test.subset <- subset(traintest, hour == i & dataset == "test")
    test.subset[[pred]] <- predict(fit.rf, test.subset)
    
    #############################################
    #Add the results of this set to the total set
    ############################################
    originalset <-rbind(originalset,train.testset)
    submit<-rbind(submit,data.frame(datetime = test.subset$datetime, count = round(test.subset[[pred]],0)))
  }
}
rmsle(originalset[[dependent]], originalset[[pred]])
###### Submit to Kaggle #######
write.csv(submit, file = "~/Dropbox/Machine Learning/Kaggle/Bicycle/submission.csv", row.names = FALSE)
summary(submit)
