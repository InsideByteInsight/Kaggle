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
train <- read.csv("~/Dropbox/Machine Learning/Kaggle/Bycicle/train.csv")
test <- read.csv("~/Dropbox/Machine Learning/Kaggle/Bycicle/test.csv")
test$registered <- NA
test$casual <- NA
test$count <- NA
train$dataset <- as.factor("train")
test$dataset <- as.factor("test")

traintest <- rbind(train, test)
########## DATA Modification ########
#do som data transformations
#convert some integers to categorical variables

traintest$time = as.integer(gsub("0","",gsub(":","",as.character(lapply(strsplit(as.character(traintest$datetime), split=" "), "[", 2)))))

traintest$season <- as.factor(traintest$season)
traintest$holiday <- as.factor(traintest$holiday)
traintest$workingday <- as.factor(traintest$workingday)
traintest$weather <- as.factor(traintest$weather)

#Our dependent variables are the count of bycicles during each hour.
#Therefore we split the train set in 24 subsets, each containing only data concerning that hour
#We will loop over the hours and run any machine learning algorithm over the hours.

# as an example we will start with hour 0
train_subset0 <- subset(traintest, time == 1 & dataset == "train")
summary(train_subset0$hour)
############ Create an easy CART tree to start ##########
fit <- rpart(count ~ 
               season +
               holiday +
               workingday +
               weather +
               temp +
               atemp +
               humidity +
               windspeed
              ,train_subset0,method="anova")
printcp(fit) # display the results 
plotcp(fit) # visualize cross-validation results 
summary(fit) # detailed summary of splits

# plot tree, extra =2 -> number of correct/number of total observations
prp(fit, faclen = 20, type = 3, varlen = 20)

#Get the predicted counts
train_subset0$predicted <- predict(fit, train)


############  Random forest #####################
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





###### Submit to Kaggle #######

submit <- data.frame(test$datetime, count = predict_count)
write.csv(submit, file = "submission.csv", row.names = FALSE)
summary(submit)
