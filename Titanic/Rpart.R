library(rattle)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(randomForest)
library(gbm)
library(som)



set.seed(415)

#read the data
train <- read.csv("~/Dropbox/Kaggle/Titanic/train.csv")
test <- read.csv("~/Dropbox/Kaggle/Titanic/test.csv")
train$traintest <- "train"
test$traintest<-"test"
test$Survived<- NA

#combine train and test set to do som operations on the columns
combi <- rbind(train, test)

#convert the name from factor to character
combi$Name <- as.character(combi$Name)
#add a Title column to the data
combi$Title <- sub(' ','',sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]}))
combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
combi$Title <- as.factor(combi$Title)
table(combi$Title)

#Create familySize
combi$FamilySize <- combi$SibSp + combi$Parch + 1

#Create lastname variable
combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})

combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="")
table(combi$FamilyID)

combi$FamilyID[combi$FamilySize <= 3] <- 'Small'
famIDs <- data.frame(table(combi$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 3,]
combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'
combi$FamilyID <- factor(combi$FamilyID)

#For random forest we should get rid of missing values (simple CART and Boosting trees know how to handle this)
#Getting rid of NA's can be done by first building a simple tree with the variable with missing values as the dependent.
#Age is missing some values
Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked,
                data=combi[!is.na(combi$Age),], method="anova")
combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age),])

#Predict the Fare for the missing value
Farefit <- rpart(Fare ~ Pclass + Sex + SibSp + Parch + Fare + Embarked+Age,
                data=combi[!is.na(combi$Fare),], method="anova")

combi$Fare[is.na(combi$Fare)] <- predict(Farefit, combi[is.na(combi$Fare),])

#Set the Embarked for the missing values to Southhampton
which(combi$Embarked=="")
combi$Embarked[combi$Embarked ==""] <- "S"
table(combi$Embarked)

#Cabin or no cabin variable
combi$HasCabin = as.factor(sapply(combi$Cabin, function(x) if (x=="") 0 else 1))


#the formula for the model
formula <- as.factor(Survived) ~ Title+FamilySize+HasCabin+Pclass + Sex + Age + SibSp + Parch + Fare + Embarked+Fare

#split the data again in a train and test set
train <- combi[combi$traintest == "train",]
test <- combi[combi$traintest == "test",]



#---------Building models
#Built a som first
som(train)
help(som)

#Built a simple CART Tree
fit <- rpart(formula, data=train,method="class")

#Plot the CART tree
fancyRpartPlot(fit)

dataSet <- data.frame(Title = train$Title,Sex = train$Sex, Survived=train$Survived)

#built a randomd Forest (for a random forest we first need to take care of missing values)
fit_rf <- randomForest(dataSet, data=train, importance=TRUE, ntree=2000, type='unsupervised' )
#plot the importance of the variables of the random forest
varImpPlot(fit_rf)


#built a boosted decison tree
fit_gbm <- gbm(Survived ~ FamilyID+Title+FamilySize+HasCabin+Pclass + Sex + Age + SibSp + Parch + Fare + Embarked+Fare          
               ,data=train,n.trees = 200,shrinkage=0.08, train.fraction=0.5)
fit_gbm
# check performance using an out-of-bag estimator
# OOB underestimates the optimal number of iterations
best.iter <- gbm.perf(fit_gbm,method="test")
print(best.iter)

#Predict unknowns from the test set
Survived_RF <- predict(fit_rf, test, type = "class")

#GBM: Predict unknowns from the test set
Survived_GBM <- predict.gbm(fit_gbm, test, type = "response")
Survived_GBM <- as.factor(sapply(Survived_GBM,function(x) if (x > 0.5) 1 else 0))


#Submit results to kaggle
submit <- data.frame(PassengerId = test$PassengerId, Survived = Survived_RF)
write.csv(submit, file = "randomforest.csv", row.names = FALSE)
summary(submit)

#Submit GBM to kaggle
submit <- data.frame(PassengerId = test$PassengerId, Survived = Survived_GBM)
write.csv(submit, file = "gbm.csv", row.names = FALSE)
summary(submit)


