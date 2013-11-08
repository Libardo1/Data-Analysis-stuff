

load("/Users/karenyang/Downloads/samsungData.rda")
str(samsungData)
names(samsungData) <- tolower(names(samsungData))
names(samsungData) <- gsub("-", ".", names(samsungData))
names(samsungData) <- gsub(",", ".", names(samsungData))
names(samsungData) <- gsub("\\()", "", names(samsungData))
names(samsungData) <- gsub("\\(", ".", names(samsungData))
names(samsungData) <- gsub("\\)", "", names(samsungData))
names(samsungData)
names(samsungData)[duplicated(names(samsungData))]
samsungData <- data.frame(samsungData)
names(samsungData)

tail(samsungData)
table(samsungData$activity)

hist(samsungData$subject)
table(samsungData$subject)
hist(samsungData$subject)


sum(is.na(samsungData)) # 0 are NA


install.packages("randomForest")
library(randomForest)
install.packages("caret")
library(caret)
library(ElemStatLearn)
install.packages("e1071")
library(e1071)
install.packages("hydroGOF")
library(hydroGOF)

#Dataframes for train, validation, and test data sets
trainset <- samsungData[samsungData$subject%in%c(1,3,5,6,10,13,12,16,20,23), ]
dim(trainset) #2053  563 are the dimensions
str(trainset)
trainset$activity <-as.factor(trainset$activity)
str(trainset$activity)
validset <- samsungData[samsungData$subject%in%c(2,7,8,9,14,17,19,21,24,26), ]
dim(validset)  #2440  563
str(validset)
validset$activity <-as.factor(validset$activity)
str(validset$activity)
testset <- samsungData[samsungData$subject%in%c(27,28,29,30,4,11,15,18,22,25), ]
dim(testset) #2859  563 are the dimensions
str(testset)
testset$activity <-as.factor(testset$activity)
str(testset$activity)
levels(testset$activity)  #"laying"   "sitting"  "standing" "walk"     "walkdown" "walkup"
sum(is.na(samsungData)) # 0 are NA

 
#Model 1 Random Forest
ss <- set.seed(456787, kind="default", normal.kind="default")
foresttrain <- randomForest(trainset$activity ~.-subject, data=trainset, prox=TRUE)
foresttrain
varImpPlot(foresttrain, sort=TRUE, pch=19, col = "brown", main="Variable Importance Plot for Random Forest")

legend("topleft", legend = "Sorted by Importance")


rfpredicted <- predict(foresttrain, newdata = validset)
rfpredicted
errorrateRF <- (sum(rfpredicted != validset$activity))/length(validset$activity)
errorrateRF #0.1122951


#Correlations
cor(trainset$tgravityacc.min.x, as.numeric(trainset$activity))#0.6365321
cor(trainset$angle.x.gravitymean, as.numeric(trainset$activity)) #-0.6049978
cor(trainset$tgravityacc.energy.x, as.numeric(trainset$activity)) #0.6318179
cor(trainset$tgravityacc.mean.x, as.numeric(trainset$activity)) #0.6432291
cor(trainset$tgravityacc.max.x, as.numeric(trainset$activity)) #0.6485595
cor(trainset$tgravityacc.max.y, as.numeric(trainset$activity)) #-0.6850469
cor(trainset$tgravityacc.min.y, as.numeric(trainset$activity)) #-0.695804
cor(trainset$angle.y.gravitymean, as.numeric(trainset$activity)) #0.6662157
cor(trainset$tgravityacc.energy.y, as.numeric(trainset$activity)) #-0.500473
cor(trainset$tgravityacc.mean.y, as.numeric(trainset$activity)) #-0.6927581
cor(trainset$tbodyacc.max.x, as.numeric(trainset$activity)) #0.8150434


#Model 2 SVM
ss <- set.seed(33833, kind="default", normal.kind="default")

svmModel <- svm(trainset$activity ~.-subject, data=trainset)
svmModel
SVMpredicted <- predict(svmModel, newdata = validset)
SVMpredicted
plot(SVMpredicted, validset$activity)
tab <- table(pred=SVMpredicted, true = validset$activity)
tab
classAgreement(tab)

errorrateSVM <-(sum(SVMpredicted != validset$activity))/length(validset$activity)
errorrateSVM #0.1467213
result <-confusionMatrix(SVMpredicted, validset$activity)
result

#Tuned model for random forest
ss <- set.seed(456787, kind="default", normal.kind="default")
foresttrain2 <- randomForest(trainset$activity ~tgravityacc.min.x + angle.x.gravitymean + tgravityacc.energy.x + tgravityacc.mean.x + tgravityacc.max.x + tgravityacc.max.y + tgravityacc.min.y + angle.y.gravitymean + tgravityacc.energy.y + tgravityacc.mean.y + tbodyacc.max.x, data=trainset, prox=TRUE)
foresttrain2
varImpPlot(foresttrain2, sort=TRUE, pch=19, col = "blue", main="Variable Importance Plot")

rfpredicted2 <- predict(foresttrain2, newdata = validset)
rfpredicted2
errorrateRF2 <- (sum(rfpredicted2 != validset$activity))/length(validset$activity)
errorrateRF2 # 0.2057377


#Tuned model for random forest
ss <- set.seed(456787, kind="default", normal.kind="default")
foresttrain2 <- randomForest(trainset$activity ~tgravityacc.min.x + angle.x.gravitymean + tgravityacc.energy.x + tgravityacc.mean.x + tgravityacc.max.x + tgravityacc.max.y + tgravityacc.min.y, data=trainset, prox=TRUE)
foresttrain2
varImpPlot(foresttrain2, sort=TRUE, pch=19, col = "blue", main="Random Forest of training dataset")
rfpredicted2 <- predict(foresttrain2, newdata = validset)
rfpredicted2
errorrateRF2 <- (sum(rfpredicted2 != validset$activity))/length(validset$activity)
errorrateRF2 # 0.3885246

#Model Test
ss <- set.seed(456787, kind="default", normal.kind="default")
foresttrain3 <- randomForest(trainset$activity ~tgravityacc.min.x + angle.x.gravitymean + tgravityacc.energy.x + tgravityacc.mean.x + tgravityacc.max.x + tgravityacc.max.y + tgravityacc.min.y + angle.y.gravitymean + tgravityacc.energy.y + tgravityacc.mean.y + tbodyacc.max.x, data=trainset, prox=TRUE)
foresttrain3
varImpPlot(foresttrain3, sort=TRUE, pch=19, col = "blue", main="Variable Importance Plot of Random Forest Model for Test Set", xlab="")
legend("topleft", legend = "Sorted by Importance")
rfpredicted3 <- predict(foresttrain3, newdata = testset)
rfpredicted3
errorrateRF <- (sum(rfpredicted3 != testset$activity))/length(testset$activity)
errorrateRF # 0.1699895

#Kitchen sink model

ss <- set.seed(456787, kind="default", normal.kind="default")
foresttrain <- randomForest(trainset$activity ~.-subject, data=trainset, prox=TRUE)
foresttrain
rfpredicted <-predict(foresttrain, newdata = testset)
rfpredicted 
errorrateRF <- (sum(rfpredicted != testset$activity))/length(testset$activity)
errorrateRF
