#install.packages("e1071")
#install.packages("gbm")
library(dplyr)
library(e1071)
#install.packages("AppliedPredictiveModeling")
#install.packages("caret")
library(lattice)
library(ggplot2)
library(caret)
#library(AppliedPredictiveModeling)
#library(gbm)
#library(survival)
train_cleaned<- read.csv("iciciTrainData.csv")
nrow(train_cleaned)
train_cleaned44478=train_cleaned[1:1000,]
dim(train_cleaned44478)
head(train_cleaned44478)
trainset <- train_cleaned44478[sapply(train_cleaned44478, is.numeric)]
dim(trainset)
class(trainset[60,60])
sum(is.na(trainset))
dim(trainset)

# Subset data with 18 independent and 1 dependent variables
trainset$Response

trainsetMODEL = cbind(Response = trainset$Response, trainset[,c(trainset$Product_Info_4,trainset$Ins_Age, trainset$Ht, trainset$Wt, trainset$BMI, trainset$InsuredInfo_2, trainset$InsuredInfo_5,
                                                                trainset$InsuredInfo_6, trainset$Family_Hist_4, trainset$Medical_History_4, trainset$Insurance_History_2_1, trainset$Medical_History_39_1, trainset$Medical_History_17.2, trainset$Medical_History_20.1, trainset$Medical_History_30_2, trainset$Medical_History_23_1, trainset$
                                                                  Medical_History_40_1)])

head(trainsetMODEL)

#Now split the data into a training set (80%) and a test set (20%):
## Prepare a training and a test set ##
n<-nrow(trainsetMODEL)
n
ntrain <- round(n*0.8) # number of training examples
ntrain
tindex <- sample(n,ntrain) # indices of training samples
xtrain <- trainsetMODEL[tindex,]
nrow(xtrain)
xtest <- trainsetMODEL[-tindex,]
nrow(xtest)

ytrain <- trainsetMODEL[tindex,1]
ytrain
nrow(ytrain)
ytest <- trainsetMODEL[-tindex,1]
ytest

# Create SVM Model and show summary
#x <- subset(xtrain,ytrain, select=-Response)
#y <- trainsetMODEL$Response

svm_model <- svm(xtrain,ytrain)
summary(svm_model)

# Run Prediction and you  measuring the execution time in R
pred <- predict(svm_model,xtrain)
pred
system.time(pred)

# See the confusion matrix result of prediction, using table function to compare the result of SVM
# prediction and the class data in y variable.
round(pred)
table(round(pred),ytrain)

# Tuning SVM to find the best cost and gamma

svm_tune <- tune(svm, train.x=xtrain, train.y=ytrain, 
                 kernel="radial", ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))

print(svm_tune)

# After you find the best cost and gamma, you can create svm model again and try to run again

svm_model_after_tune <- svm(xtrain,ytrain, kernel="radial", cost=10, gamma=0.5)
summary(svm_model_after_tune)

# Run Prediction again with new model
pred <- predict(svm_model_after_tune,xtrain)
system.time(predict(svm_model_after_tune,xtrain))

# See the confusion matrix result of prediction, using command table to compare the result of SVM
# prediction and the class data in y variable.
table(round(pred),ytrain)

# tuning the test data with same cost and gamma value to predict labels on test
svm_model_after_tune_test <- svm(xtest,ytest, kernel="radial", cost=10, gamma=0.5)

# prediction and the class data in y variable for test data
ypredscore = predict(svm_model_after_tune_test,xtest,type="decision")
round(ypredscore)
table(round(ypredscore),ytest)
mean((ypredscore-ytest)^2)
# Compute accuracy of the model
confusionMatrix(table(round(ypredscore),ytest),ypredscore,positive = NULL,dnn = c("Prediction", "Reference"),prevalence = NULL, mode='everything')



















