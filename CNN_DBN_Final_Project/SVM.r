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
sparse_matrix<- read.csv("sparse_representation.csv")
label_reviews <-read.csv("label_reviews.csv")

sparseFrame = data.frame(sparse_matrix)
decision <- label_reviews$final_decision
labelFrame = data.frame(decision)
mergedFrame = data.frame(sparseFrame,labelFrame)
mergedFrame$X <- NULL


trainset <- mergedFrame


# Subset data with 18 independent and 1 dependent variables
#Response <- trainset$label_reviews.final_decision

#trainsetMODEL = cbind(Response, trainset)

#head(trainsetMODEL)
 

#Now split the data into a training set (80%) and a test set (20%):
## Prepare a training and a test set ##
n<-nrow(trainset)
n
ntrain <- round(n*0.8) # number of training examples
ntrain
tindex <- sample(n,ntrain) # indices of training samples
xtrain <- trainset[tindex,]
nrow(xtrain)
xtest <- trainset[-tindex,]
nrow(xtest)

x <- subset(xtrain, select= -decision)
y <- xtrain$decision

model_beforeTune = svm(decision ~ .,data = xtrain)

# Create SVM Model and show summary
#x <- subset(xtrain,ytrain, select=-Response)
#y <- trainsetMODEL$Response

#svm_model <- svm(xtrain,ytrain)
summary(model_beforeTune)

# Run Prediction and you  measuring the execution time in R
pred <- predict(model_beforeTune,xtest)
pred
system.time(pred)

# See the confusion matrix result of prediction, using table function to compare the result of SVM
# prediction and the class data in y variable.

table(pred,xtest$decision)

# Tuning SVM to find the best cost and gamma

svm_tune1 <- tune(svm, train.x=x, train.y=y, 
                 kernel="radial", ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))
svm_tune2 <- tune(svm, train.x=xtrain, train.y=xtrain$Response, 
                 kernel="linear", ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))
svm_tune3 <- tune(svm, train.x=xtrain, train.y=xtrain$Response, 
                 kernel="sigmoid", ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))
svm_tune4 <- tune(svm, train.x=xtrain, train.y=xtrain$Response, 
                 kernel="polynomial", ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))

print(svm_tune)

# After you find the best cost and gamma, you can create svm model again and try to run again

# We are building the model with the suitable value of of kernel and cost and gamma 

model_afterTune = svm(decision ~ ., kernel ="radial",cost= 7 , gamma=0.065,data = xtrain, scale = F)
summary(model_afterTune)

# Run Prediction again with new model
pred <- predict(model_afterTune,xtest)
system.time(predict(model_afterTune,xtest))

# See the confusion matrix result of prediction, using command table to compare the result of SVM
# prediction and the class data in y variable.
table(pred,xtest$decision)

# tuning the test data with same cost and gamma value to predict labels on test
#svm_model_after_tune_test <- svm(xtest,ytest, kernel="radial", cost=10, gamma=0.5)

# prediction and the class data in y variable for test data
ypredscore = predict(model_afterTune,xtest,type="decision")
#round(ypredscore)
table(round(ypredscore),ytest)

# Compute accuracy of the model
confusionMatrix(table(ypredscore,xtest$decision),ypredscore,positive = NULL,dnn = c("Prediction", "Reference"),prevalence = NULL, mode='everything')



















