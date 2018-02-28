install.packages("drat", repos="https://cran.rstudio.com")
drat:::addRepo("dmlc")

install.packages("deepnet")
install.packages("darch")
install.packages("newDarch")
install.packages("matrixStats")
library(caret)
library(matrixStats)
library(mxnet)
library(darch)
library(deepnet)
#load input
setwd("E:/Subjects/ADS/Assignment_Neural_Network/")
sparse<-read.csv("final.csv")
ncol(sparse)
nrow(sparse)

sparse <- sparse[-1]
summary(sparse)

index <- sample(1:nrow(sparse),round(0.75*nrow(sparse)))
train <- sparse[index,]
test <- sparse[-index,]
dim(train)

#standardize

train.x <- data.matrix(train[-10138])
train.y <- data.matrix(train[-(1:10137)])
summary(train.y)
summary(train.x)
model_cnn <- darch(train.x,train.y,darch.fineTuneFunction = "minimizeClassifier",
                   darch.batchSize = 50,darch.numEpochs = 10,dataSetValid = NULL)

nrow(test)
nrow(train)


#predict
preds <- predict(model_cnn, test[-10138])
predictions <- unlist(preds)
cnn.out<-as.data.frame(preds)
nrow(cnn.out)

xtab <- table(unlist(test[10138]),unlist(round(preds)))
xtab

library(caret)
a <- confusionMatrix(unlist(round(preds)), unlist(test[10138]))
a
plot(test[10138])
