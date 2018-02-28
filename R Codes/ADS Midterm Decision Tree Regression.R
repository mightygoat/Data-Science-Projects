treedata <- read.csv("/Users/Desktop/ADS miterm/icicitrainData.csv")
str(treedata)
summary(treedata)

#Data frame with selected columns
selected_treedata=subset(treedata, select = c(Product_Info_4,Ins_Age,Ht,Wt,BMI,InsuredInfo_2,InsuredInfo_5,InsuredInfo_6,Family_Hist_4,Medical_History_4,Insurance_History_2_1,Medical_History_39_1,Medical_History_17.2,Medical_History_20.1,Medical_History_30_2,Medical_History_23_1,Medical_History_40_1,Response) )

#spliting the training dataset to evaluate the model.
treedata_train <- treedata[1:40000,]
treedata_test <- treedata[40001:44476,]


#spliting the selected columns training dataset to evaluate the model.
selected_treedata_train <- selected_treedata[1:40000,]
selected_treedata_test <- selected_treedata[40001:44476,]

#training a model
library(rpart)
treemodel <- rpart(treedata_train$Response~., data = treedata_train, method = "anova", 
               control=rpart.control(minsplit=30, cp=0.001))
summary(treemodel)
printcp(treemodel)
plotcp(treemodel)

#training a model with selected columns
selected_treemodel <- rpart(selected_treedata_train$Response~., data = selected_treedata_train, method = "anova", 
                   control=rpart.control(minsplit=30, cp=0.001))
summary(selected_treemodel)
printcp(selected_treemodel)
plotcp(selected_treemodel)

#visualizing the model
library(rpart.plot)
library(RColorBrewer)

fancyRpartPlot(treemodel,uniform=TRUE)
rpart.plot(treemodel,uniform=TRUE)

#visualizing the model with selected columns
fancyRpartPlot(selected_treemodel,uniform=TRUE)
rpart.plot(selected_treemodel,uniform=TRUE)

#evaluating the performance of the model
predicted <- (predict(treemodel, treedata_test))
summary(round(predicted))
summary(treedata_test$Response)
cor(predicted,treedata_test$Response)
accuracy(predicted,treedata_test$Response)#measuring performance with accuracy

#evaluating the performance of the model with selected columns
selected_predicted <- (predict(selected_treemodel, selected_treedata_test))
summary(round(selected_predicted))
summary(selected_treedata_test$Response)
cor(selected_predicted,selected_treedata_test$Response)
accuracy(selected_predicted,selected_treedata_test$Response)#measuring performance with accuracy


#output using actuall test data
realtestdata <- read.csv("/Users/nithinkartha/Desktop/ADS miterm/icicitestData.csv")
realpredicted <- (predict(treemodel, realtestdata))
summary(round(realpredicted))
output <- data.frame(Id = realtestdata$Id,Response = round(realpredicted))
write.csv(output, "/Users/nithinkartha/Desktop/ADS miterm/TreeOutput.csv",row.names = FALSE)