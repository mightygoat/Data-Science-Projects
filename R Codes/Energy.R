library(readxl)

train <-as.data.frame(read_excel("D:/Subjects/ADS/Assignment week4/Energy Efficient Training Data.xlsx"))
train
test <-as.data.frame(read_excel("D:/Subjects/ADS/Assignment week4/Energy Efficiency Test.xlsx"))
test
library("rpart")
library("rpart.plot")
decision<-train
decision
summary(decision)
train.column.types <- c('integer',   # 
                        'integer',    #  
                        'integer',    # 
                        'integer', # 
                        'integer',    # 
                        'factor',   # 
                        'factor',   # 
                        'factor',   # 
                        'integer', # 
                        'integer'   # 
)

test.column.types <- train.column.types[-2]
fit <- rpart(Y1 ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 +X8,
             data=train,
             method="ANOVA")
summary(fit)
rpart(fit)
rpart.plot(fit, type = 4,extra = 4)

plot(fit)
text(fit)
predict(fit, data = test, type = "class")
Prediction<-predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "myfirstdectree.csv", row.names = FALSE)
predict(fit, data = test, type = "prob")
prediction_prob<-predict(fit,  test, type = "prob")
submit <- data.frame(PassengerId = test$PassengerId, Survived = prediction_prob)
write.csv(submit, file = "myfirstdectreeprob.csv", row.names = FALSE)



library("rpart")
library("rpart.plot")
library(readxl)

train <-as.data.frame(read_excel("D:/Subjects/ADS/Assignment week4/Energy Efficient Training Data.xlsx"))
train
test <-as.data.frame(read_excel("D:/Subjects/ADS/Assignment week4/Energy Efficiency Test.xlsx"))
test
adm_data<-as.data.frame(train)
fit <- rpart(Y1 ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 +X8,
               data = train,
               method="ANOVA")



library(readxl)

train_data <-as.data.frame(read.csv("D:/Subjects/ADS/Assignment week4/Energy Efficiency.csv"))
train_data

library("rpart")
library("rpart.plot")
Load_decision<-train_data
summary(Load_decision)
train.column.types <- c('integer',   # PassengerId
                        'factor',    # Survived 
                        'factor',    # Pclass
                        'character', # Name
                        'factor',    # Sex
                        'numeric',   # Age
                        'integer',   # SibSp
                        'integer',   # Parch
                        'character', # Ticket
                        'numeric',   # Fare
                        'character', # Cabin
                        'factor'     # Embarked
)
test.column.types <- train.column.types[-2]

fit <- rpart(Load_decision$Y1 ~ Load_decision$X1 + Load_decision$X2 + Load_decision$X3 + Load_decision$X4 + Load_decision$X5
             + Load_decision$X6 + Load_decision$X7 + Load_decision$X8,
             data=Load_decision,
             method="A")
summary(fit)
plot(fit)
rpart.plot(fit)
library(rattle)
rattle()
library(rpart.plot)
library(RColorBrewer)
fancyRpartPlot(fit)



library(tree)
install.packages("tree")
train_data <-as.data.frame(read.csv("D:/Subjects/ADS/Assignment week4/Energy Efficiency.csv"))
data <- read.table("D:/Subjects/ADS/Assignment week4/Energy Efficiency.csv", header=TRUE)
data
train_data
library("rpart")
library("rpart.plot")
Load_decision<-train_data
summary(Load_decision)

tree.model <- tree(log(Y1) ~ X1 + X2 + X3 + X4 + X5
                   + X6 + X7 + X8 , data=Load_decision)
plot(tree.model)
text(tree.model, cex=.75)
summary(tree.model)

tree.model2 <- tree(log(Y1) ~ X1 + X2 + X3 + X4 + X5
                    + X6 + X7 + X8 , data=Load_decision, mindev=0.001)
plot(tree.model2)
text(tree.model2, cex=.75)
summary(tree.model2)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
Prediction <- predict(tree.model, test, type = "tree")

Prediction
fit <- rpart(Y1 ~ X1 + X2 + X3 + X4 + X5
                    + X6 + X7 + X8 , data=Load_decision,  method = "anova")

plot(fit)
text(fit)

printcp(fit) # display the results 
plotcp(fit) # visualize cross-validation results 
summary(fit) # de
par(mfrow=c(1,2)) # two plots on one page 
rsq.rpart(fit) # visualize cross-validation results  

plot(fit, uniform=TRUE, 
     main="Regression Tree ")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

# create attractive postcript plot of tree 
post(fit, file = "c:/tree2.ps", 
     title = "Regression Tree  ")
# prune the tree 
pfit<- prune(fit, cp=0.01160389) # from cptable   

# plot the pruned tree 
plot(pfit, uniform=TRUE, 
     main="Pruned Regression Tree")
text(pfit, use.n=TRUE, all=TRUE, cex=.8)
post(pfit, file = "c:/ptree2.ps", 
     title = "Pruned Regression Tree for Mileage")



