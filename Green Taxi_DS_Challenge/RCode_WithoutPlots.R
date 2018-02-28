
# Prepare Data
library(data.table)
mydat <- fread('https://s3.amazonaws.com/nyc-tlc/trip+data/green_tripdata_2015-09.csv')
head(mydat)
str(mydat)
#write.csv(mydat, file = "greenTaxi.csv", row.names = FALSE)
summary(mydat)

is.data.frame(mydat)

#Cleaning the data for the predictive model for tip as a percentage of the total fare
#Clean data for derived variable for tip as a percentage of the total fare.

mydat$Total_amount[mydat$Total_amount<2.5] <-2.5
mydat$Fare_amount[mydat$Fare_amount<0] <-0
mydat$improvement_surcharge[mydat$improvement_surcharge<0] <-0.30
mydat$Tip_amount[mydat$Tip_amount<0] <-0
mydat$Trip_type[mydat$Trip_type=="NaN"]<-1
mydat$Trip_type[mydat$Trip_type=="NA"]<-1


mydat$RateCodeID[mydat$RateCodeID==99] <-2 
mydat$Ehail_fee<- NULL
mydat$Extra[mydat$Extra<0] <-0
Tip_Percentage<- (100*mydat$Tip_amount)/mydat$Total_amount
mydat$Tip_Percentage <- Tip_Percentage


Tip_Percentage
summary(Tip_Percentage)
str(Tip_Percentage)
#write.csv(mydat, file = "greenTaxicleanDataTip.csv", row.names = FALSE)
summary(mydat)
str(mydat)

cleanData<-mydat
# Change the format of datetime from string to POSIXct objects

cleanData$lpep_pickup_datetime <- as.POSIXct(cleanData$lpep_pickup_datetime,format='%Y-%m-%d %H:%M:%S')
cleanData$Lpep_dropoff_datetime <- as.POSIXct(cleanData$Lpep_dropoff_datetime,format='%Y-%m-%d %H:%M:%S')

cleanData$month <- month(cleanData$lpep_pickup_datetime)
cleanData$wday  <- wday(cleanData$lpep_pickup_datetime)
cleanData$hour  <- hour(cleanData$lpep_pickup_datetime)

cleanData$DropOffhour  <- hour(cleanData$Lpep_dropoff_datetime)
summary(cleanData)
cleanData$duration <- floor(as.double(cleanData$Lpep_dropoff_datetime-cleanData$lpep_pickup_datetime)/60.0)
#cleanData$TipPercentOnFareAmount  <- (cleanData$Tip_amount/cleanData$Fare_amount) * 100.0
cleanData$speed<- (60* cleanData$Trip_distance/cleanData$duration)
cleanData$speed[cleanData$speed=="NaN"]<-0
cleanData$speed[cleanData$speed=="Inf"]<-0

cleanData$duration[cleanData$duration=="NA"]<-0

#cleanData$BySpeed<-(cleanData$Payment_type==1 && cleanData$speed<80 && cleanData$Tip_Percentage<50) 
#write.csv(cleanData, file = "greenTaxi1.csv", row.names = FALSE)

newLevel<-c(rep('0-6am',6),rep('6-9am',3),rep('9am-4pm',7),rep('4-7pm',3),rep('7-12pm',5))
levels(cleanData$hour)<-newLevel
levels
str(cleanData)
summary(cleanData)
#Applying Regression
library(lattice)
#splom(~cleanData[c(10,11,15)], groups = NULL, data = cleanData, axis.line.tck =0,axis.text.alpha=0)
VendorID<-as.factor(cleanData$VendorID)
Passenger_count<-as.factor(cleanData$Passenger_count)
Trip_distance<-as.factor(cleanData$Trip_distance)
Total_amount<-as.factor(cleanData$Total_amount)
Payment_type<-as.factor(cleanData$Payment_type)
Hour<-as.factor(cleanData$hour)
Week<-as.factor(cleanData$wday)
Month_day<-as.factor(cleanData$month)
duration<-as.factor(cleanData$duration)
Speed_mph<-as.factor(cleanData$speed)
Tolls_amount<-as.factor(cleanData$Tolls_amount)
Extra<-as.factor(cleanData$Extra)
## 2% of the sample size
smp_size <- floor(0.02 * nrow(cleanData))

## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(cleanData)), size = smp_size)

trainn <- cleanData[train_ind, ]
test <- cleanData[-train_ind, ]
summary(trainn)
str(trainn)

results <-lm(formula = Tip_Percentage ~  
               Total_amount +Passenger_count +speed +Tolls_amount+month+Trip_distance+duration+hour+wday+Extra+Payment_type
             , data = trainn)

results <-lm(formula = Tip_Percentage ~  
               Total_amount +speed +Tolls_amount+Trip_distance+duration+Payment_type
             , data = trainn)
summary(results)


library("rpart")
library("rpart.plot")
tip_decision<-trainn
tip_decision
summary(tip_decision)

fit <- rpart(Tip_Percentage ~ 
               Total_amount +Passenger_count +speed +Tolls_amount+month+Trip_distance+duration+hour+wday+Extra+Payment_type
             ,data = trainn, method="anova")
summary(fit)
rpart.plot(fit)

plot(fit)
text(fit)

Fit2 <-rpart(Tip_Percentage ~  
               Total_amount +speed +Tolls_amount+Trip_distance+duration+Payment_type
             , data = trainn,method="anova")

summary(Fit2)
rpart.plot(Fit2)

plot(Fit2)
text(Fit2)

Fit3 <-rpart(Tip_Percentage ~  
               Total_amount +speed +Trip_distance+duration+Payment_type
             , data = trainn,method="anova")
summary(Fit3)
rpart.plot(Fit3)

plot(Fit3)
text(Fit3)

Fit4 <-rpart(Tip_Percentage ~  
               Total_amount +speed +Trip_distance+duration
             , data = trainn,method="anova")
summary(Fit4)
rpart.plot(Fit4)

plot(Fit4)
text(Fit4)

Fit5 <-rpart(Tip_Percentage ~  
               Total_amount  +Trip_distance+duration
             , data = trainn,method="anova")
summary(Fit5)
rpart.plot(Fit5)

plot(Fit5)
text(Fit5)

Fit6 <-rpart(Tip_Percentage ~  
               Total_amount +Trip_distance
             ,data = trainn,method="anova")

summary(Fit6)
rpart.plot(Fit6)

plot(Fit6)
text(Fit6)


#write.csv(trainn, file = "mytaxidectree.csv", row.names = FALSE)

summary(Fit6)
rpart.plot(Fit6, type = 4,extra = 0)



library(ggplot2)
## 2% of the sample size
s <- floor(0.00002 * nrow(cleanData))

## set the seed to make your partition reproductible
set.seed(1234)
train_ind <- sample(seq_len(nrow(cleanData)), size = s)

trainCluster <- cleanData[train_ind, ]

summary(trainCluster)
str(trainCluster)
clusterPlot <- function(type) {
  clusters <- hclust(dist(trainCluster[, 10:26]), method = type)
  plot(clusters)
  
  clusterCut <- cutree(clusters, 3)
  show(table(clusterCut, trainCluster$RateCodeID)) # show required, else will not print
  
}
d <- dist(trainCluster, method = "euclidean") # distance matrix
fit <- hclust(d, method="ward")
plot(fit) # display dendogram
clusterPlot('ward.D')

#write.csv(trainn, file = "mygreen.csv", row.names = FALSE)

################################

#Report mean and median trip distance grouped by hour of day.
library(ggplot2)
library(dplyr)
library(plyr)


cdata <- ddply(cleanData, c("Hour"), summarise,
               N    = length(Trip_distance),
               mean = mean(Trip_distance),
               sd   = sd(Trip_distance),
               se   = sd / sqrt(N))
summary(cdata)
str(cdata)
cdata

############################################
cleanData$airport_trips <- data((cleanData$RateCodeID==2) | (cleanData$RateCodeID==3))

summary(cleanData$RateCodeID==2 | cleanData$RateCodeID==3)
Total_aiport_trips<-summary(cleanData$RateCodeID==2 | cleanData$RateCodeID==3)
Total_aiport_trips
Total_trips<- filter(cleanData, cleanData$RateCodeID==2 | cleanData$RateCodeID==3)
count(Total_trips)
nrow(Total_trips)

####Average fare and total fare 

AverageFareAmount <- mean(Total_trips$Fare_amount)
#Average Fare amount for Airport Trip
AverageFareAmount
AverageTotalAmount <-mean(Total_trips$Total_amount)
#Average Total amount for Airport Trip
AverageTotalAmount


AverageFareAmountallTrips <- mean(cleanData$Fare_amount)
#Average Fare amount for all the Trip
AverageFareAmountallTrips
AverageTotalAmountallTrips <-mean(cleanData$Total_amount)
#Average Total amount for all the Trip
AverageTotalAmountallTrips

#Trips distribution by trip distances and hour of the day

# Airport Trip Distance
AirportDist = Total_trips$Trip_distance # airport trips
AvgAirportDist = mean(AirportDist)
#Average Airport Distance
AvgAirportDist

#Average Total Distance
AverageDist = mean(cleanData$Trip_distance)
AverageDist

# Airport Trip Hour
AirportHour = Total_trips$hour # airport trips
AvgAirportHour = mean(AirportHour)
#Average Airport Hour
AvgAirportHour

#Average Total Hour
AverageHour = mean(cleanData$hour)
AverageHour

# Airport Trip Duration
AirportDuration = Total_trips$duration # airport trips
AvgAirportDuration = mean(AirportDuration)
#Average Airport Duration
AvgAirportDuration

#Average Total Duration
AverageDuration = mean(cleanData$duration)
AverageDuration

# Airport Trip Tip amount
AirportTip = Total_trips$Tip_amount # airport trips
AvgAirportTip = mean(AirportTip)
#Average Airport Tip Amount
AvgAirportTip

#Average Total Tip Amount
AverageTip = mean(cleanData$Tip_amount)
AverageTip


#Report any structure you find and any hypotheses you have about that structure

set1 = cleanData[cleanData$Tip_amount>0]
set2 = cleanData[cleanData$Tip_amount==0]
#Total passenger paying Tip
nrow(set1)
#Total passenger not paying Tip
nrow(set2)


require(graphics)
cleanData$Tip_Percentage[cleanData$Tip_Percentage=="NA"]<-0
offer<-sample(c(Payment_type),size = 500,replace = T)
amountPur<-sample(c(Total_amount),size = 500,replace = T)
offertest<-data.frame(offer=as.factor(offer))
model<-aov(amountPur ~ offer,  data = offertest)
summary(model)

require(graphics)
offer<-sample(c(Payment_type,Trip_distance),size = 500,replace = T)
amountPur<-sample(c(Total_amount),size = 500,replace = T)
offertest<-data.frame(offer=as.factor(offer))
model<-aov(amountPur ~ offer,  data = offertest)
summary(model)


#Clustering based on Payment type and Rate code ID or Trip Type
library(plyr)
library(ggplot2)
library(cluster)
library(lattice)
library(grid)
library(gridExtra)
kmdata_orig<-as.matrix(trainCluster[,c("Payment_type","speed", "Trip_type","Total_amount","duration")])
kmdata_orig[1:10,]
wss<-numeric(15)
for(k in 1:15) wss[k]<-sum(kmeans(kmdata_orig, centers = k, nstart = 25)$withinss)
km = kmeans(kmdata_orig, 3, nstart = 25)
km
c(wss[3], sum(km$withinss))

#preparation of the data and clustering results
df = as.data.frame(kmdata_orig[,2:4])
df$cluster = factor(km$cluster)
centers=as.data.frame(km$centers)

g1=ggplot(data = df, aes(x=speed, y =Total_amount, color=cluster ))+
  geom_point()+theme(legend.position = "right")+
  geom_point(data=centers, aes(x=speed, y =Total_amount, color=as.factor(c(1,2,3))),
             size=10,alpha=.3,show.legend =FALSE)
tmp=ggplot_gtable(ggplot_build(g1))
g1
g2=ggplot(data = df, aes(x=speed, y =Trip_type, color=cluster ))+
  geom_point()+theme(legend.position = "right")+
  geom_point(data=centers, aes(x=speed, y =Trip_type, color=as.factor(c(1,2,3))),
             size=10,alpha=.3,show.legend =FALSE)
tmp=ggplot_gtable(ggplot_build(g2))
g2


g3=ggplot(data = df, aes(x=Trip_type, y =Total_amount, color=cluster ))+
  geom_point()+theme(legend.position = "right")+
  geom_point(data=centers, aes(x=Trip_type, y =Total_amount, color=as.factor(c(1,2,3))),
             size=10,alpha=.3,show.legend =FALSE)
tmp=ggplot_gtable(ggplot_build(g3))
g3




#Clustering based on Payment type and Rate code ID or Trip Type
library(plyr)
library(ggplot2)
library(cluster)
library(lattice)
library(grid)
library(gridExtra)
kmdata_orig<-as.matrix(trainCluster[,c("Payment_type","speed", "Trip_type","Total_amount")])
kmdata_orig[1:10,]
wss<-numeric(15)
for(k in 1:15) wss[k]<-sum(kmeans(kmdata_orig, centers = k, nstart = 25)$withinss)
km = kmeans(kmdata_orig, 3, nstart = 25)
km
c(wss[3], sum(km$withinss))

#preparation of the data and clustering results
df = as.data.frame(kmdata_orig[,2:4])
df$cluster = factor(km$cluster)
centers=as.data.frame(km$centers)

g1=ggplot(data = df, aes(x=speed, y =Total_amount, color=cluster ))+
  geom_point()+theme(legend.position = "right")+
  geom_point(data=centers, aes(x=speed, y =Total_amount, color=as.factor(c(1,2,3))),
             size=10,alpha=.3,show.legend =FALSE)
tmp=ggplot_gtable(ggplot_build(g1))
g1

g2=ggplot(data = df, aes(x=speed, y =Trip_type, color=cluster ))+
  geom_point()+theme(legend.position = "right")+
  geom_point(data=centers, aes(x=speed, y =Trip_type, color=as.factor(c(1,2,3))),
             size=10,alpha=.3,show.legend =FALSE)
tmp=ggplot_gtable(ggplot_build(g2))
g2


g3=ggplot(data = df, aes(x=Trip_type, y =Total_amount, color=cluster ))+
  geom_point()+theme(legend.position = "right")+
  geom_point(data=centers, aes(x=Trip_type, y =Total_amount, color=as.factor(c(1,2,3))),
             size=10,alpha=.3,show.legend =FALSE)
tmp=ggplot_gtable(ggplot_build(g3))
g3


















