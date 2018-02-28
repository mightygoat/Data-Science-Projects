
library(data.table)
library(spatial)

library(jsonlite)
library(geojsonio)


nycjson<-geojson_read("https://raw.githubusercontent.com/dwillis/nyc-maps/master/boroughs.geojson",what='sp')
  
  
#'http://catalog.civicdashboards.com/dataset/c3555efe-cb95-48f5-8816-9083d1f30c3d/resource/57356e9e-e43c-44c0-9536-6e07ab9e2e75/download/ec25edd692b24248a2b70c95d0ed85fbtemp.geojson',what='sp')

Boroughs = c('Manhattan', 'Bronx', 'Brooklyn', 'Queens', 'Staten Island') 
summary(Boroughs)
nycZone<-nycjson
nycZone[,2:3]<-NULL #only keep the borough code
taxiGreen <- fread('https://s3.amazonaws.com/nyc-tlc/trip+data/green_tripdata_2015-09.csv',stringsAsFactors = F)


taxiGreen$dummy1 <- NULL  #handle the excess ',' in all the rows of the csv files
taxiGreen$dummy2 <- NULL


names(taxiGreen) = c("VendorID"  ,            "Pickup_datetime"  ,"Dropoff_datetime" ,"Store_and_fwd_flag" ,  
                      "RateCodeID"        ,    "Pickup_longitude"  ,    "Pickup_latitude"   ,    "Dropoff_longitude" ,   
                      "Dropoff_latitude"   ,   "Passenger_count"    ,   "Trip_distance"     ,    "Fare_amount"  ,        
                      "Extra"       ,          "MTA_tax"       ,        "Tip_amount"        ,    "Tolls_amount" ,        
                      "Ehail_fee"    ,         "improvement_surcharge" ,"Total_amount"      ,    "Payment_type"  ,       
                      "Trip_type" )


summary(taxiGreen)
library(sp)
library(rgdal)

PickupArea<-SpatialPoints(cbind(taxiGreen$Pickup_longitude,taxiGreen$Pickup_latitude))

PickupArea@proj4string <- nycjson@proj4string
pickupBoroughCodes<-PickupArea %over% nycZone
summary(pickupBoroughCodes)
taxiGreen$boroughCode_p <- pickupBoroughCodes$BoroName
#taxiGreen$boroughCode<-pickupBoroughCodes$name


DropoffPts<-SpatialPoints(cbind(taxiGreen$Dropoff_longitude,taxiGreen$Dropoff_latitude))
DropoffPts@proj4string <- nycjson@proj4string
dropoffBoroughCodes<-DropoffPts %over% nycZone
taxiGreen$boroughCodeDrop<-dropoffBoroughCodes$BoroName


taxiGreen$month <- month(taxiGreen$Pickup_datetime)
taxiGreen$wday  <- wday(taxiGreen$Pickup_datetime)
taxiGreen$hour  <- hour(taxiGreen$Pickup_datetime)

# Change the format of datetime from string to POSIXct objects

taxiGreen$Pickup_datetime <- as.POSIXct(taxiGreen$Pickup_datetime,format='%Y-%m-%d %H:%M:%S')
taxiGreen$Dropoff_datetime <- as.POSIXct(taxiGreen$Dropoff_datetime,format='%Y-%m-%d %H:%M:%S')

taxiGreen$duration <- floor(as.double(taxiGreen$Dropoff_datetime-taxiGreen$Pickup_datetime)/60.0)
taxiGreen$percent  <- taxiGreen$Tip_amount/taxiGreen$Fare_amount * 100.0
taxiGreen$speed    <- taxiGreen$Trip_distance/taxiGreen$duration *60

taxiGreen$speed[taxiGreen$speed=="NaN"]<-0


# future usage
write.csv(taxiGreen, file = "greenLocationBoro.csv", row.names = TRUE)

str(taxiGreen)

subset(taxiGreen,taxiGreen$Payment_type<3 && taxiGreen$Payment_type <3)
taxiGreen$speed[taxiGreen$speed=="NaN"]<-0
taxiGreen$BySpeed<-(taxiGreen$Payment_type==1 && taxiGreen$speed<80 && taxiGreen$Tip_Percentage<50) 


taxiGreen$green_clean<-(taxiGreen$Payment_type==1 && taxiGreen$speed<80 && taxiGreen$Tip_Percentage<50 && taxiGreen$Fare_amount<200 && taxiGreen$Total_amount<1000  &&  taxiGreen$Tolls_amount<30  &&  taxiGreen$Trip_distance<50&& taxiGreen$duration<120 
                        && taxiGreen$percent<100  &&  taxiGreen$speed<100 &&
                   !is.na(taxiGreen$boroughCode) && !is.na(taxiGreen$boroughCodeDrop))
summary(taxiGreen$green_clean)

require(plyr) 
DD <- ddply(taxiGreen, c("Hour"),summarise,
                      speed1 = mean(speed),duration1=mean(duration),
                      Payment_type1=mean(Payment_type), 
                      dist1=mean(Trip_distance),
                      percent1=mean(percent),count=n()/365.0) 


