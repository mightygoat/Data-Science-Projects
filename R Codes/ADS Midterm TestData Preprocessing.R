test <-as.data.frame(read.csv("test.csv"))
test

str(test)
summary(test)
str(test)
summary(test)

#removed the outlier of the column Medical_History_1
clean_testData=subset(test,Medical_History_1<100 )
#removed the columns containg more than 60% NAs
clean_testData=subset(clean_testData, select = -c(Product_Info_2,Product_Info_3,Employment_Info_2,InsuredInfo_3,Medical_History_2, Medical_History_10, Medical_History_15, Medical_History_24, Medical_History_32,Family_Hist_5,Family_Hist_2,Family_Hist_3) )
summary(clean_testData)

test <- clean_testData

#Product
#unique(data_frame_out$Product_Info_1)
test$Product_Info_1[test$Product_Info_1==1 ] <-  0
test$Product_Info_1[test$Product_Info_1==2 ] <-  1

test$Product_Info_5[test$Product_Info_5==2 ] <-  0
test$Product_Info_5[test$Product_Info_5==3 ] <-  1

test$Product_Info_6[test$Product_Info_6==1 ] <-  0
test$Product_Info_6[test$Product_Info_6==3 ] <-  1

#Add cols for prodcut_info-7
test$Product_Info_7_1 <- 0
test$Product_Info_7_2 <- 0
test$Product_Info_7_3 <- 0
test$Product_Info_7_1[test$Product_Info_7 == 1] <- 1
test$Product_Info_7_2[test$Product_Info_7 == 2] <- 1
test$Product_Info_7_3[test$Product_Info_7 == 3] <- 1
test$Product_Info_7 <- NULL


test$Employment_Info_3[test$Employment_Info_3==1 ] <-  0
test$Employment_Info_3[test$Employment_Info_3==3 ] <-  1

test$Employment_Info_5[test$Employment_Info_5==2 ] <-  0
test$Employment_Info_5[test$Employment_Info_5==3 ] <-  1

test$InsuredInfo_1_1 <- 0
test$InsuredInfo_1_2 <- 0
test$InsuredInfo_1_3 <- 0
test$InsuredInfo_1_1[test$InsuredInfo_1==1]=1
test$InsuredInfo_1_2[test$InsuredInfo_1==2]=1
test$InsuredInfo_1_3[test$InsuredInfo_1==3]=1
test$InsuredInfo_1=NULL

test$InsuredInfo_2[test$InsuredInfo_2==2]=0
test$InsuredInfo_2[test$InsuredInfo_2==3]=1

test$InsuredInfo_4[test$InsuredInfo_4==2]=0
test$InsuredInfo_4[test$InsuredInfo_4==3]=1

test$InsuredInfo_5[test$InsuredInfo_5==1]=0
test$InsuredInfo_5[test$InsuredInfo_5==3]=1

test$InsuredInfo_6[test$InsuredInfo_6==1]=0
test$InsuredInfo_6[test$InsuredInfo_6==2]=1

test$InsuredInfo_7[test$InsuredInfo_7==1]=0
test$InsuredInfo_7[test$InsuredInfo_7==3]=1

#Insurance_History_1
test$Insurance_History_1[test$Insurance_History_1==1]=0
test$Insurance_History_1[test$Insurance_History_1==2]=1

#Insurance_History_2
test$Insurance_History_2_1 <- 0
test$Insurance_History_2_2 <- 0
test$Insurance_History_2_3 <- 0
test$Insurance_History_2_1[test$Insurance_History_2==1]=1
test$Insurance_History_2_2[test$Insurance_History_2==2]=1
test$Insurance_History_2_3[test$Insurance_History_2==3]=1
test$Insurance_History_2=NULL

#Insurance_History_3
test$Insurance_History_3_1 <- 0
test$Insurance_History_3_2 <- 0
test$Insurance_History_3_3 <- 0
test$Insurance_History_3_1[test$Insurance_History_3==1]=1
test$Insurance_History_3_2[test$Insurance_History_3==2]=1
test$Insurance_History_3_3[test$Insurance_History_3==3]=1
test$Insurance_History_3=NULL

#Insurance_History_4
test$Insurance_History_4_1 <- 0
test$Insurance_History_4_2 <- 0
test$Insurance_History_4_3 <- 0
test$Insurance_History_4_1[test$Insurance_History_4==1]=1
test$Insurance_History_4_2[test$Insurance_History_4==2]=1
test$Insurance_History_4_3[test$Insurance_History_4==3]=1
test$Insurance_History_4=NULL

#Insurance_History_7
test$Insurance_History_7_1 <- 0
test$Insurance_History_7_2 <- 0
test$Insurance_History_7_3 <- 0
test$Insurance_History_7_1[test$Insurance_History_7==1]=1
test$Insurance_History_7_2[test$Insurance_History_7==2]=1
test$Insurance_History_7_3[test$Insurance_History_7==3]=1
test$Insurance_History_7=NULL

#Insurance_History_8
test$Insurance_History_8_1 <- 0
test$Insurance_History_8_2 <- 0
test$Insurance_History_8_3 <- 0
test$Insurance_History_8_1[test$Insurance_History_8==1]=1
test$Insurance_History_8_2[test$Insurance_History_8==2]=1
test$Insurance_History_8_3[test$Insurance_History_8==3]=1
test$Insurance_History_8=NULL

#Insurance_History_9
test$Insurance_History_9_1 <- 0
test$Insurance_History_9_2 <- 0
test$Insurance_History_9_3 <- 0
test$Insurance_History_9_1[test$Insurance_History_9==1]=1
test$Insurance_History_9_2[test$Insurance_History_9==2]=1
test$Insurance_History_9_3[test$Insurance_History_9==3]=1
test$Insurance_History_9=NULL


#Family_Hist_1
test$Family_Hist_1_1 <- 0
test$Family_Hist_1_2 <- 0
test$Family_Hist_1_3 <- 0
test$Family_Hist_1_1[test$Family_Hist_1==1]=1
test$Family_Hist_1_2[test$Family_Hist_1==2]=1
test$Family_Hist_1_3[test$Family_Hist_1==3]=1
test$Family_Hist_1=NULL


# Getting the unique values of Medical_History_3
# unique(test$Medical_History_3)
test$Medical_History_3.1 = 0
test$Medical_History_3.2 = 0
test$Medical_History_3.3 = 0
test$Medical_History_3.1[test$Medical_History_5==0] = 1
test$Medical_History_3.2[test$Medical_History_5==1] = 1
test$Medical_History_3.3[test$Medical_History_5==2] = 1
test$Medical_History_3=NULL



# Getting the unique values of Medical_History_6
test$Medical_History_6 = factor(test$Medical_History_6,labels = c(0,1,2))
test$Medical_History_6.1 = 0
test$Medical_History_6.2 = 0
test$Medical_History_6.3 = 0
test$Medical_History_6.1[test$Medical_History_6==0] = 1
test$Medical_History_6.2[test$Medical_History_6==1] = 1
test$Medical_History_6.3[test$Medical_History_6==2] = 1
test$Medical_History_6=NULL
is.factor(test$Medical_History_6)
# Getting the unique values of Medical_History_7
test$Medical_History_7 = factor(test$Medical_History_7,labels = c(0,1,2))
test$Medical_History_7.1 = 0
test$Medical_History_7.2 = 0
test$Medical_History_7.3 = 0
test$Medical_History_7.1[test$Medical_History_7==0] = 1
test$Medical_History_7.2[test$Medical_History_7==1] = 1
test$Medical_History_7.3[test$Medical_History_7==2] = 1
test$Medical_History_7=NULL

# Getting the unique values of Medical_History_8
test$Medical_History_8 = factor(test$Medical_History_8,labels = c(0,1,2))
test$Medical_History_8.1 = 0
test$Medical_History_8.2 = 0
test$Medical_History_8.3 = 0
test$Medical_History_8.1[test$Medical_History_8==0] = 1
test$Medical_History_8.2[test$Medical_History_8==1] = 1
test$Medical_History_8.3[test$Medical_History_8==2] = 1
test$Medical_History_8=NULL

# Getting the unique values of Medical_History_9
test$Medical_History_9 = factor(test$Medical_History_9,labels = c(0,1,2))
test$Medical_History_9.1 = 0
test$Medical_History_9.2 = 0
test$Medical_History_9.3 = 0
test$Medical_History_9.1[test$Medical_History_9==0] = 1
test$Medical_History_9.2[test$Medical_History_9==1] = 1
test$Medical_History_9.3[test$Medical_History_9==2] = 1
test$Medical_History_9=NULL

# Getting the unique values of Medical_History_11
test$Medical_History_11 = factor(test$Medical_History_11,labels = c(0,1,2))
test$Medical_History_11.1 = 0
test$Medical_History_11.2 = 0
test$Medical_History_11.3 = 0
test$Medical_History_11.1[test$Medical_History_11==0] = 1
test$Medical_History_11.2[test$Medical_History_11==1] = 1
test$Medical_History_11.3[test$Medical_History_11==2] = 1
test$Medical_History_11=NULL


# Getting the unique values of Medical_History_12
test$Medical_History_12 = factor(test$Medical_History_12,labels = c(0,1,2))
test$Medical_History_12.1 = 0
test$Medical_History_12.2 = 0
test$Medical_History_12.3 = 0
test$Medical_History_12.1[test$Medical_History_12==0] = 1
test$Medical_History_12.2[test$Medical_History_12==1] = 1
test$Medical_History_12.3[test$Medical_History_12==2] = 1
test$Medical_History_12=NULL


# Getting the unique values of Medical_History_13
test$Medical_History_13 = factor(test$Medical_History_13,labels = c(0,1,2))
test$Medical_History_13.1 = 0
test$Medical_History_13.2 = 0
test$Medical_History_13.3 = 0
test$Medical_History_13.1[test$Medical_History_13==0] = 1
test$Medical_History_13.2[test$Medical_History_13==1] = 1
test$Medical_History_13.3[test$Medical_History_13==2] = 1
test$Medical_History_13=NULL

# Getting the unique values of Medical_History_14
test$Medical_History_14 = factor(test$Medical_History_14,labels = c(0,1,2))
test$Medical_History_14.1 = 0
test$Medical_History_14.2 = 0
test$Medical_History_14.3 = 0
test$Medical_History_14.1[test$Medical_History_14==0] = 1
test$Medical_History_14.2[test$Medical_History_14==1] = 1
test$Medical_History_14.3[test$Medical_History_14==2] = 1
test$Medical_History_14=NULL


# Getting the unique values of Medical_History_16
test$Medical_History_16 = factor(test$Medical_History_16,labels = c(0,1,2))
test$Medical_History_16.1 = 0
test$Medical_History_16.2 = 0
test$Medical_History_16.3 = 0
test$Medical_History_16.1[test$Medical_History_16==0] = 1
test$Medical_History_16.2[test$Medical_History_16==1] = 1
test$Medical_History_16.3[test$Medical_History_16==2] = 1
test$Medical_History_16=NULL

# Getting the unique values of Medical_History_17
test$Medical_History_17 = factor(test$Medical_History_17,labels = c(0,1,2))
test$Medical_History_17.1 = 0
test$Medical_History_17.2 = 0
test$Medical_History_17.3 = 0
test$Medical_History_17.1[test$Medical_History_17==0] = 1
test$Medical_History_17.2[test$Medical_History_17==1] = 1
test$Medical_History_17.3[test$Medical_History_17==2] = 1
test$Medical_History_17=NULL


# Getting the unique values of Medical_History_18
test$Medical_History_18 = factor(test$Medical_History_18,labels = c(0,1,2))
test$Medical_History_18.1 = 0
test$Medical_History_18.2 = 0
test$Medical_History_18.3 = 0
test$Medical_History_18.1[test$Medical_History_18==0] = 1
test$Medical_History_18.2[test$Medical_History_18==1] = 1
test$Medical_History_18.3[test$Medical_History_18==2] = 1
test$Medical_History_18=NULL



# Getting the unique values of Medical_History_19
test$Medical_History_19 = factor(test$Medical_History_19,labels = c(0,1,2))
test$Medical_History_19.1 = 0
test$Medical_History_19.2 = 0
test$Medical_History_19.3 = 0
test$Medical_History_19.1[test$Medical_History_19==0] = 1
test$Medical_History_19.2[test$Medical_History_19==1] = 1
test$Medical_History_19.3[test$Medical_History_19==2] = 1
test$Medical_History_19=NULL


# Getting the unique values of Medical_History_20
test$Medical_History_20 = factor(test$Medical_History_20,labels = c(0,1,2))
test$Medical_History_20.1 = 0
test$Medical_History_20.2 = 0
test$Medical_History_20.3 = 0
test$Medical_History_20.1[test$Medical_History_20==0] = 1
test$Medical_History_20.2[test$Medical_History_20==1] = 1
test$Medical_History_20.3[test$Medical_History_20==2] = 1
test$Medical_History_20=NULL


# Getting the unique values of Medical_History_21
test$Medical_History_21 = factor(test$Medical_History_21,labels = c(0,1,2))
test$Medical_History_21.1 = 0
test$Medical_History_21.2 = 0
test$Medical_History_21.3 = 0
test$Medical_History_21.1[test$Medical_History_21==0] = 1
test$Medical_History_21.2[test$Medical_History_21==1] = 1
test$Medical_History_21.3[test$Medical_History_21==2] = 1
test$Medical_History_21=NULL

# Getting the unique values of Medical_History_22
test$Medical_History_22[test$Medical_History_22==1 ] <-  0
test$Medical_History_22[test$Medical_History_22==2 ] <-  1

#add cols for medical_History_23
test$Medical_History_23_1 <- 0
test$Medical_History_23_2 <- 0
test$Medical_History_23_3 <- 0
test$Medical_History_23_1[test$Medical_History_23 == 1] <- 1
test$Medical_History_23_2[test$Medical_History_23 == 2] <- 1
test$Medical_History_23_3[test$Medical_History_23 == 3] <- 1
test$Medical_History_23 <- NULL


#add cols for medical_History_25
test$Medical_History_25_1 <- 0
test$Medical_History_25_2 <- 0
test$Medical_History_25_3 <- 0
test$Medical_History_25_1[test$Medical_History_25 == 1] <- 1
test$Medical_History_25_2[test$Medical_History_25 == 2] <- 1
test$Medical_History_25_3[test$Medical_History_25 == 3] <- 1
test$Medical_History_25 <- NULL

#add cols for medical_History_26
test$Medical_History_26_1 <- 0
test$Medical_History_26_2 <- 0
test$Medical_History_26_3 <- 0
test$Medical_History_26_1[test$Medical_History_26 == 1] <- 1
test$Medical_History_26_2[test$Medical_History_26 == 2] <- 1
test$Medical_History_26_3[test$Medical_History_26 == 3] <- 1
test$Medical_History_26 <- NULL


#add cols for medical_History_27
test$Medical_History_27_1 <- 0
test$Medical_History_27_2 <- 0
test$Medical_History_27_3 <- 0
test$Medical_History_27_1[test$Medical_History_27 == 1] <- 1
test$Medical_History_27_2[test$Medical_History_27 == 2] <- 1
test$Medical_History_27_3[test$Medical_History_27 == 3] <- 1
test$Medical_History_27 <- NULL


#add cols for medical_History_28
test$Medical_History_28_1 <- 0
test$Medical_History_28_2 <- 0
test$Medical_History_28_3 <- 0
test$Medical_History_28_1[test$Medical_History_28 == 1] <- 1
test$Medical_History_28_2[test$Medical_History_28 == 2] <- 1
test$Medical_History_28_3[test$Medical_History_28 == 3] <- 1
test$Medical_History_28 <- NULL

#add cols for medical_History_29
test$Medical_History_29_1 <- 0
test$Medical_History_29_2 <- 0
test$Medical_History_29_3 <- 0
test$Medical_History_29_1[test$Medical_History_29 == 1] <- 1
test$Medical_History_29_2[test$Medical_History_29 == 2] <- 1
test$Medical_History_29_3[test$Medical_History_29 == 3] <- 1
test$Medical_History_29 <- NULL


#add cols for medical_History_30
test$Medical_History_30_1 <- 0
test$Medical_History_30_2 <- 0
test$Medical_History_30_3 <- 0
test$Medical_History_30_1[test$Medical_History_30 == 1] <- 1
test$Medical_History_30_2[test$Medical_History_30 == 2] <- 1
test$Medical_History_30_3[test$Medical_History_30 == 3] <- 1
test$Medical_History_30 <- NULL


#add cols for medical_History_31
test$Medical_History_31_1 <- 0
test$Medical_History_31_2 <- 0
test$Medical_History_31_3 <- 0
test$Medical_History_31_1[test$Medical_History_31 == 1] <- 1
test$Medical_History_31_2[test$Medical_History_31 == 2] <- 1
test$Medical_History_31_3[test$Medical_History_31 == 3] <- 1
test$Medical_History_31 <- NULL

#transfer Medical_History-33
test$Medical_History_33[test$Medical_History_33==1 ] <-  0
test$Medical_History_33[test$Medical_History_33==3 ] <-  1

#add cols for Medical_History_34
test$Medical_History_34_1 <- 0
test$Medical_History_34_2 <- 0
test$Medical_History_34_3 <- 0
test$Medical_History_34_1[test$Medical_History_34 == 1] <- 1
test$Medical_History_34_2[test$Medical_History_34 == 2] <- 1
test$Medical_History_34_3[test$Medical_History_34 == 3] <- 1
test$Medical_History_34 <- NULL

#add cols for Medical_History_35
# unique(test$Medical_History_35)
test$Medical_History_35_1 <- 0
test$Medical_History_35_2 <- 0
test$Medical_History_35_3 <- 0
test$Medical_History_35_1[test$Medical_History_35 == 1] <- 1
test$Medical_History_35_2[test$Medical_History_35 == 2] <- 1
test$Medical_History_35_3[test$Medical_History_35 == 3] <- 1
test$Medical_History_35 <- NULL


#add cols for Medical_History_36
test$Medical_History_36_1 <- 0
test$Medical_History_36_2 <- 0
test$Medical_History_36_3 <- 0
test$Medical_History_36_1[test$Medical_History_36 == 1] <- 1
test$Medical_History_36_2[test$Medical_History_36 == 2] <- 1
test$Medical_History_36_3[test$Medical_History_36 == 3] <- 1
test$Medical_History_36 <- NULL

#add cols for Medical_History_37
test$Medical_History_37_1 <- 0
test$Medical_History_37_2 <- 0
test$Medical_History_37_3 <- 0
test$Medical_History_37_1[test$Medical_History_37 == 1] <- 1
test$Medical_History_37_2[test$Medical_History_37 == 2] <- 1
test$Medical_History_37_3[test$Medical_History_37 == 3] <- 1
test$Medical_History_37 <- NULL

# Medical_History_38
test$Medical_History_38[test$Medical_History_38 == 1] <- 0
test$Medical_History_38[test$Medical_History_38 == 2] <- 1


#add cols for Medical_History_39
# unique(test$Medical_History_39)
test$Medical_History_39_1 <- 0
test$Medical_History_39_2 <- 0
test$Medical_History_39_3 <- 0
test$Medical_History_39_1[test$Medical_History_39 == 1] <- 1
test$Medical_History_39_2[test$Medical_History_39 == 2] <- 1
test$Medical_History_39_3[test$Medical_History_39 == 3] <- 1
test$Medical_History_39 <- NULL

#add cols for Medical_History_40
unique(test$Medical_History_40)
test$Medical_History_40_1 <- 0
test$Medical_History_40_2 <- 0
test$Medical_History_40_3 <- 0
test$Medical_History_40_1[test$Medical_History_40 == 1] <- 1
test$Medical_History_40_2[test$Medical_History_40 == 2] <- 1
test$Medical_History_40_3[test$Medical_History_40 == 3] <- 1
test$Medical_History_40 <-NULL


#add cols for Medical_History_41
test$Medical_History_41_1 <- 0
test$Medical_History_41_2 <- 0
test$Medical_History_41_3 <- 0
test$Medical_History_41_1[test$Medical_History_41 == 1] <- 1
test$Medical_History_41_2[test$Medical_History_41 == 2] <- 1
test$Medical_History_41_3[test$Medical_History_41 == 3] <- 1
test$Medical_History_41 <- NULL



#Replace the values with max values
hist(test$Employment_Info_1)
hist(test$Employment_Info_4)
hist(test$Employment_Info_6)
hist(test$Family_Hist_4)
hist(test$Medical_History_1)

sort(table(test$Medical_History_1))
sort(table(test$Employment_Info_6))


for(i in 1:ncol(test)){
  test[is.na(test[,i]), i] <- mean(test[,i], na.rm = TRUE)
}

summary(test)

names(test)
ncol(test)

write.csv(test, file = "iciciTestData.csv", row.names = FALSE)


model1<-lm(trainset$Response~ Product_Info_4+Ins_Age+Ht+Wt+BMI
           +InsuredInfo_2+InsuredInfo_5+InsuredInfo_6+
             Family_Hist_4+Medical_History_4+Insurance_History_2_1+
             Medical_History_39_1+Medical_History_17.2+Medical_History_20.1+
             Medical_History_30_2+Medical_History_23_1+
             Medical_History_40_1
           ,trainset)
summary(model1)

predictionWithTest<-predict(model1, test, se.fit = FALSE,
                            scale = NULL, df = Inf,interval = c("none", "confidence", "prediction"),
                            level = 0.95, type = c("response", "terms"),terms = NULL, 
                            na.action = na.pass,pred.var = res.var/weights, weights = 1)
summary(predictionWithTest)

output<-round(predictionWithTest)
actualResponse<-test$Response
mean(test$Response-output)^2
str(test)
View(test)

write.csv(test, file = "TestData.csv", row.names = FALSE)



abline(lm(trainset$Response~ Product_Info_4+Ins_Age+Ht+Wt+BMI
          +InsuredInfo_2+InsuredInfo_5+InsuredInfo_6+
            Family_Hist_4+Medical_History_4+Insurance_History_2_1+
            Medical_History_39_1+Medical_History_17.2+Medical_History_20.1+
            Medical_History_30_2+Medical_History_23_1+
            Medical_History_40_1,trainset))



anova(model1) 

library(MASS)
anova(model2, model1)
step <- stepAIC(model2, direction="both")
step$anova # display results



