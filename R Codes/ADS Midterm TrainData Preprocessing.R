
library(readxl)

train <-as.data.frame(read.csv("train.csv"))
train
test <-as.data.frame(read.csv("test.csv"))
test

str(train)
summary(train)
str(test)
summary(test)

#removed the outlier of the column Medical_History_1
clean_trainData=subset(train,Medical_History_1<100 )
#removed the columns containg more than 60% NAs
clean_trainData=subset(clean_trainData, select = -c(Product_Info_2,Product_Info_3,Employment_Info_2,InsuredInfo_3,Medical_History_2, Medical_History_10, Medical_History_15, Medical_History_24, Medical_History_32,Family_Hist_5,Family_Hist_2,Family_Hist_3) )
summary(clean_trainData)

train <- clean_trainData

#Product
#unique(data_frame_out$Product_Info_1)
train$Product_Info_1[train$Product_Info_1==1 ] <-  0
train$Product_Info_1[train$Product_Info_1==2 ] <-  1

train$Product_Info_5[train$Product_Info_5==2 ] <-  0
train$Product_Info_5[train$Product_Info_5==3 ] <-  1

train$Product_Info_6[train$Product_Info_6==1 ] <-  0
train$Product_Info_6[train$Product_Info_6==3 ] <-  1

#Add cols for prodcut_info-7
train$Product_Info_7_1 <- 0
train$Product_Info_7_2 <- 0
train$Product_Info_7_3 <- 0
train$Product_Info_7_1[train$Product_Info_7 == 1] <- 1
train$Product_Info_7_2[train$Product_Info_7 == 2] <- 1
train$Product_Info_7_3[train$Product_Info_7 == 3] <- 1
train$Product_Info_7 <- NULL


train$Employment_Info_3[train$Employment_Info_3==1 ] <-  0
train$Employment_Info_3[train$Employment_Info_3==3 ] <-  1

train$Employment_Info_5[train$Employment_Info_5==2 ] <-  0
train$Employment_Info_5[train$Employment_Info_5==3 ] <-  1

train$InsuredInfo_1_1 <- 0
train$InsuredInfo_1_2 <- 0
train$InsuredInfo_1_3 <- 0
train$InsuredInfo_1_1[train$InsuredInfo_1==1]=1
train$InsuredInfo_1_2[train$InsuredInfo_1==2]=1
train$InsuredInfo_1_3[train$InsuredInfo_1==3]=1
train$InsuredInfo_1=NULL

train$InsuredInfo_2[train$InsuredInfo_2==2]=0
train$InsuredInfo_2[train$InsuredInfo_2==3]=1

train$InsuredInfo_4[train$InsuredInfo_4==2]=0
train$InsuredInfo_4[train$InsuredInfo_4==3]=1

train$InsuredInfo_5[train$InsuredInfo_5==1]=0
train$InsuredInfo_5[train$InsuredInfo_5==3]=1

train$InsuredInfo_6[train$InsuredInfo_6==1]=0
train$InsuredInfo_6[train$InsuredInfo_6==2]=1

train$InsuredInfo_7[train$InsuredInfo_7==1]=0
train$InsuredInfo_7[train$InsuredInfo_7==3]=1

#Insurance_History_1
train$Insurance_History_1[train$Insurance_History_1==1]=0
train$Insurance_History_1[train$Insurance_History_1==2]=1

#Insurance_History_2
train$Insurance_History_2_1 <- 0
train$Insurance_History_2_2 <- 0
train$Insurance_History_2_3 <- 0
train$Insurance_History_2_1[train$Insurance_History_2==1]=1
train$Insurance_History_2_2[train$Insurance_History_2==2]=1
train$Insurance_History_2_3[train$Insurance_History_2==3]=1
train$Insurance_History_2=NULL

#Insurance_History_3
train$Insurance_History_3_1 <- 0
train$Insurance_History_3_2 <- 0
train$Insurance_History_3_3 <- 0
train$Insurance_History_3_1[train$Insurance_History_3==1]=1
train$Insurance_History_3_2[train$Insurance_History_3==2]=1
train$Insurance_History_3_3[train$Insurance_History_3==3]=1
train$Insurance_History_3=NULL

#Insurance_History_4
train$Insurance_History_4_1 <- 0
train$Insurance_History_4_2 <- 0
train$Insurance_History_4_3 <- 0
train$Insurance_History_4_1[train$Insurance_History_4==1]=1
train$Insurance_History_4_2[train$Insurance_History_4==2]=1
train$Insurance_History_4_3[train$Insurance_History_4==3]=1
train$Insurance_History_4=NULL

#Insurance_History_7
train$Insurance_History_7_1 <- 0
train$Insurance_History_7_2 <- 0
train$Insurance_History_7_3 <- 0
train$Insurance_History_7_1[train$Insurance_History_7==1]=1
train$Insurance_History_7_2[train$Insurance_History_7==2]=1
train$Insurance_History_7_3[train$Insurance_History_7==3]=1
train$Insurance_History_7=NULL

#Insurance_History_8
train$Insurance_History_8_1 <- 0
train$Insurance_History_8_2 <- 0
train$Insurance_History_8_3 <- 0
train$Insurance_History_8_1[train$Insurance_History_8==1]=1
train$Insurance_History_8_2[train$Insurance_History_8==2]=1
train$Insurance_History_8_3[train$Insurance_History_8==3]=1
train$Insurance_History_8=NULL

#Insurance_History_9
train$Insurance_History_9_1 <- 0
train$Insurance_History_9_2 <- 0
train$Insurance_History_9_3 <- 0
train$Insurance_History_9_1[train$Insurance_History_9==1]=1
train$Insurance_History_9_2[train$Insurance_History_9==2]=1
train$Insurance_History_9_3[train$Insurance_History_9==3]=1
train$Insurance_History_9=NULL


#Family_Hist_1
train$Family_Hist_1_1 <- 0
train$Family_Hist_1_2 <- 0
train$Family_Hist_1_3 <- 0
train$Family_Hist_1_1[train$Family_Hist_1==1]=1
train$Family_Hist_1_2[train$Family_Hist_1==2]=1
train$Family_Hist_1_3[train$Family_Hist_1==3]=1
train$Family_Hist_1=NULL


# Getting the unique values of Medical_History_3
# unique(train$Medical_History_3)
train$Medical_History_3.1 = 0
train$Medical_History_3.2 = 0
train$Medical_History_3.3 = 0
train$Medical_History_3.1[train$Medical_History_5==0] = 1
train$Medical_History_3.2[train$Medical_History_5==1] = 1
train$Medical_History_3.3[train$Medical_History_5==2] = 1
train$Medical_History_3=NULL



# Getting the unique values of Medical_History_6
train$Medical_History_6 = factor(train$Medical_History_6,labels = c(0,1,2))
train$Medical_History_6.1 = 0
train$Medical_History_6.2 = 0
train$Medical_History_6.3 = 0
train$Medical_History_6.1[train$Medical_History_6==0] = 1
train$Medical_History_6.2[train$Medical_History_6==1] = 1
train$Medical_History_6.3[train$Medical_History_6==2] = 1
train$Medical_History_6=NULL

# Getting the unique values of Medical_History_7
train$Medical_History_7 = factor(train$Medical_History_7,labels = c(0,1,2))
train$Medical_History_7.1 = 0
train$Medical_History_7.2 = 0
train$Medical_History_7.3 = 0
train$Medical_History_7.1[train$Medical_History_7==0] = 1
train$Medical_History_7.2[train$Medical_History_7==1] = 1
train$Medical_History_7.3[train$Medical_History_7==2] = 1
train$Medical_History_7=NULL

# Getting the unique values of Medical_History_8
train$Medical_History_8 = factor(train$Medical_History_8,labels = c(0,1,2))
train$Medical_History_8.1 = 0
train$Medical_History_8.2 = 0
train$Medical_History_8.3 = 0
train$Medical_History_8.1[train$Medical_History_8==0] = 1
train$Medical_History_8.2[train$Medical_History_8==1] = 1
train$Medical_History_8.3[train$Medical_History_8==2] = 1
train$Medical_History_8=NULL

# Getting the unique values of Medical_History_9
train$Medical_History_9 = factor(train$Medical_History_9,labels = c(0,1,2))
train$Medical_History_9.1 = 0
train$Medical_History_9.2 = 0
train$Medical_History_9.3 = 0
train$Medical_History_9.1[train$Medical_History_9==0] = 1
train$Medical_History_9.2[train$Medical_History_9==1] = 1
train$Medical_History_9.3[train$Medical_History_9==2] = 1
train$Medical_History_9=NULL

# Getting the unique values of Medical_History_11
train$Medical_History_11 = factor(train$Medical_History_11,labels = c(0,1,2))
train$Medical_History_11.1 = 0
train$Medical_History_11.2 = 0
train$Medical_History_11.3 = 0
train$Medical_History_11.1[train$Medical_History_11==0] = 1
train$Medical_History_11.2[train$Medical_History_11==1] = 1
train$Medical_History_11.3[train$Medical_History_11==2] = 1
train$Medical_History_11=NULL


# Getting the unique values of Medical_History_12
train$Medical_History_12 = factor(train$Medical_History_12,labels = c(0,1,2))
train$Medical_History_12.1 = 0
train$Medical_History_12.2 = 0
train$Medical_History_12.3 = 0
train$Medical_History_12.1[train$Medical_History_12==0] = 1
train$Medical_History_12.2[train$Medical_History_12==1] = 1
train$Medical_History_12.3[train$Medical_History_12==2] = 1
train$Medical_History_12=NULL


# Getting the unique values of Medical_History_13
train$Medical_History_13 = factor(train$Medical_History_13,labels = c(0,1,2))
train$Medical_History_13.1 = 0
train$Medical_History_13.2 = 0
train$Medical_History_13.3 = 0
train$Medical_History_13.1[train$Medical_History_13==0] = 1
train$Medical_History_13.2[train$Medical_History_13==1] = 1
train$Medical_History_13.3[train$Medical_History_13==2] = 1
train$Medical_History_13=NULL

# Getting the unique values of Medical_History_14
train$Medical_History_14 = factor(train$Medical_History_14,labels = c(0,1,2))
train$Medical_History_14.1 = 0
train$Medical_History_14.2 = 0
train$Medical_History_14.3 = 0
train$Medical_History_14.1[train$Medical_History_14==0] = 1
train$Medical_History_14.2[train$Medical_History_14==1] = 1
train$Medical_History_14.3[train$Medical_History_14==2] = 1
train$Medical_History_14=NULL


# Getting the unique values of Medical_History_16
train$Medical_History_16 = factor(train$Medical_History_16,labels = c(0,1,2))
train$Medical_History_16.1 = 0
train$Medical_History_16.2 = 0
train$Medical_History_16.3 = 0
train$Medical_History_16.1[train$Medical_History_16==0] = 1
train$Medical_History_16.2[train$Medical_History_16==1] = 1
train$Medical_History_16.3[train$Medical_History_16==2] = 1
train$Medical_History_16=NULL

# Getting the unique values of Medical_History_17
train$Medical_History_17 = factor(train$Medical_History_17,labels = c(0,1,2))
train$Medical_History_17.1 = 0
train$Medical_History_17.2 = 0
train$Medical_History_17.3 = 0
train$Medical_History_17.1[train$Medical_History_17==0] = 1
train$Medical_History_17.2[train$Medical_History_17==1] = 1
train$Medical_History_17.3[train$Medical_History_17==2] = 1
train$Medical_History_17=NULL


# Getting the unique values of Medical_History_18
train$Medical_History_18 = factor(train$Medical_History_18,labels = c(0,1,2))
train$Medical_History_18.1 = 0
train$Medical_History_18.2 = 0
train$Medical_History_18.3 = 0
train$Medical_History_18.1[train$Medical_History_18==0] = 1
train$Medical_History_18.2[train$Medical_History_18==1] = 1
train$Medical_History_18.3[train$Medical_History_18==2] = 1
train$Medical_History_18=NULL



# Getting the unique values of Medical_History_19
train$Medical_History_19 = factor(train$Medical_History_19,labels = c(0,1,2))
train$Medical_History_19.1 = 0
train$Medical_History_19.2 = 0
train$Medical_History_19.3 = 0
train$Medical_History_19.1[train$Medical_History_19==0] = 1
train$Medical_History_19.2[train$Medical_History_19==1] = 1
train$Medical_History_19.3[train$Medical_History_19==2] = 1
train$Medical_History_19=NULL


# Getting the unique values of Medical_History_20
train$Medical_History_20 = factor(train$Medical_History_20,labels = c(0,1,2))
train$Medical_History_20.1 = 0
train$Medical_History_20.2 = 0
train$Medical_History_20.3 = 0
train$Medical_History_20.1[train$Medical_History_20==0] = 1
train$Medical_History_20.2[train$Medical_History_20==1] = 1
train$Medical_History_20.3[train$Medical_History_20==2] = 1
train$Medical_History_20=NULL


# Getting the unique values of Medical_History_21
train$Medical_History_21 = factor(train$Medical_History_21,labels = c(0,1,2))
train$Medical_History_21.1 = 0
train$Medical_History_21.2 = 0
train$Medical_History_21.3 = 0
train$Medical_History_21.1[train$Medical_History_21==0] = 1
train$Medical_History_21.2[train$Medical_History_21==1] = 1
train$Medical_History_21.3[train$Medical_History_21==2] = 1
train$Medical_History_21=NULL

# Getting the unique values of Medical_History_22
train$Medical_History_22[train$Medical_History_22==1 ] <-  0
train$Medical_History_22[train$Medical_History_22==2 ] <-  1

#add cols for medical_History_23
train$Medical_History_23_1 <- 0
train$Medical_History_23_2 <- 0
train$Medical_History_23_3 <- 0
train$Medical_History_23_1[train$Medical_History_23 == 1] <- 1
train$Medical_History_23_2[train$Medical_History_23 == 2] <- 1
train$Medical_History_23_3[train$Medical_History_23 == 3] <- 1
train$Medical_History_23 <- NULL


#add cols for medical_History_25
train$Medical_History_25_1 <- 0
train$Medical_History_25_2 <- 0
train$Medical_History_25_3 <- 0
train$Medical_History_25_1[train$Medical_History_25 == 1] <- 1
train$Medical_History_25_2[train$Medical_History_25 == 2] <- 1
train$Medical_History_25_3[train$Medical_History_25 == 3] <- 1
train$Medical_History_25 <- NULL

#add cols for medical_History_26
train$Medical_History_26_1 <- 0
train$Medical_History_26_2 <- 0
train$Medical_History_26_3 <- 0
train$Medical_History_26_1[train$Medical_History_26 == 1] <- 1
train$Medical_History_26_2[train$Medical_History_26 == 2] <- 1
train$Medical_History_26_3[train$Medical_History_26 == 3] <- 1
train$Medical_History_26 <- NULL


#add cols for medical_History_27
train$Medical_History_27_1 <- 0
train$Medical_History_27_2 <- 0
train$Medical_History_27_3 <- 0
train$Medical_History_27_1[train$Medical_History_27 == 1] <- 1
train$Medical_History_27_2[train$Medical_History_27 == 2] <- 1
train$Medical_History_27_3[train$Medical_History_27 == 3] <- 1
train$Medical_History_27 <- NULL


#add cols for medical_History_28
train$Medical_History_28_1 <- 0
train$Medical_History_28_2 <- 0
train$Medical_History_28_3 <- 0
train$Medical_History_28_1[train$Medical_History_28 == 1] <- 1
train$Medical_History_28_2[train$Medical_History_28 == 2] <- 1
train$Medical_History_28_3[train$Medical_History_28 == 3] <- 1
train$Medical_History_28 <- NULL

#add cols for medical_History_29
train$Medical_History_29_1 <- 0
train$Medical_History_29_2 <- 0
train$Medical_History_29_3 <- 0
train$Medical_History_29_1[train$Medical_History_29 == 1] <- 1
train$Medical_History_29_2[train$Medical_History_29 == 2] <- 1
train$Medical_History_29_3[train$Medical_History_29 == 3] <- 1
train$Medical_History_29 <- NULL


#add cols for medical_History_30
train$Medical_History_30_1 <- 0
train$Medical_History_30_2 <- 0
train$Medical_History_30_3 <- 0
train$Medical_History_30_1[train$Medical_History_30 == 1] <- 1
train$Medical_History_30_2[train$Medical_History_30 == 2] <- 1
train$Medical_History_30_3[train$Medical_History_30 == 3] <- 1
train$Medical_History_30 <- NULL


#add cols for medical_History_31
train$Medical_History_31_1 <- 0
train$Medical_History_31_2 <- 0
train$Medical_History_31_3 <- 0
train$Medical_History_31_1[train$Medical_History_31 == 1] <- 1
train$Medical_History_31_2[train$Medical_History_31 == 2] <- 1
train$Medical_History_31_3[train$Medical_History_31 == 3] <- 1
train$Medical_History_31 <- NULL

#transfer Medical_History-33
train$Medical_History_33[train$Medical_History_33==1 ] <-  0
train$Medical_History_33[train$Medical_History_33==3 ] <-  1

#add cols for Medical_History_34
train$Medical_History_34_1 <- 0
train$Medical_History_34_2 <- 0
train$Medical_History_34_3 <- 0
train$Medical_History_34_1[train$Medical_History_34 == 1] <- 1
train$Medical_History_34_2[train$Medical_History_34 == 2] <- 1
train$Medical_History_34_3[train$Medical_History_34 == 3] <- 1
train$Medical_History_34 <- NULL

#add cols for Medical_History_35
# unique(train$Medical_History_35)
train$Medical_History_35_1 <- 0
train$Medical_History_35_2 <- 0
train$Medical_History_35_3 <- 0
train$Medical_History_35_1[train$Medical_History_35 == 1] <- 1
train$Medical_History_35_2[train$Medical_History_35 == 2] <- 1
train$Medical_History_35_3[train$Medical_History_35 == 3] <- 1
train$Medical_History_35 <- NULL


#add cols for Medical_History_36
train$Medical_History_36_1 <- 0
train$Medical_History_36_2 <- 0
train$Medical_History_36_3 <- 0
train$Medical_History_36_1[train$Medical_History_36 == 1] <- 1
train$Medical_History_36_2[train$Medical_History_36 == 2] <- 1
train$Medical_History_36_3[train$Medical_History_36 == 3] <- 1
train$Medical_History_36 <- NULL

#add cols for Medical_History_37
train$Medical_History_37_1 <- 0
train$Medical_History_37_2 <- 0
train$Medical_History_37_3 <- 0
train$Medical_History_37_1[train$Medical_History_37 == 1] <- 1
train$Medical_History_37_2[train$Medical_History_37 == 2] <- 1
train$Medical_History_37_3[train$Medical_History_37 == 3] <- 1
train$Medical_History_37 <- NULL

# Medical_History_38
train$Medical_History_38[train$Medical_History_38 == 1] <- 0
train$Medical_History_38[train$Medical_History_38 == 2] <- 1


#add cols for Medical_History_39
# unique(train$Medical_History_39)
train$Medical_History_39_1 <- 0
train$Medical_History_39_2 <- 0
train$Medical_History_39_3 <- 0
train$Medical_History_39_1[train$Medical_History_39 == 1] <- 1
train$Medical_History_39_2[train$Medical_History_39 == 2] <- 1
train$Medical_History_39_3[train$Medical_History_39 == 3] <- 1
train$Medical_History_39 <- NULL

#add cols for Medical_History_40
unique(train$Medical_History_40)
train$Medical_History_40_1 <- 0
train$Medical_History_40_2 <- 0
train$Medical_History_40_3 <- 0
train$Medical_History_40_1[train$Medical_History_40 == 1] <- 1
train$Medical_History_40_2[train$Medical_History_40 == 2] <- 1
train$Medical_History_40_3[train$Medical_History_40 == 3] <- 1
train$Medical_History_40 <-NULL


#add cols for Medical_History_41
train$Medical_History_41_1 <- 0
train$Medical_History_41_2 <- 0
train$Medical_History_41_3 <- 0
train$Medical_History_41_1[train$Medical_History_41 == 1] <- 1
train$Medical_History_41_2[train$Medical_History_41 == 2] <- 1
train$Medical_History_41_3[train$Medical_History_41 == 3] <- 1
train$Medical_History_41 <- NULL



#Replace the values with max values
hist(train$Employment_Info_1)
hist(train$Employment_Info_4)
hist(train$Employment_Info_6)
hist(train$Family_Hist_4)
hist(train$Medical_History_1)

sort(table(train$Medical_History_1))
sort(table(train$Employment_Info_6))


for(i in 1:ncol(train)){
  train[is.na(train[,i]), i] <- mean(train[,i], na.rm = TRUE)
}

summary(train)

names(train)
ncol(train)

write.csv(train, file = "iciciTrainData.csv", row.names = FALSE)


#################################Train Data Prep Completed######

index<-1:nrow(train)
testindex<-sample(index,trunc(length(index)*40/100))
testindex
dim(testindex)
testset<-train[testindex,]
testset[,2]
trainset<-train[-testindex,]
names(train)
testset

trainset
##model1<-lm(response~Id+Ins_Age+Ht+Wt+BMI+Medical_Keyword_1+Medical_Keyword_2+Medical_Keyword_3+Medical_Keyword_4+Medical_Keyword_5+Medical_Keyword_6+Medical_Keyword_7+Medical_Keyword_8+Medical_Keyword_9+Medical_Keyword_10+Medical_Keyword_11+Medical_Keyword_12+Medical_Keyword_13+Medical_Keyword_14+Medical_Keyword_15+Medical_Keyword_16+Medical_Keyword_17+Medical_Keyword_18+Medical_Keyword_19+Medical_Keyword_20+Medical_Keyword_21+Medical_Keyword_22+Medical_Keyword_23+Medical_Keyword_24+Medical_Keyword_25+Medical_Keyword_26+Medical_Keyword_27+Medical_Keyword_28+Medical_Keyword_29+Medical_Keyword_30+Medical_Keyword_31+Medical_Keyword_32+Medical_Keyword_33+Medical_Keyword_34+Medical_Keyword_35+Medical_Keyword_36+Medical_Keyword_37+Medical_Keyword_38+Medical_Keyword_39+Medical_Keyword_40+Medical_Keyword_41+Medical_Keyword_42+Medical_Keyword_43+Medical_Keyword_44+Medical_Keyword_45+Medical_Keyword_46+Medical_Keyword_47+Medical_Keyword_48+Product_Info_1_1+Product_Info_1_2+Product_Info_2_A1+Product_Info_2_A2+Product_Info_2_A3+Product_Info_2_A4+Product_Info_2_A5+Product_Info_2_A6+Product_Info_2_A7+Product_Info_2_A8+Product_Info_2_B1+Product_Info_2_B2+Product_Info_2_C1+Product_Info_2_C2+Product_Info_2_C3+Product_Info_2_C4+Product_Info_2_D1+Product_Info_2_D2+Product_Info_2_D3+Product_Info_2_D4+Product_Info_2_E1+Product_Info_4_C+Product_Info_5_2+Product_Info_5_3+Product_Info_6_1+Product_Info_6_3+Product_Info_7_1+Product_Info_7_2+Product_Info_7_3+Employment_Info_1_C+Employment_Info_3_1+Employment_Info_3_3+Employment_Info_4_C+Employment_Info_5_2+Employment_Info_5_3+Employment_Info_6_C+InsuredInfo_1_1+InsuredInfo_1_2+InsuredInfo_1_3+InsuredInfo_2_2+InsuredInfo_2_3+InsuredInfo_4_2+InsuredInfo_4_3+InsuredInfo_5_1+InsuredInfo_5_3+InsuredInfo_6_1+InsuredInfo_6_2+InsuredInfo_7_1+InsuredInfo_7_3+Insurance_History_1_1+Insurance_History_1_2+Insurance_History_2_1+Insurance_History_2_2+Insurance_History_2_3+Insurance_History_3_1+Insurance_History_3_2+Insurance_History_3_3+Insurance_History_4_1+Insurance_History_4_2+Insurance_History_4_3+Insurance_History_5_C+Insurance_History_7_1+Insurance_History_7_2+Insurance_History_7_3+Insurance_History_8_1+Insurance_History_8_2+Insurance_History_8_3+Insurance_History_9_1+Insurance_History_9_2+Insurance_History_9_3+Family_Hist_1_1+Family_Hist_1_2+Family_Hist_1_3+Family_Hist_2_C+Family_Hist_3_C+Family_Hist_4_C+Family_Hist_5_C+Medical_History_1_D+Medical_History_3_1+Medical_History_3_2+Medical_History_3_3+Medical_History_4_1+Medical_History_4_2+Medical_History_5_1+Medical_History_5_2+Medical_History_5_3+Medical_History_6_1+Medical_History_6_2+Medical_History_6_3+Medical_History_7_1+Medical_History_7_2+Medical_History_7_3+Medical_History_8_1+Medical_History_8_2+Medical_History_8_3+Medical_History_9_1+Medical_History_9_2+Medical_History_9_3+Medical_History_11_1+Medical_History_11_2+Medical_History_11_3+Medical_History_12_1+Medical_History_12_2+Medical_History_12_3+Medical_History_13_1+Medical_History_13_2+Medical_History_13_3+Medical_History_14_1+Medical_History_14_2+Medical_History_14_3+Medical_History_15_D+Medical_History_16_1+Medical_History_16_2+Medical_History_16_3+Medical_History_17_1+Medical_History_17_2+Medical_History_17_3+Medical_History_18_1+Medical_History_18_2+Medical_History_18_3+Medical_History_19_1+Medical_History_19_2+Medical_History_19_3+Medical_History_20_1+Medical_History_20_2+Medical_History_20_3+Medical_History_21_1+Medical_History_21_2+Medical_History_21_3+Medical_History_22_1+Medical_History_22_2+Medical_History_23_1+Medical_History_23_2+Medical_History_23_3+Medical_History_25_1+Medical_History_25_2+Medical_History_25_3+Medical_History_26_1+Medical_History_26_2+Medical_History_26_3+Medical_History_27_1+Medical_History_27_2+Medical_History_27_3+Medical_History_28_1+Medical_History_28_2+Medical_History_28_3+Medical_History_29_1+Medical_History_29_2+Medical_History_29_3+Medical_History_30_1+Medical_History_30_2+Medical_History_30_3+Medical_History_31_1+Medical_History_31_2+Medical_History_31_3+Medical_History_33_1+Medical_History_33_3+Medical_History_34_1+Medical_History_34_2+Medical_History_34_3+Medical_History_35_1+Medical_History_35_2+Medical_History_35_3+Medical_History_36_1+Medical_History_36_2+Medical_History_36_3+Medical_History_37_1+Medical_History_37_2+Medical_History_37_3+Medical_History_38_1+Medical_History_38_2+Medical_History_39_1+Medical_History_39_2+Medical_History_39_3+Medical_History_40_1+Medical_History_40_2+Medical_History_40_3+Medical_History_41_1+Medical_History_41_2+Medical_History_41_3)
model1<-lm(trainset$Response~.,trainset)
summary(model1)

trainset$Id<-NULL
trainset$Employment_Info_1<-NULL
trainset$Employment_Info_4<-NULL
trainset$Employment_Info_6<-NULL
trainset$Family_Hist_4<-NULL
trainset$Insurance_History_1 <-NULL
trainset$Insurance_History_5<-NULL
trainset$InsuredInfo_4<-NULL
trainset$Medical_History_5<-NULL
trainset$Medical_History_33<-NULL
trainset$Medical_History_38<-NULL
trainset$Product_Info_1<-NULL
trainset$Product_Info_5<-NULL
trainset$Product_Info_6<-NULL
trainset$BMI<-NULL

model2<-lm(trainset$Response~ Product_Info_4+Ins_Age+Ht+Wt+Employment_Info_3+BMI+
             Employment_Info_5+InsuredInfo_2+InsuredInfo_5+InsuredInfo_6+InsuredInfo_7+
             Family_Hist_4+Medical_History_1+Medical_History_4+Medical_History_22+
             Medical_Keyword_2+Medical_Keyword_3+Medical_Keyword_6+Medical_Keyword_9+
             Medical_Keyword_15+Medical_Keyword_19+Medical_Keyword_25+Medical_Keyword_33+
             Medical_History_39_1+Medical_Keyword_41+Medical_Keyword_45+
             Insurance_History_2_1+Medical_History_7.1+Medical_History_11.2+Medical_History_12.2+
             Medical_History_20.1+Medical_History_17.2+Medical_History_20.1+Medical_History_5+
             Medical_History_23_1+Medical_History_27_1+Medical_History_29_1+
             Medical_History_30_2+Medical_History_31_1+Medical_History_35_1+
             Medical_History_39_1+Medical_History_40_1
             ,trainset)
summary(model2)

model3<-lm(trainset$Response~ Product_Info_4+Ins_Age+Ht+Wt+Employment_Info_1+BMI+
             Employment_Info_6+InsuredInfo_2+InsuredInfo_5+InsuredInfo_6+
             Family_Hist_4+Medical_History_4+Insurance_History_2_1+
             Medical_History_39_1+Medical_History_17.2+Medical_History_20.1+
             Medical_History_30_2+Medical_History_23_1+
             Medical_History_40_1
           ,trainset)
summary(model3)

model1<-lm(Response~ Product_Info_4+Ins_Age+Ht+Wt+BMI
             +InsuredInfo_2+InsuredInfo_5+InsuredInfo_6+
             Family_Hist_4+Medical_History_4+Insurance_History_2_1+
             Medical_History_39_1+Medical_History_17.2+Medical_History_20.1+
             Medical_History_30_2+Medical_History_23_1+
             Medical_History_40_1
           ,trainset)
summary(model1)

predictionWithTest<-predict(model1, testset, se.fit = FALSE,
                            scale = NULL, df = Inf,interval = c("none", "confidence", "prediction"),
                            level = 0.95, type = c("response", "terms"),terms = NULL, 
                            na.action = na.pass,pred.var = res.var/weights, weights = 1)
summary(predictionWithTest)

output<-round(predictionWithTest)
actualResponse<-testset$Response
mean(testset$Response-output)^2
str(testset)
View(testset)

abline(lm(trainset$Response~ Product_Info_4+Ins_Age+Ht+Wt+BMI
          +InsuredInfo_2+InsuredInfo_5+InsuredInfo_6+
            Family_Hist_4+Medical_History_4+Insurance_History_2_1+
            Medical_History_39_1+Medical_History_17.2+Medical_History_20.1+
            Medical_History_30_2+Medical_History_23_1+
            Medical_History_40_1,trainset))



anova(model1) 

library(MASS)
library(rpart.plot)
anova(model2, model1)
step <- stepAIC(model2, direction="both")
step$anova # display results




plot(model1)
text(model1)


predictionWithTest<-predict(model1, test, se.fit = FALSE,
                            scale = NULL, df = Inf,interval = c("none", "confidence", "prediction"),
                            level = 0.95, type = c("response", "terms"),terms = NULL, 
                            na.action = na.pass,pred.var = res.var/weights, weights = 1)

submit <- data.frame(Id = test$Id, Response = round(predictionWithTest))
write.csv(submit, file = "myfirstprediction.csv", row.names = FALSE)
