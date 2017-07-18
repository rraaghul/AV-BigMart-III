setwd("C:/Users/RR/Desktop/Big Mart")

library(plyr)
library(e1071)
library(outliers)
library(data.table)
library(car)

####Import Data
Mart_Train<- data.table(read.csv("train.csv",header =TRUE))
Mart_Test <- data.table(read.csv("test.csv",header =TRUE))

Transform_values<-function(A)
{
  ##weight
  wgt<-A[, max(Item_Weight,na.rm = TRUE), by = "Item_Identifier"]
  is.na(wgt) <- sapply(wgt, is.infinite)
  A<-join(A,wgt,by="Item_Identifier")
  A$Item_Weight=A$V1
  A$V1<-NULL
  A$Item_Identifier = NULL
  A$Outlet_Identifier =NULL
  ##Fat Content
  
 A$Item_Fat_Content=recode(A$Item_Fat_Content,"c('LF', 'low fat','Low Fat')='LF';c('reg', 'Regular') = 'Reg'")
 A$Item_Fat_Content<-as.numeric(A$Item_Fat_Content)
 A$Item_Type<-as.numeric(A$Item_Type)
 A$Outlet_Size[A$Outlet_Size ==""]=NA
 A$Outlet_Size = as.numeric(A$Outlet_Size)-1
 A$Outlet_Location_Type = as.numeric(A$Outlet_Location_Type)
 A$Outlet_Type = as.numeric(A$Outlet_Type)

 
 require(mice)
 set.seed(145)
 A=complete(mice(A))
 A$Item_Type = NULL
 A$Item_Weight=NULL

  A
}

Sales=Mart_Train$Item_Outlet_Sales
Item_Identifier = Mart_Test$Item_Identifier
Outlet_Identifier = Mart_Test$Outlet_Identifier

Mart_Train$Item_Outlet_Sales<-NULL
Mart_Train$Dt<-0
Mart_Test$Dt<-1

Comb<-rbind(Mart_Train,Mart_Test)

Comb<-Transform_values(Comb)

Train<-subset(Comb,Comb$Dt==0)
Test<-subset(Comb,Comb$Dt==1)

Train$Dt<-NULL
Test$Dt<-NULL
Train$Item_Outlet_Sales<-Sales


#Train$Item_Outlet_Sales<-NULL

Model_linear<-lm(Train$Item_Outlet_Sales~.,data=Train)

summary(Model_linear)
#Predict Output
Output_lr= predict(Model_linear,Test)


Mysubmission = data.frame(Item_Identifier = Item_Identifier, Outlet_Identifier = Outlet_Identifier, Item_Outlet_Sales = Output_lr)
write.csv(Mysubmission,"Submission_linear.csv", row.names = FALSE)


require(rpart)
require(rpart.plot)
tree = rpart(Item_Outlet_Sales ~., data= Train,minbucket=5,cp=.01)
prp(tree)

pred_tree=predict(tree,newdata = Test)
Mysubmission = data.frame(Item_Identifier = Item_Identifier, Outlet_Identifier = Outlet_Identifier, Item_Outlet_Sales = pred_tree)
write.csv(Mysubmission,"Submission_tree.csv", row.names = FALSE)



svm=svm(Item_Outlet_Sales~.,data=Train,kernel="radial")
pred_svm=predict(svm,newdata=Test)
Mysubmission = data.frame(Item_Identifier = Item_Identifier, Outlet_Identifier = Outlet_Identifier, Item_Outlet_Sales = pred_svm)
write.csv(Mysubmission,"Submission_svm.csv", row.names = FALSE)


require(randomForest)
forest=randomForest(Item_Outlet_Sales~.,data=Train,ntree=100,nodesize=21,cp=.39)
pred_rf=predict(forest,newdata = Test)
Mysubmission = data.frame(Item_Identifier = Item_Identifier, Outlet_Identifier = Outlet_Identifier, Item_Outlet_Sales = pred_rf)
write.csv(Mysubmission,"Submission_rf.csv", row.names = FALSE)
