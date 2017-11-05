install.packages("caret")
install.packages("pbkrtest")
library(caret)
library(ResourceSelection)
library(pbapply)
library(survey)
library(data.table)
library(InformationValue)


churndata$State=NULL
churndata$`Area Code`=NULL
churndata$Phone=NULL
na.omit(churndata)
summary(churndata)
set.seed(1234)
inTrain=createDataPartition(churndata$Churn,p=0.8,list=FALSE)
Training=churndata[inTrain,]
Testing=churndata[-inTrain,]
fit0=glm(Churn ~.,data = Training,family = binomial(link = "logit") )
summary(fit0)
library(MASS)
install.packages("e1071")
library(e1071)
step= stepAIC(fit0,direction = "both")

fit1= glm(Churn ~ `Account Length` + `VMail Message` + `Day Mins` + `Eve Mins` + 
  `CustServ Calls` + `Int'l Plan` + `VMail Plan` + `Night Charge` + 
  `Intl Calls` + `Intl Charge`,data = Training,family = binomial(link = "logit") )
summary(fit1)

fit2= update(fit1,.~.-`Account Length`,data = Training)
summary(fit2)

pred=predict(fit2,newdata = Training[,-1],type = "response" )
View(pred)
pred1=ifelse(pred<0.5,0,1)
View(pred1)
table(Training$Churn,pred1,dnn = list('Actual','Predicted'))
confusionMatrix(table(Training$Churn,pred1,dnn = list('Actual','Predicted')))


testpred=predict(fit2,newdata = Testing[,-1],type = "response" )
View(testpred)
testpred1=ifelse(testpred<0.5,0,1)
View(testpred1)
confusionMatrix(table(Testing$Churn,testpred1,dnn = list('Actual','Predicted')))



plotROC(actuals = Training$Churn,predictedScores = as.numeric(fitted(fit2)))
ks_plot(actuals = Training$Churn,predictedScores = as.numeric(fitted(fit2)))

install.packages("Rserve")
library(Rserve)
Rserve()

