####predicting medical appointment no shows
data<- read.csv("data_mining/no_show_appointments_data.csv")

str(data)
head(data)
summary(data)

###converting to factors
data$Gender <- factor(data$Gender, levels = c("M", "F"))
data$Scholarship<-as.factor(data$Scholarship)
data$Hipertension<-as.factor(data$Hipertension)
data$Diabetes<-as.factor(data$Diabetes)
data$Alcoholism<-as.factor(data$Alcoholism)
data$Handcap<-as.factor(data$Handcap)
data$No.show<-as.factor(data$No.show)
str(data)



####removing unwanted columns
names(data)
new<-data[1]
names(new)
ncol(data)
new_data<-data[c(T,T,T,F,F,T,F,T,T,T,T,T,T,T)]
names(new_data)


#partitioning data

set.seed(123)
ind <- sample(2,nrow(new_data),replace=TRUE,prob=c(0.8,0.2))
train <-new_data[ind==1,]
test <-new_data[ind==2,]

###performing random forest###
str(data)
library(randomForest)
set.seed(222)

#training the data
rf <- randomForest(No.show~.,data=train)
print(rf)
rf$confusion
#prediction and confusion matrix
library(caret)
p1<-predict(rf,train)
p1# to view predicted  values
head(train$No.show)#to view actual values
confusionMatrix(p1,train$No.show)

#prediction with test data
p2<-predict(rf,test)
confusionMatrix(p2,test$No.show)

#error-rate
library(ggplot2)
plot(rf)
#tuning the model
#t<-tuneRF(train[,100],train[,200],
       #stepFactor = 1,
       #plot = TRUE,
       #ntreeTry = 300,
       #trace = TRUE,
       #improve = 0.05)
#no of nodes for trees
#no of nodes for the trees
hist(treesize(rf),main="no of nodes for the trees",col = "green")

#variable importance
varImpPlot(rf)
varUsed(rf)#to show the number of times a variable is used
 
#partial dependence plot
partialPlot(rf,train,Alcoholism,"No")

#extract single tree
getTree(rf,1,labelVar = TRUE)#information about first tree

#mdsplot-error ,to be used in case
MDSplot(rf,train$No.show)

################################################################################
##logisticregression
library(caTools)#explore the package
set.seed(123)
split<-sample.split(new_data,SplitRatio = 0.8)
split
training<-subset(new_data,split== "TRUE")
testing<-subset(new_data,split== "FALSE")


model<-glm(No.show ~.,training,family = "binomial")
summary(model)

#optimising the model by removing  variables which are not statistically significant

#optiminsing model by removing gender
model<-glm(No.show ~.-Gender,training,family = "binomial")
summary(model) #shows high residual deviance and low aic but only 1 point below 

#optimising model by removing hypertension
model<-glm(No.show ~.-Hipertension,training,family = "binomial")
summary(model) #shows same residual deviance and low aic 


#optimising model by removing handicap
model<-glm(No.show ~.-Handcap,training,family = "binomial")
summary(model) #shows high residual deviance and low aic

#since hypertension shows same resid deviance and low aic-removing hypertension

model<-glm(No.show ~.-Hipertension,training,family = "binomial")
summary(model)


#predict the values of the test dataset  and categorise them according to a threshold which is 0.5

res<-predict(model,testing,type = "response") #response means we have to get a probability from model
table(Actualvalue)
print(res)#gives the probabilities of  "no shows" in testing dataset


print(testing)#check if the mmodel prediction is right

#creating a confusion matrix to test for accuracy with a threshold value of 0.5
(table(ActualValue=testing$No.show,PredictedValue=res>0.5))
(23953+67)/(23953+67+115+6008)##accuracy of the model is 79.6

#finding out the exact threshold value using roc curve
res<-predict(model,training,type = "response") #changing the roc value to training
library(ROCR)

#defining rocr prediction and performance
ROCRPred<-prediction(res,training$No.show)
ROCRPref<-performance(ROCRPred,"tpr","fpr")
plot(ROCRPref,colorize=TRUE,print.cutoffs.at=seq(0.1,by=0.1)) #analyse the graph 
#analysing that 0.3/0.2 has better TP rate as compared to 0.5 from the roc , changing threshold value to 0.3

#comparing the thresholds 0.5 vs 0.3 for testing
(table(ActualValue=testing$No.show,PredictedValue=res>0.5))
(table(ActualValue=testing$No.show,PredictedValue=res>0.3))#false positive has decreased
(table(ActualValue=testing$No.show,PredictedValue=res>0.2))#checking with 0.2 threshold value

#checking for accuracy
(1249+21573)/(2495+4826+21573+1249) #accuracy reduced to 75.71 for threshold of 0.3
(3509+15386)/(15386+3509+8682+2566) #accuracy further decreased to 62.68 

#0.5 has high accuracy and less true negative rate so that 115 people will show off and model predicts that they wont




































