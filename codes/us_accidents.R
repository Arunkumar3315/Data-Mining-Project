####KNN Algorithm
accident_data<-read.csv("data_mining/US_Accidents_Dec19.csv",nrows = 10000)
head(accident_data)
str(accident_data)
names(accident_data)

#cleaning and preprocessing the data for taking only useful variables for analysis
#severity being the target variable
#model will predict severity based on the other 2 factors
accident_subset<-accident_data[c('Humidity...','Visibility.mi.','Severity')]
names(accident_subset)#we have selected only useful variables needed for our analysis
str(accident_subset)

any(is.na(accident_subset))#checking if there is any na values in data
sum(is.na(accident_subset))#printing sum
str(accident_subset)
sum(is.na(accident_subset$Humidity...))
sum(is.na(accident_subset$Visibility.mi.))
colSums(is.na(accident_subset))#humidity and visibility has missing cases
cleaned_data<-na.omit(accident_subset)
colSums(is.na(cleaned_data))# checking if there is any na values in cleaned dara
str(cleaned_data)



#Normalise the data so that there is no biasness in the data
#normalize <- function(x){
  #return((x-min(x))/max(x)-min(x))}

#cleaned_data.n<-as.data.frame(lapply(cleaned_data[,2:5],normalize))
#head(cleaned_data.n)

#partitioning the data
set.seed(123)
part<-sample(1:nrow(cleaned_data),size = nrow(cleaned_data)*0.7,replace = FALSE)
train_accident<-cleaned_data[part,] #70% for training
test_accident<-cleaned_data[-part,]#30% for testing

#creating seperate dataframes for 'severity' which is our target variable for testing and training dataset
train_accident.labels<-cleaned_data[part,3]
test_accident.labels<-cleaned_data[-part,3]
print(test_accident.labels)

#training the model on training data
#install.packages('class')
library(class)

NROW(train_accident.labels) #to find number of observations in training data


#to find the optimum value of k, the square root of total number of observations is taken ie 6887
#square root of 6887 is ~83

knn.83<-knn(train_accident,test = test_accident, cl=train_accident.labels,k=83)
knn.84<-knn(train_accident,test = test_accident, cl=train_accident.labels,k=84)

#evaluating accuracy of the model as to which model gives the best accuracy
#calculating the correct proportion of classification for k=83 and 84

Accuracy.83<-100*sum(test_accident.labels==knn.83)/NROW(test_accident.labels) #for knn=83 accuracy is 87.46612
Accuracy.84<-100*sum(test_accident.labels==knn.84)/NROW(test_accident.labels) #for knn=84 accuracy is 87.19512

print(Accuracy.83)
print(Accuracy.84)
                  #model 83 is more accurate with an accuracy value of 87.46612


table(knn.83,test_accident.labels) #to check prediction against actual values 
                                   #
knn.83 #predictions of severity of accident based on humidity and visibility for k=83

table(knn.84,test_accident.labels) #to check prediction against actual values 
knn.84 #predictions for k=84

#calculating accuracy using confusion matrix

#library(caret)
#confusionMatrix(table(knn.83,test_accident.labels)) #confusion matrix not loading due to unequal values

#improving the performance of the model using elbow method
i=1
k.optm=1
for(i in 1:83){
  knn.mod<- knn(train = train_accident, test = test_accident, cl=train_accident.labels, k=i)
  k.optm[i]<-100*sum(test_accident.labels==knn.mod)/NROW(test_accident.labels)
  k=i
  cat(k,'=',k.optm[i],'\n')   # to print % accuracy
}
plot(k.optm,type = 'b',xlab = 'k-value',ylab = 'accuracy_level') #plot accuracy wrt to k value

# from elbow method , we have found out that maximum accuracy is at k=1

knn.1<-knn(train_accident,test = test_accident, cl=train_accident.labels,k=1)
Accuracy.1<-100*sum(test_accident.labels==knn.1)/NROW(test_accident.labels) #for knn=1 accuracy is 99
print(Accuracy.1)





#############decision trees###########

train_accident #already partitioned training data
test_accident #already partitioned testing data
str(accident_data)
accident_data$Severity<-as.factor(accident_data$Severity)
accident_data$Humidity...<-as.integer(accident_data$Humidity...)
accident_data$Visibility.mi.<-as.integer(accident_data$Visibility.mi.)
accident_data$Wind_Speed.mph.<-as.integer(accident_data$Wind_Speed.mph.)

set.seed(2)
library(caTools)
part<-sample.split(accident_data,SplitRatio = 0.7)
train_data<-subset(accident_data,part=="TRUE")
test_data<-subset(accident_data,part=="FALSE")

#decision tree

library(party)
Dtree<-ctree(Severity~Humidity...+Visibility.mi.+Wind_Speed.mph.,data = train_data)
Dtree
plot(Dtree)

predict(Dtree,test_data) #prediction

#misclassificationerror for train data

table<-table(predict(Dtree),train_data$Severity)
print(table)    
1-sum(diag(table))/sum(table)  #0.397

#misclassification error for test data
test_pred<-predict(Dtree,newdata=test_data)
table2<-table(test_pred,test_data$Severity)
print(table2)
1-sum(diag(table2))/sum(table2) #0.389













  


