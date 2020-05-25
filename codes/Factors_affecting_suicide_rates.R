
#########linear regression
suicide_data<- read.csv("data_mining/suicide_rates_overview.csv",stringsAsFactors = FALSE)
head(suicide_data)
clean_data<-na.omit(suicide_data)
clean_data
names(clean_data)

suicide_data$sex <- factor(suicide_data$sex, levels = c("M", "F"))
suicide_data$age<-as.factor(suicide_data$age)
suicide_data$generation<-as.factor(suicide_data$generation)


#alias(clean_data)

#predicting the factors affecting suicide rates

#splitting the data into train and test
set.seed(2)
library(caTools)
part<-sample.split(clean_data,SplitRatio = 0.7)
train_suicide<-subset(clean_data,part=="TRUE")
test_suicide<-subset(clean_data,part=="FALSE")
suicide_data$gdp_for_year

#CREATE THE MODEL
lm_model<-lm(suicides_no~sex+age+generation,data=train_suicide)#training the model
summary(lm_model)

#prediction
predict_suicide<-predict(lm_model,test_suicide)
predict_suicide
#finding accuracy ff the model
RMSE_val<-sqrt(mean(predict_suicide-clean_data$suicides_no)^2)
RMSE_val


















