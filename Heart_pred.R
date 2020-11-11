dataset_heart = read.csv(file = "/Users/jyoticaa/Desktop/heart_failure_clinical_records_dataset.csv")

dim(dataset_heart)
names(dataset_heart)

head(dataset_heart)
tail(dataset_heart)

sum(is.na(dataset_heart))
#it says we have 0 null values

#Here our target column is DEATH_EVENT
#we have boolean values in our target variable
#checking their balance
x = table(dataset_heart$DEATH_EVENT)
x
# we can see we have 203 entries as people who havent died and 1 is people who died within the time period
  



#Now, visualizing the data
#tableau

library(ggplot2)
library(corrplot)
library(tidyverse)


correlation <- cor(dataset_heart[,c(1,3,5,7:9,12)])
corrplot(correlation, method="circle")
#there aew no 2 variables which are highly correlated 

#splitting the training and testing sets
library(caret)
nrows <- NROW(dataset_heart)
set.seed(11)                           
index <- sample(1:nrows, 0.7 * nrows)  


train <- dataset_heart[index,]                  
test <- dataset_heart[-index, ]


prop.table(table(train$DEATH_EVENT))
prop.table(table(test$DEATH_EVENT))



library(dplyr)
#step wise regression model
null_model<-glm(DEATH_EVENT~1,data=train,family='binomial')
full_model<-glm(DEATH_EVENT~.,data=train,family='binomial')

step_model <- step(null_model, 
                   scope = list(lower = null_model,
                                upper = full_model),
                   direction = "forward")


summary(step_model)

#Equation 
#model =  11.485818  -0.019278(time) -0.079240 (ejection_fraction) +  0.055899 (age) -0.082430(serum_sodium) + 
#0.484291(serum_creatinine)


test$pred<-predict(step_model,test,type='response')
test$death_pred<-ifelse(test$pred>=0.5,1,0)
table(test$death_pred,test$DEATH_EVENT)

library(pROC)
plot(roc(test$DEATH_EVENT,test$death_pred),col='red')

AUC  = auc(roc(test$DEATH_EVENT,test$death_pred))
AUC




