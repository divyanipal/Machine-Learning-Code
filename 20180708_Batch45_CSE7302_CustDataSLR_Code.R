#####Linear Regression Activity-Session#####
Fdata<-read.csv("CustomerData.csv",header=T)

#Data Understanding
names(Fdata)
str(Fdata)
#Keeping ID aside
Fdata<-Fdata[,-1]

#Are there any missing values in the data
sum(is.na(Fdata))

#Type conversion
Fdata$City<-as.factor(Fdata$City)

#To see which attributes have high influence on target- Total revenue generated
#We use correlations plot
library(corrplot)

Fdata2<-Fdata[,-c(1,11:12)] #Keeping aside factor variables
corrplot::corrplot(cor(Fdata2),method="number") #This is wrt to all attributes

#No. of units purchased has high correlation. So lets consider it for predicting revenue




mod<-lm(TotalRevenueGenerated~NoOfUnitsPurchased,data=Fdata2)
par(mfrow=c(2,2))
plot(mod)
#Report your observations

predictions=predict(mod,newdata=Fdata2)
library(DMwR)
regr.eval(Fdata2$TotalRevenueGenerated,predictions) #There is 22% error in predictions
#Instead study train and validation split
library(caret)
y<-createDataPartition(y=Fdata2$TotalRevenueGenerated,
                     times=1,list=F,p=0.7)
train<-Fdata2[y,]
valid<-Fdata2[-y,]
mod<-lm(TotalRevenueGenerated~NoOfUnitsPurchased,data=train)
predictions_valid=predict(mod,newdata = valid)
regr.eval(valid$TotalRevenueGenerated,predictions_valid)
test.data <-read.csv("Eval.csv", header=T)
#### predictions on test data 
regr.eval(test.data$TotalRevenueGenerated, predict(mod,newdata=test.data))

#Try doing a log transformation of Target and check

