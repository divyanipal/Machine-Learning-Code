##################### Simple Linear Regression
#### removinng the variables in the working directory
rm(list=ls())
#### setting the working directory
dir()


### reading the file
data1<- read.csv("Data_Regression.csv",header=T)
summary(data1)

### visualization of a data
View(data1)
plot(data1$X,data1$Y)
### checking the correlation
cor(data1)

### building the linear regression model
modlm<- lm(Y~X,data=data1)
summary(modlm)
modlm

### plotting of residual plots
par(mfrow=c(2,2))
plot(modlm)
### assigning the modlm values in the dataset
data1$resid<- modlm$residuals
data1$lev <-hatvalues(modlm)
data1$cooksD<-cooks.distance(modlm)
### To Check the Max values in the rows of a specific value
data1[which.max(data1$resid),]
data1[which.max(data1$lev),]
data1[which.max(data1$cooksD),]

### removing the points which has the high leverage and cooks distance
data2<- data1[-c(10,33),]

## again rebulid the linear regression model to check the  significance
modlm2<- lm(Y~X,data = data2)
summary(modlm2)

### plotting the residual plots
plot(modlm2)
shapiro.test(modlm2$residuals)
data2[which.max(data2$cooksD),]



### find the values of MSE,RMSE,MAE,MAPE
library(DMwR)
options("scipen"=100) ## used for better viewing of the values 
regr.eval(data2$Y,modlm2$fitted.values)
