## clear the working directory
rm(list = ls())
## to check the data file is in the working directory
dir()

## reading the data set
customer_data<- read.csv("CustomerData.csv",header = T)
## checking for null values 
sum(is.na(customer_data))

summary(customer_data)
str(customer_data)
## data cleaning 
customer_data<-customer_data[-1]
## type conversion
customer_data$City<- as.factor(customer_data$City)
str(customer_data)

## splitiing numerical and categorical data
customer_data1<- customer_data[-c(1,11,12)]
str(customer_data1)

### checking for the attribute which has the highest influence on the target
## we use the corr plot
library(corrplot)
par(mfrow=c(1,1))
corrplot::corrplot(cor(customer_data1),method = 'number')

## the factor that has highest influence on totalrevenue generated is no.of units purchased
## now we build the linear model
modlm<- lm(TotalRevenueGenerated~NoOfUnitsPurchased,data=customer_data1)
summary(modlm)

##plotting of residual plots
par(mfrow=c(2,2))
plot(modlm)
customer_data1$resid<- modlm$residuals
customer_data1$lev <- hatvalues(modlm)
customer_data1$cooksD<-cooks.distance(modlm)
customer_data1[which.max(customer_data1$cooksD),]
customer_data1[which.max(customer_data1$resid),]
## removing the max cooksD point
customer_data2<- customer_data1[-c(1489,974,667,1764,1329,2146),]
modlm2<-lm(TotalRevenueGenerated~NoOfUnitsPurchased,data=customer_data2)
summary(modlm2)
plot(modlm2)
shapiro.test(modlm2$residuals)
## to find the errors in the data

predictions = predict(modlm2,newdata=customer_data2)
library(DMwR)
regr.eval(customer_data2$TotalRevenueGenerated,predictions)


### instead of split the data into train and test
