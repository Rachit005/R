getwd()
setwd("D:/RAC_IMARTICUS/R")

Data = read.csv("Regression.csv")

summary(Data)
hist(Data$Age)

#finding NA value in AGE column
sum(is.na(Data$Age))


#replacing it with median
Data$Age[is.na(Data$Age)] = 38

#display 1st six data
head(Data)

#converting Job.Type to numeric data formula = n-1
Data$Job.Type_employed = as.numeric(Data$Job.Type=="Employed")
Data$Job.Type_unemployed = as.numeric(Data$Job.Type=="Unemployed")
Data$Job.Type_retired = as.numeric(Data$Job.Type=="Retired")

#converting Marital.Status to numeric data formula = n-1
Data$Marital.Status_yes = as.numeric(Data$Marital.Status=="Yes")

#converting Education to numeric data formula = n-1
Data$Education_graduate = as.numeric(Data$Education=="Graduate")
Data$Education_secondary = as.numeric(Data$Education=="Secondry")

#converting Metro.City to numeric data formula = n-1
Data$Metro.City_yes = as.numeric(Data$Metro.City=="Yes")

head(Data)

#removing the columns which are converted to numeric
final_data = Data[-c(2,3,4,5)]


head(final_data)

#Include randomness
set.seed(56952)

#Box plot

#help("par")
par(mfrow=c(1,2))
bx = boxplot(final_data$Age)

#checking for distribution of AGE
quantile(final_data$Age ,seq(0,1,0.01))

#replacing the outlier with 98% value
final_data$Age = ifelse(final_data$Age>60,57,final_data$Age)
boxplot(final_data$Age)


#finding outliers in column Signed.in.since.Days.
boxplot(final_data$Signed.in.since.Days.)

#checking for distribution of Signed.in.since.Days.
quantile(final_data$Signed.in.since.Days., seq(0,1,0.01))

#replacing the outlier
final_data$Signed.in.since.Days. = ifelse(final_data$Signed.in.since.Days.<45,48,final_data$Signed.in.since.Days.)
boxplot(final_data$Signed.in.since.Days.)

par(mfrow=c(1,2))

hist(final_data$Purchase.made, main='Dependent')
boxplot(final_data$Purchase.made)


#help("par")
#install.packages("car")

library(car)

scatterplot(final_data$Age,final_data$Purchase.made)

cor(final_data)

t1=sample(1:nrow(final_data),0.8*nrow(final_data))
t_train1=final_data[t1,]
t_test1=final_data[-t1,]



#checking multicolinearity
d1 = lm(Purchase.made~.,data = t_train1)
summary(d1)
vif(d1)

d2 = lm(Purchase.made~ Age + Signed.in.since.Days. + Marital.Status_yes + Metro.City_yes, data = t_train1)
summary(d2)
vif(d2)

d3 = lm(Purchase.made~Signed.in.since.Days. + Marital.Status_yes + Metro.City_yes, data = t_train1)
summary(d3)
vif(d3)


head(t_test1)

a = data.frame(Signed.in.since.Days. = 48 , Marital.Status_yes = 0,Metro.City_yes = 0)
re = predict(d3,a)
re

prediction = predict(d3,t_test1)
head(prediction)

prediction
output = cbind(t_test1,prediction)
final_output = output[-c(1,2,4:10)]
head(final_output)

#install.packages("DMwR")
library(DMwR)
regr.eval(final_output$Purchase.made,final_output$prediction)
AIC(d3)
cor(final_output$Purchase.made,final_output$prediction)

min_max_accuracy <- mean(apply(final_output, 1, min) / apply(final_output, 1, max))  
min_max_accuracy

mape <- mean(abs((final_output$prediction - final_output$Purchase.made))/final_output$Purchase.made)  
mape

durbinWatsonTest(d3)

plot(final_output$Purchase.made, residuals(d3))


