getwd()
setwd("D:/RAC_IMARTICUS/R")

data = read.csv("weatherHistory.csv")

head(data)

data = data[-c(1,7:12)]
summary(data)

data$P_type_rain = as.numeric(data$Precip.Type=="rain")
data$P_type_snow = as.numeric(data$Precip.Type=="snow")

data$sum_pc = as.numeric(data$Summary=="Partly Cloudy")
data$sum_mc = as.numeric(data$Summary=="Mostly Cloudy")
data$sum_overcast = as.numeric(data$Summary=="Overcast")
data$sum_clear = as.numeric(data$Summary=="Clear")
data$sum_foggy = as.numeric(data$Summary=="Foggy")
data$sum_bo = as.numeric(data$Summary=="Breezy and Overcast")

head(data)
data = data[-c(1,2)]


set.seed(200)

par(mfrow=c(1,2))
boxplot(data$Apparent.Temperature..C.)
quantile(data$Apparent.Temperature..C. ,seq(0,1,0.01))
data$Apparent.Temperature..C. = ifelse(data$Apparent.Temperature..C.< -12,-11,data$Apparent.Temperature..C.)
boxplot(data$Apparent.Temperature..C.)

par(mfrow=c(1,2))
boxplot(data$Humidity)
quantile(data$Humidity,seq(0,1,0.01))
data$Humidity = ifelse(data$Humidity< 0.3,0.3,data$Humidity)
boxplot(data$Humidity)

cor(data)

t1=sample(1:nrow(data),0.8*nrow(data))
t_train1=data[t1,]
t_test1=data[-t1,]

library(car)
d1 = lm(Temperature..C.~.,data=t_train1)
summary(d1)
vif(d1)

d2 = lm(Temperature..C.~ Apparent.Temperature..C. + Humidity + sum_bo , data=t_train1)
summary(d2)
vif(d2)

prediction = predict(d2,t_test1)
head(prediction)

prediction
output = cbind(t_test1,prediction)
head(output)
final_output = output[-c(2:11)]
head(final_output)

#install.packages("DMwR")
library(DMwR)
regr.eval(final_output$Temperature..C.,final_output$prediction)
AIC(d2)
cor(final_output$Temperature..C.,final_output$prediction)

