#Script by : Rachit Agarwal
#Dataset taken from UCI (https://archive.ics.uci.edu/) : Combined Cycle Power Plant

#The following script is used to build a Linear Regression Model which can be used to predict the Energy Output[PE] of the Power Plant with the help of the given data



#Step 1 : Loading Data
#Reading Data 
data =  read.csv("PowerPlant.csv")

#View Data
View(data)

#Print First 6 Records
head(data)

#Check Summary
summary(data)

#Step 2 : Data Cleaning
#Checking Outliers
bx = boxplot(data$AT) #No Outliers Found
bx = boxplot(data$V)  #No Outliers Found

bx = boxplot(data$AP) #Outliers Exist 

#Removing Outliers :
quantile(data$AP,seq(0,1,0.01))

data$AP <- ifelse(data$AP<1000,1000,data$AP) #1% data loss
data$AP <- ifelse(data$AP>1028,1028,data$AP) #1% data loss

bx = boxplot(data$AP) #Outliers Removed
bx = boxplot(data$RH) #Outliers Exist

#Removing Outliers :
quantile(data$RH,seq(0,1,0.01))

data$RH = ifelse(data$RH<38,38,data$RH) #1% Data Loss
bx = boxplot(data$RH) #Outliers Removed

#Step 3 : Training And Testing Data
t1=sample(1:nrow(data),0.8*nrow(data))
t_train1=data[t1,]
t_test1=data[-t1,]

#Step 4 : Creating Model
library(car)
mod1 <- lm(PE~.,data=t_train1)
summary(mod1)
vif(mod1)

#The vif value of AT is greater than 5 so another model ignoring AT can be created :

mod2 <- lm(PE~V+AP+RH,data=t_train1)
summary(mod2)
vif(mod2)

#Step 5 : Predictions
#Based on Model 1

prediction = predict(mod1,t_test1)
head(prediction)


#To compare predictions with test data :
output = cbind(t_test1,prediction)
head(output)
final_output = output[-c(1:4)]
head(final_output)
#plot
plot(final_output$PE,final_output$prediction)

#Based on Model 2

prediction = predict(mod2,t_test1)

head(prediction)

#To compare predictions with test data :
output = cbind(t_test1,prediction)
head(output)
final_output = output[-c(1:4)]
head(final_output)
#Plot
plot(final_output$PE,final_output$prediction)

#To check Regression Evaluation
#install.packages("DMwR")
library(DMwR)
regr.eval(final_output$PE,final_output$prediction) #Run the model script first for which eval is to be printed

##############################################################################################################