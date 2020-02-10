#Script by : Rachit Agarwal
#Dataset taken from UCI (https://archive.ics.uci.edu/) : direct marketing campaigns of a Portuguese banking institution

#The data is related with direct marketing campaigns of a Portuguese banking institution
#The following script is used to build a Logistic Regression Model which can be used to predict the result of the direct marketing campaigns with the help of the given data


setwd("D:/RAC_IMARTICUS/R/Logistic Regression/Bank Data")

#reading the csv file
d = read.csv("bank-full.csv")

#removing the columns not required
d = d[-c(9:12,14,15)]

#checking for missing values
summary(d)


#checking for datatypes
str(d)

#checking for outliers in age column
par(mfrow=c(1,2))
boxplot(d$age)
quantile(d$age,seq(0,1,0.01))

#replacing the outlier with 99% value 
d$age = ifelse(d$age > 70,70,d$age)
boxplot(d$age)


#checking for outliers in age column
par(mfrow=c(1,2))
boxplot(d$balance)
quantile(d$balance,seq(0,1,0.01))

#replacing the outlier with 96% value 
d$balance = ifelse(d$balance > 6500,6500,d$balance)

#replacing the outlier with 1% value 
d$balance = ifelse(d$balance < -627,-627,d$balance)
boxplot(d$balance)


#checking for outliers in campaign column
par(mfrow=c(1,2))
boxplot(d$campaign)
quantile(d$campaign,seq(0,1,0.01))

#replacing the outlier with 96% value 
d$campaign = ifelse(d$campaign > 8,8,d$campaign)

boxplot(d$campaign)

library(caret)
set.seed(9)


#checking for count for ditinct levels in job
summary(d$job)

#converting them to numeric
d$job_admin = as.numeric(d$job == "admin")
d$job_bc = as.numeric(d$job == "blue-collar")
d$job_e = as.numeric(d$job == "entrepreneur")
d$job_h = as.numeric(d$job == "housemaid")
d$job_m = as.numeric(d$job == "management")
d$job_r = as.numeric(d$job == "retired")
d$job_se = as.numeric(d$job == "self-employed")
d$job_ser = as.numeric(d$job == "services")
d$jobs_stu = as.numeric(d$job == "student")
d$job_t = as.numeric(d$job == "technician")
d$job_un = as.numeric(d$job == "unemployed")

#removing the original column
d = d[-2]

head(d)


#checking for count for ditinct levels in marital
summary(d$marital)

#converting them to numeric
d$marital_m = as.numeric(d$marital == "married")
d$marital_s = as.numeric(d$marital == "single")

#removing the original column
d = d[-2]

head(d)


#checking for count for ditinct levels in education
summary(d$education)

#converting them to numeric
d$education_p = as.numeric(d$education == "primary")
d$education_s = as.numeric(d$education == "secondary")
d$education_t = as.numeric(d$education == "tertiary")

#removing the original column
d = d[-2]

head(d)


#checking for count for ditinct levels in default
summary(d$default)

#converting them to numeric
d$default_n = as.numeric(d$default == "no")

#removing the original column
d = d[-2]

head(d)

#checking for count for ditinct levels in housing
summary(d$housing)

#converting them to numeric
d$housing_y = as.numeric(d$housing == "yes")

#removing the original column
d = d[-3]

head(d)


#checking for count for ditinct levels in loan
summary(d$loan)

#converting them to numeric
d$loan_n = as.numeric(d$loan == "no")

#removing the original column
d = d[-3]

head(d)


#checking for count for ditinct levels in loan
summary(d$poutcome)

#converting them to numeric
d$poutcome_f = as.numeric(d$poutcome == "failure")
d$poutcome_o = as.numeric(d$poutcome == "other")
d$poutcome_u = as.numeric(d$poutcome == "unknown")

#removing the original column
d = d[-4]

head(d)

#d$y = ifelse(d$y=="no",0,1)

#Dividing the data in training and testing
t = sample(1:nrow(d),0.7*nrow(d))
train = d[t,]
test = d[-t,]


library(car)
model1 = glm(y~., family = "binomial", data = train)
summary(model1)
vif(model1)

s = step(model1)
summary(s)

#new model with the required columns based on the s
new_m = glm(y ~ balance + campaign + job_admin + job_m + job_r + jobs_stu + 
               job_t + job_un + marital_m + marital_s + education_p + education_t + 
               housing_y + loan_n + poutcome_f + poutcome_o + poutcome_u, family = "binomial", data = train)
summary(new_m)

#Making prediction on the testing data
p = predict(new_m,test,type = "response")
head(p)

#Adding the prediction to the test data
output = cbind(test,p)
head(output)

#Keeping the actual and predicted value 
output = output[c(4,27)]

#Replacing the probabilities with the labels
#based on the threshold value set
output$p = ifelse(output$p>0.6,"yes","no")

#0 -> No , 1-> Yes
library(caret)

#ploting the confusion matrix
confusionMatrix(table(output$y,output$p))
