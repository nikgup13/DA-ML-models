#reading csv file for LA2014 data separated from main file
setwd("C:\\Users\\Nikita\\Desktop\\Final Project\\Progetto")
data = read.csv('LA2014regression.csv')
data

#GLM is done because of presence of non-numeric values in data

glm.fit=glm(Crime.Code.Desc~ Victim.Age + Victim.Sex + Victim.Descent + Time.Occurred + Lat + Lon ,data=data ,family = binomial)
summary(glm.fit)

#calculating coefficients so that positive and negative parameters can be noticed and analysis can be done

coef(glm.fit)
summary(glm.fit)$coef

#probabilities are calculated for all the independent variables 

glm.probs = predict(glm.fit,type = "response")
glm.probs
attach(data)
contrasts(Crime.Code.Desc)

#2015(same is done for 2015 LA data extracted from main dataset)

setwd("C:\\Users\\Nikita\\Desktop\\Final Project\\Progetto")
data1 = read.csv('LA2015regression.csv')
data1
library(MASS)
names(data)
glm.fit = glm(Crime.Code.Desc~ Victim.Age + Victim.Sex + Victim.Descent + Time.Occurred + Lat + Lon ,data=data ,family = binomial)
summary(glm.fit)
coef(glm.fit)
summary(glm.fit)$coef
glm.probs = predict(glm.fit,type = "response")
glm.probs
attach(data1)
contrasts(Crime.Code.Desc)