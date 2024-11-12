#calling required libraries

library(xlsx)
library( MASS)
library( ISLR)
library(car)
library(cluster)

#reading csv file for usa crime 2014,2015 and 2016

setwd("C:\\Users\\Nikita\\Desktop\\Final Project\\Progetto")
data = read.csv('regression14.csv')
data1 = read.csv('regression15.csv')
data16 = read.csv('2016_usa_crime.csv', sep = ',')
data
data1

#converting to dataframe

data14=data.frame(data)
data15=data.frame(data1)
data16_df = data.frame(data16)

x = data16_df$'Aggravated.assault' #putting value of aggravate assault for 2016 in x
x
y = x[0:146] #extracting 146 rows
y
 
#2014

fix(data14)
names(data14)
data14["aggravated_assault_2016"]<- y #making column for aggarvated assault of 2016
data14["m2014"]<- data14["Violent.crime"]*10000/ data14["Population"] #making column for mean population per 10,000 people statewise
data14

# multiple variable linear regression with m2014 as dependent variable and other types of violent crimes as independent variables

lm.fit=lm(m2014~ Murder.and.nonnegligent.manslaughter + Rape..revised.definition. + Robbery + Rape..legacy.definition. + Aggravated.assault, data=data14)
summary(lm.fit)

#we remove murder column as it has high p-value and we can see that aggravated assault has very low p-value so it fits the data very well

lm.fit14=lm(m2014~ Rape..revised.definition. + Robbery + Rape..legacy.definition. + Aggravated.assault, data=data14)
summary(lm.fit14)

confint(lm.fit14) #calculating confidence interval

attach(data14)
predict(lm.fit14, data.frame(Aggravated.assault,interval ="confidence")) #predicting the value of m2014 for all the values of aggravated values

attach(data14)
predict(lm.fit14, data.frame(Aggravated.assault=c(480,573,1005,1055,129,141,65,71,652,701,103,110,85,81,216,253,961,1219,911,1018,342,363,219,199,505,580,84,84,39,70,55,46,165,232,73,76,135,131,42,25,94,100,75,73,53,90,91,105,225,201,115,117,146,162,148,166,263,289,54,74,762,962,162,154,47,51,93,87,105,114,168,161,24,25,287,334,740,741,6308,7531,689,745,139,280,15,31,111,117,1264,1287,172,203,147,175,57,64,235,217,257,226,109,141,212,164,47,62,114,159,209,255,360,470,1183,1142,313,254,735,869,1916,1600,1358,1302,930,1091,86,75,439,511,33,43,86,78,117,146,205,223,50,49,1366,1498,27,39,26,30,35,523), interval ="confidence")) #predicting m2014(which is approximately equal to m2016 when predicted with values of aggravated assault of 2016) using the aggravated assault values of 2016 

#plotting aggravated assault vs m2014 

plot(Aggravated.assault, m2014)
abline(lm.fit14)

#plotting hatvalues(how much independent variables are far from m2014) and finding the index of the maximum variable
plot(hatvalues(lm.fit14))
which.max( hatvalues(lm.fit14))

#2015 (Same fitting as 2014)

fix(data15)
names(data15)
data15["m2015"]<- data15["Violent.crime"]*10000/ data15["Population"]
data15
lm.fit2=lm(m2015~ Murder.and.nonnegligent.manslaughter + Rape..revised.definition. + Robbery + Rape..legacy.definition. + Aggravated.assault, data=data15)
summary(lm.fit2)

#we remove murder column as it has high p-value and we can see that aggravated assault has very low p-value so it fits the data very well

lm.fit15=lm(m2015~ Rape..revised.definition. + Robbery + Rape..legacy.definition. + Aggravated.assault, data=data15)
summary(lm.fit15)

predict(lm.fit15, data.frame(Aggravated.assault,interval ="confidence"))

plot(Aggravated.assault, m2014)
abline(lm.fit14)
plot(hatvalues(lm.fit15))
which.max( hatvalues(lm.fit15))

#null hypothesis
attach(data14)
attach(data15)
mean_df_14 =  matrix(data = 0, nrow = 146, ncol = 1)
mean_df_15 = matrix(data = 0, nrow = 146, ncol = 1)

for  (i in 1:146) {
  s1 = sample(data14$m2014, 146, replace = TRUE)
  s2 = sample(data15$m2015, 30, replace = TRUE)
  
  mean_df_14[i] = mean(s1)
  mean_df_15[i] = mean(s2)
}
alpha = 0.01
alphahalf = alpha / 2
lower = alphahalf
upper = 1-alphahalf
r = 1
q = quantile(mean_df_14, c(lower, upper), na.rm = TRUE)
print(c(alpha,q[1],q[2]))
  
mean_crime_15 = mean(mean_df_15)


#test the null hypothesis that the crime per 10,000 population in 2014 is same as the crime per 10,000 popuplation in 2015

if (( mean_crime_15> q[1]) & (mean_crime_15 < q[2])) {print("ACCEPT")}
if ((mean_crime_15 <= q[1]) || (mean_crime_15 >= q[2])) {print("REJECT")}
print(c(alpha,q[1],q[2]))
print("Mean crime 2015")
mean_crime_15

