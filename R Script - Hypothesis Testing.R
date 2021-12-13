#FINAL PROJECT - MILESTONE 2


#importing libraries

library(FSA)
library(FSAdata)
library(magrittr)
library(dplyr)
library(tidyr)
library(plyr)
library(ggplot2)
library(scales)
library(ggboxplot)


#reading csv file
deathrate_dataset <- read.csv("number-of-deaths-by-risk-factor.csv")
deathrate_dataset


#descriptive analysis of the data set
colnames(deathrate_dataset)

start_records <- head(deathrate_dataset,10)
start_records  
  
end_records <- tail(deathrate_dataset,10)
end_records

summary(deathrate_dataset)



#creating a randomly selected sample data set
randomly_selected_dataset <- sample_n(deathrate_dataset,1500)
randomly_selected_dataset



#descriptive analysis for the sample data set
colnames(randomly_selected_dataset)
summary(randomly_selected_dataset)



#create new variables for one sample testing
air_pollution <- randomly_selected_dataset$Air.pollution
air_pollution

smoking <- randomly_selected_dataset$Smoking
smoking

no_access_to_handwash <- randomly_selected_dataset$No.access.to.handwashing.facility
no_access_to_handwash


sample_entity <- randomly_selected_dataset$Entity
sample_entity


table_entity <- table(sample_entity)
table_entity


#subset from original data set
india_entity <- subset(deathrate_dataset, subset=(deathrate_dataset$Entity=="India"))
india_entity

australia_entity <- subset(deathrate_dataset, subset = (deathrate_dataset$Entity=="Australia"))
australia_entity

eastasia_entity <- subset(deathrate_dataset, subset = (deathrate_dataset$Entity=="East Asia"))
eastasia_entity



#descriptive analysis of the variables created
mean(air_pollution)
summary(air_pollution)
mean(smoking)
summary(smoking)
mean(no_access_to_handwash)
summary(no_access_to_handwash)
mean(india_entity$Alcohol.use)
summary(india_entity$Alcohol.use)
mean(australia_entity$Drug.use)
summary(australia_entity$Drug.use)
mean(eastasia_entity$Iron.deficiency)
summary(eastasia_entity$Iron.deficiency)



#data visualizations
par(mfrow=c(2,2))
boxplot(air_pollution, main = "Risk factor - Air Pollution", col = "lightblue")
boxplot(smoking, main ="Risk factor - Smoking", col = "cadetblue")
boxplot(no_access_to_handwash, main = "Risk factor - No access to handwash facility", col = "lightblue")
barplot(table_entity, main = "Frequency of the Entity of sample dataset", col = "cadetblue", xlab = "Entity")



#one sample testing for overall data set
t.test(air_pollution, mu = 104500)           #reject the alternative hypothesis
t.test(air_pollution, mu = 2000)             #reject the null hypothesis
t.test(air_pollution, mu = 200000)           #reject the null hypothesis

t.test(smoking, mu = 150000)       #reject the alternative hypothesis
t.test(smoking, mu = 100000)       #reject the null hypothesis
t.test(smoking, mu = 2000000)      #reject the null hypothesis

t.test(no_access_to_handwash, mu = 25000)             #reject null hypothesis
t.test(no_access_to_handwash, mu = 13000)             #reject null hypothesis
t.test(no_access_to_handwash, mu = 17000)             #reject alternative hypothesis



#one sample testing for overall data set with additional parameters
t.test(air_pollution, mu = 104520, alternative = "greater")            #reject alternative hypothesis
t.test(smoking, mu = 200000, alternative = "less")                     #reject null hypothesis
t.test(no_access_to_handwash, mu = 16000 , alternative = "less")       #reject alternative hypothesis



#visualizations for testing

par(mfrow=c(2,2))
#plot1 : air pollution
qqnorm(air_pollution, main = "Normal Q-Q Plot for Air Pollution", col = "blue")
qqline(air_pollution, lty=10, col = "red")
#plot2 : smoking
qqnorm(smoking, main = "Normal Q-Q Plot for Smoking", col = "yellow")
qqline(smoking, lty=10, col ="red")
#plot3 : no access to hand wash
qqnorm(no_access_to_handwash, main = "Normal Q-Q Plot for No access to handwash facility", col = "green")
qqline(no_access_to_handwash, lty=10, col = "red")



#one sample testing for subset of data set
t.test(india_entity$Alcohol.use, mu = 430000)                                      #reject the alternative hypothesis
t.test(india_entity$Alcohol.use, mu = 200000, alternative = "greater")             #reject the null hypothesis

t.test(australia_entity$Drug.use, mu = 2000)                                       #reject the alternative hypothesis
t.test(australia_entity$Drug.use, mu = 3000, alternative = "less")                 #reject the null hypothesis

t.test(eastasia_entity$Iron.deficiency, mu = 3100)                                 #reject the null hypothesis
t.test(eastasia_entity$Iron.deficiency, mu = 2000, alternative = "greater")        #reject the alternative hypothesis


#visualizations for testing

par(mfrow=c(2,2))
#plot 1 : India Entity
qqnorm(india_entity$Alcohol.use, main = "Normal Q-Q Plot for Alcohol use in the entity India", col = "blue")
qqline(india_entity$Alcohol.use, lty=10, col = "red")
#plot 2 : Australia Entity
qqnorm(australia_entity$Drug.use, main = "Normal Q-Q Plot for Drug use in the entity Australia", col = "black")
qqline(australia_entity$Drug.use, lty=10, col = "red")
#plot 3 : East Asia Entity
qqnorm(eastasia_entity$Iron.deficiency, main = "Normal Q-Q Plot for Iron Deficiency in the entity East Asia", col = "green")
qqline(eastasia_entity$Iron.deficiency, lty=10, col = "red")



#two sample testing:


#creating new variables
alcohol_use <- randomly_selected_dataset$Alcohol.use
alcohol_use

drug_use <- randomly_selected_dataset$Drug.use
drug_use


#subset of data set
england_entity <- subset(deathrate_dataset, subset = (deathrate_dataset$Entity=="England"))
england_entity

italy_entity <- subset(deathrate_dataset, subset = (deathrate_dataset$Entity=="Italy"))
italy_entity


#descriptive analysis of the variables created
mean(alcohol_use)
summary(alcohol_use)
mean(drug_use)
summary(drug_use)
mean(england_entity$Diet.low.in.fruits)
summary(england_entity$Diet.low.in.fruits)
mean(italy_entity$Diet.low.in.fruits)
summary(italy_entity$Diet.low.in.fruits)


#two sample t-test

#t-test on risk factors
t.test(alcohol_use,drug_use)    #reject the null hypothesis

#output
#since the p-value is less than 0.05, we reject the null hypothesis
#also, the alcohol use in different entities is high/more than that of the drug use which means 
#that the risk factor affecting the death rate is more due to alcohol use as compared to drug use.



#t-test on subset of data set
t.test(england_entity$Diet.low.in.fruits, italy_entity$Diet.low.in.fruits)   #reject the null hypothesis

#output
#since the p-value is less than 0.05, we reject the null hypothesis
#also, the diet low in fruits, a risk factor leading to death rate is more in England as compared to in Italy.



#NOTE: A paired t-test cannot be performed here since we reject the null hypothesis.
#Also, changing the level of significance from 0.05 to 0.1 will not change the conclusion in both situations


















