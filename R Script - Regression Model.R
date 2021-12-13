#FINAL PROJECT 


#installing packages
install.packages("car")


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
library(MASS)
library(ggpubr)
library(broom)
library(corrplot)
library(gtsummary)
library(car)


#reading csv file
deathrate_dataset <- read.csv("number-of-deaths-by-risk-factor.csv")
deathrate_dataset



#describing the data
colnames(deathrate_dataset)

start_records <- head(deathrate_dataset,10)
start_records  

end_records <- tail(deathrate_dataset,10)
end_records

dim(deathrate_dataset)

str(deathrate_dataset)

summary_dataset <- summary(deathrate_dataset)
summary_dataset

classtype_variable <- sapply(deathrate_dataset, class)
classtype_variable



#descriptive analysis

#alcohol use
min(deathrate_dataset$Alcohol.use)
max(deathrate_dataset$Alcohol.use)
mean(deathrate_dataset$Alcohol.use)
median(deathrate_dataset$Alcohol.use)
sd(deathrate_dataset$Alcohol.use)
range(deathrate_dataset$Alcohol.use)
summary(deathrate_dataset$Alcohol.use)

#air pollution
min(deathrate_dataset$Air.pollution)
max(deathrate_dataset$Air.pollution)
mean(deathrate_dataset$Air.pollution)
median(deathrate_dataset$Air.pollution)
sd(deathrate_dataset$Air.pollution)
range(deathrate_dataset$Air.pollution)
summary(deathrate_dataset$Air.pollution)

#smoking
min(deathrate_dataset$Smoking)
max(deathrate_dataset$Smoking)
mean(deathrate_dataset$Smoking)
median(deathrate_dataset$Smoking)
sd(deathrate_dataset$Smoking)
range(deathrate_dataset$Smoking)
summary(deathrate_dataset$Smoking)

#drug use
min(deathrate_dataset$Drug.use)
max(deathrate_dataset$Drug.use)
mean(deathrate_dataset$Drug.use)
median(deathrate_dataset$Drug.use)
sd(deathrate_dataset$Drug.use)
range(deathrate_dataset$Drug.use)
summary(deathrate_dataset$Drug.use)

#hand washing facility
min(deathrate_dataset$No.access.to.handwashing.facility)
max(deathrate_dataset$No.access.to.handwashing.facility)
mean(deathrate_dataset$No.access.to.handwashing.facility)
median(deathrate_dataset$No.access.to.handwashing.facility)
sd(deathrate_dataset$No.access.to.handwashing.facility)
range(deathrate_dataset$No.access.to.handwashing.facility)
summary(deathrate_dataset$No.access.to.handwashing.facility)



#frequency table
unique_entity <- unique(deathrate_dataset$Entity)
unique_entity

table_counrty <- table(deathrate_dataset$Entity)
table_counrty

count_entity <- count(deathrate_dataset$Entity)
count_entity



#creating new subset of dataset

#Entity = India
india_entity <- subset(deathrate_dataset, subset=(deathrate_dataset$Entity=="India"))
india_entity

#Entity = North America
northamerica_entity <- subset(deathrate_dataset, subset = (deathrate_dataset$Entity=="North America"))
northamerica_entity

#Entity = Maldives
maldives_entity <- subset(deathrate_dataset, subset = (deathrate_dataset$Entity=="Maldives"))
maldives_entity



#visualizations

#graph 1 : Boxplot of the attributes for Entity India
par(mfrow=c(2,2))
boxplot(india_entity$Alcohol.use, col = "green", main = "Alcohol Use in India")
boxplot(india_entity$Drug.use, col = "yellow", main = "Drug Use in India")
boxplot(india_entity$Smoking, col = "orange", main = "Smoking in India")
boxplot(india_entity$Air.pollution, col = "blue", main = "Air Pollution in India")


#graph 2 : Boxplot of the attributes for Entity North America
par(mfrow=c(2,2))
boxplot(northamerica_entity$Alcohol.use, col = "blue", main = "Alcohol Use in North America")
boxplot(northamerica_entity$Drug.use, col = "orange", main = "Drug Use in North America")
boxplot(northamerica_entity$Smoking, col = "green", main = "Smoking in North America")
boxplot(northamerica_entity$Air.pollution, col = "yellow", main = "Air Pollution in North America")


#graph 3 : Boxplot of the attributes for Entity Maldives
par(mfrow=c(2,2))
boxplot(maldives_entity$Alcohol.use, col = "yellow", main = "Alcohol Use in Maldives")
boxplot(maldives_entity$Drug.use, col = "green", main = "Drug Use in Maldives")
boxplot(maldives_entity$Smoking, col = "blue", main = "Smoking in Maldives")
boxplot(maldives_entity$Air.pollution, col = "orange", main = "Air Pollution in Maldives")


#graph 4 : Density Plot of the attributes for Entity India
par(mfrow=c(2, 2)) 
plot(density(india_entity$Alcohol.use), main = "Density Plot, Entity-India : Risk Factor, Alcohol Use", ylab = "Frequency")
polygon(density(india_entity$Alcohol.use), col = "cadetblue")
plot(density(india_entity$Drug.use), main = "Density Plot, Entity-India : Risk Factor, Drug Use", ylab = "Frequency")
polygon(density(india_entity$Drug.use), col = "blue")
plot(density(india_entity$Smoking), main = "Density Plot, Entity-India : Risk Factor, Smoking", ylab = "Frequency")
polygon(density(india_entity$Smoking), col = "lightblue")
plot(density(india_entity$Air.pollution), main = "Density Plot, Entity-India : Risk Factor, Air Pollution", ylab = "Frequency")
polygon(density(india_entity$Air.pollution), col = "darkblue")


#graph 5 : Density Plot of the attributes for Entity North America
par(mfrow=c(2, 2)) 
plot(density(northamerica_entity$Alcohol.use), main = "Density Plot, Entity-NorthAmerica : Risk Factor, Alcohol Use", ylab = "Frequency")
polygon(density(northamerica_entity$Alcohol.use), col = "gray")
plot(density(northamerica_entity$Drug.use), main = "Density Plot, Entity-NorthAmerica : Risk Factor, Drug Use", ylab = "Frequency")
polygon(density(northamerica_entity$Drug.use), col = "cadetblue")
plot(density(northamerica_entity$Smoking), main = "Density Plot, Entity-NorthAmerica : Risk Factor, Smoking", ylab = "Frequency")
polygon(density(northamerica_entity$Smoking), col = "yellow")
plot(density(northamerica_entity$Air.pollution), main = "Density Plot, Entity-NorthAmerica : Risk Factor, Air Pollution", ylab = "Frequency")
polygon(density(northamerica_entity$Air.pollution), col = "pink")


#graph 6 : Density Plot of the attributes for Entity Maldives
par(mfrow=c(2, 2)) 
plot(density(maldives_entity$Alcohol.use), main = "Density Plot, Entity-Maldives : Risk Factor, Alcohol Use", ylab = "Frequency")
polygon(density(maldives_entity$Alcohol.use), col = "pink")
plot(density(maldives_entity$Drug.use), main = "Density Plot, Entity-Maldives : Risk Factor, Drug Use", ylab = "Frequency")
polygon(density(maldives_entity$Drug.use), col = "orange")
plot(density(maldives_entity$Smoking), main = "Density Plot, Entity-Maldives : Risk Factor, Smoking", ylab = "Frequency")
polygon(density(maldives_entity$Smoking), col = "yellow")
plot(density(maldives_entity$Air.pollution), main = "Density Plot, Entity-Maldives : Risk Factor, Air Pollution", ylab = "Frequency")
polygon(density(maldives_entity$Air.pollution), col = "lightblue")



#1.relationship between year and alcohol use in entity India

#correlation
correlation_value_india_alcoholuse <- cor(india_entity$Alcohol.use, india_entity$Year)
correlation_value_india_alcoholuse

#linear regression model
plot(india_entity$Year, india_entity$Alcohol.use, col = "blue")
linearregression_model1 <- lm(Alcohol.use ~ Year, data = india_entity)
linearregression_model1
abline(linearregression_model1, col = "red")
summary(linearregression_model1)

#regression table 1
tbl_regression(linearregression_model1)



#2.relationship between year and drug use in entity India

#correlation
correlation_value_india_druguse <- cor(india_entity$Drug.use, india_entity$Year)
correlation_value_india_druguse

#linear regression model
plot(india_entity$Year, india_entity$Drug.use, col = "blue")
linearregression_model2 <- lm(Drug.use ~ Year, data = india_entity)
linearregression_model2
abline(linearregression_model2, col = "red")
summary(linearregression_model2)

#regression table 2
tbl_regression(linearregression_model2)



#3.relationship between year and drug use in entity North America

#correlation
correlation_value_northamerica_druguse <- cor(northamerica_entity$Drug.use, northamerica_entity$Year)
correlation_value_northamerica_druguse

#linear regression model
plot(northamerica_entity$Year, northamerica_entity$Drug.use, col = "blue")
linearregression_model3 <- lm(Drug.use ~ Year, data = northamerica_entity)
linearregression_model3
abline(linearregression_model3, col = "red")
summary(linearregression_model3)


#regression table 3
tbl_regression(linearregression_model3)



#4.relationship between year and smoking in entity Maldives

#correlation
correlation_value_maldives_smoking <- cor(maldives_entity$Smoking, maldives_entity$Year)
correlation_value_maldives_smoking

#linear regression model
plot(maldives_entity$Year, maldives_entity$Smoking, col = "blue")
linearregression_model4 <- lm(Smoking ~ Year, data = maldives_entity)
linearregression_model4
abline(linearregression_model4, col = "red")
summary(linearregression_model4)


#regression table 4
tbl_regression(linearregression_model4)



#subset of the dataset
new_india_entity <- data.frame(india_entity$Year, india_entity$Alcohol.use, india_entity$Drug.use, 
                               india_entity$Smoking, india_entity$Air.pollution, 
                               india_entity$No.access.to.handwashing.facility)
new_india_entity

new_northamerica_entity <- data.frame(northamerica_entity$Year, northamerica_entity$Alcohol.use, northamerica_entity$Drug.use, 
                                      northamerica_entity$Smoking, northamerica_entity$Air.pollution, 
                                      northamerica_entity$No.access.to.handwashing.facility)
new_northamerica_entity

new_maldives_entity <- data.frame(maldives_entity$Year, maldives_entity$Alcohol.use, maldives_entity$Drug.use, 
                                  maldives_entity$Smoking, maldives_entity$Air.pollution, 
                                  maldives_entity$No.access.to.handwashing.facility)
new_maldives_entity



#correlation table & chart

#india entity
new_india_entity.cor = cor(new_india_entity)
new_india_entity.cor

corrplot(new_india_entity.cor)


#north america entity
new_northamerica_entity.cor = cor(new_northamerica_entity)
new_northamerica_entity.cor

corrplot(new_northamerica_entity.cor)



#MV Regression model for entity India
mvregression1 <- lm(cbind(Smoking, Drug.use) ~ Year, data = india_entity)
mvregression1
summary(mvregression1)
coef(mvregression1)
sigma(mvregression1)
vcov(mvregression1)
Anova(mvregression1)      #Analysis of Variance Table


#MV Regression model for entity North America
mvregression2 <- lm(cbind(Alcohol.use, Air.pollution) ~ Year, data = northamerica_entity)
mvregression2
summary(mvregression2)
coef(mvregression2)
sigma(mvregression2)
vcov(mvregression2)
Anova(mvregression2)


#MV Regression model for entity Maldives
mvregression3 <- lm(cbind(Drug.use, Alcohol.use) ~ Year, data = maldives_entity)
mvregression3
summary(mvregression3)
coef(mvregression3)
sigma(mvregression3)
vcov(mvregression3)
Anova(mvregression3)











