#FINAL PROJECT - MILESTONE 1


#importing libraries

library(FSA)
library(FSAdata)
library(magrittr)
library(dplyr)
library(tidyr)
library(plyr)
library(ggplot2)
library(scales)


#reading csv file
deathrate_dataset <- read.csv("number-of-deaths-by-risk-factor.csv")
deathrate_dataset



#describing the data set
colnames(deathrate_dataset)

starting_records <- head(deathrate_dataset,10)
starting_records

ending_records <- tail(deathrate_dataset,10)
ending_records

dimensions_data <- dim(deathrate_dataset)
dimensions_data

summary_dataset <- summary(deathrate_dataset)
summary_dataset

structure_dataset <- str(deathrate_dataset)
structure_dataset

class_of_variable <- sapply(deathrate_dataset,class)
class_of_variable

entity_count <- count(deathrate_dataset$Entity)
entity_count

unique_entity <- unique(deathrate_dataset$Entity)
unique_entity



#creating tables and new attributes
entity_table <- table(deathrate_dataset$Entity)
entity_table



#data visualization
barplot(entity_table,
        main = "Frequency of the Entity",
        xlab = "Entity",
        ylab = "Count",
        col = "lightblue",
        horiz = FALSE,
        ylim = c(0,30),
        cex.names = 0.8)

boxplot(deathrate_dataset$Year,
        main = "Death rate in a particular year",
        xlab = "Year",
        horizontal = TRUE,
        col = "cadetblue")



#data cleaning 

#removing NAN values
alcohol_use_filtered = subset(deathrate_dataset, Alcohol.use != "")
airpollution_filtered = subset(deathrate_dataset, Air.pollution != "")
smoking_filtered = subset(deathrate_dataset, Smoking !="")
drug_use_filtered = subset(deathrate_dataset, Drug.use !="")
handwash_filtered = subset(deathrate_dataset, No.access.to.handwashing.facility !="")



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



#subset of data sets and creating new attributes

#randomly selected subset of data set with 30 records
random_dataset1 <- sample_n(deathrate_dataset,30)
random_dataset1

random_dataset2 <- sample_n(deathrate_dataset,50)
random_dataset2



#filtering data set into subsets of data

dataset <- deathrate_dataset
filtered_dataset_AFG = filter(dataset, Entity=="Afghanistan")
filtered_dataset_AFG

filtered_dataset_IN = filter(dataset, Entity=="India")
filtered_dataset_IN

filtered_dataset_AUT = filter(dataset, Entity=="Austria")
filtered_dataset_AUT

filtered_dataset_CAN = filter(dataset, Entity=="Canada")
filtered_dataset_CAN


#data visualization

#graph 1 : Death rate due to Smoking in the country
ggplot(random_dataset1, aes(x=Smoking, y=Entity, fill=Smoking)) + geom_bar(stat = "identity",width = 0.6) + 
  labs(
    x = "Smoking",
    title = "Death rate due to Smoking in the country") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90))



#graph 2 : Death rate due to Alcohol use in the country
ggplot(random_dataset2, aes(x=Entity, y=Alcohol.use, fill=Alcohol.use)) + geom_point(stat = "identity") + 
  labs(
    x = "Entity",
    title = "Death rate due to Alcohol use in the country") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90))



#graph 3 : Death rate due to no access to hand wash facility in the country
ggplot(random_dataset1, aes(x=Entity, y=No.access.to.handwashing.facility, fill=No.access.to.handwashing.facility)) + geom_bar(stat = "identity",width = 0.6) + 
  labs(
    y = "No access to Handwash facility",
    title = "Death rate due to no access to hand wash facility in the country") +
  theme(plot.title = element_text(hjust = 0.5))
new_graph = last_plot()
new_graph + coord_polar()



#graph 4 : Risk factor due to Air Pollution
boxplot(random_dataset1$Air.pollution,
        main = "Risk factor due to Air Pollution",
        xlab = "Air Pollution",
        horizontal = TRUE,
        col = "lightblue")



#graph 5 : Death rate due to Drug use in the country
ggplot(random_dataset3, aes(x=Entity, y=Drug.use)) + geom_line(color = "lightblue", linetype="dashed", size = 1.5)+ 
  geom_point(color = "lightblue", size = 4, shape = 21, fill = "cadetblue")+ 
  labs(
    x = "Entity",
    title = "Death rate due to Drug use in the country") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90))



#graph 6 : Death rate due to Alcohol use and Drug use based on the year in Afghanistan
x <- filtered_dataset_AFG$Year
y1 <- filtered_dataset_AFG$Alcohol.use
y2 <- filtered_dataset_AFG$Drug.use
par(mar = c(5,5,3,5))
plot(x,y1, type ="l", ylab="Alcohol Use", xlab="Year", main="Death rate due to Alcohol use and Drug use based on the year in 'Afghanistan'", col="blue")
par(new=TRUE)
plot(x,y2, type="l", xaxt = "n", yaxt = "n", ylab="",xlab="",col="red", lty= 2)
axis(side = 4)
mtext("Drug Use",side = 4, line=3)
legend("topleft",c("Alcohol Use","Drug Use"),col = c("blue","red"),lty = c(1,2))



#graph 7 : Death rate due to Alcohol use and Drug use based on the year in India
x <- filtered_dataset_IN$Year
y1 <- filtered_dataset_IN$Alcohol.use
y2 <- filtered_dataset_IN$Drug.use
par(mar = c(5,5,3,5))
plot(x,y1, type ="l", ylab="Alcohol Use", xlab="Year", main="Death rate due to Alcohol use and Drug use based on the year in 'India'", col="blue")
par(new=TRUE)
plot(x,y2, type="l", xaxt = "n", yaxt = "n", ylab="",xlab="",col="red", lty= 2)
axis(side = 4)
mtext("Drug Use",side = 4, line=3)
legend("topleft",c("Alcohol Use","Drug Use"),col = c("blue","red"),lty = c(1,2))


