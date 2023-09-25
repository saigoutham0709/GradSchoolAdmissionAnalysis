getwd()
#setting directory

setwd("C:/Users/namra/Downloads")
getwd()

#reading data file
data1 <- read.csv('STATFINAL.csv')
fdata = na.omit(data1)
fdata

#libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(tidyr)
library(randomForest)

#removing serial numbers column from the dataset
data =select(fdata, -1)
data

#data description
dim(data)
summary(data)
names(data)
str(data)
pairs(data)


#GRE scatter plot
ggplot(data, aes(x=GRE.Score,y=Chance.of.Admit))+geom_point(aes(color=Chance.of.Admit)) + scale_color_gradient(low='red',high='green')+ggtitle("GRE score VS Chance of Admit")+
  labs(x="GRE Score",y="Chance of Admit")
#CGPA scatter plot
ggplot(data, aes(x=CGPA,y=Chance.of.Admit))+geom_point(aes(color=Chance.of.Admit)) + scale_color_gradient(low='red',high='green')+ggtitle("CGPA VS Chance of Admit")+
  labs(x="CGPA", y = "Chance of Admit")

#TOEFL Barplot 
ggplot(data, aes(x=TOEFL.Score,y=Chance.of.Admit, fill = TOEFL.Score))+geom_bar(stat = "identity")+ggtitle("TOEFL score VS Chance of Admit")+
  labs(x="TOEFL Score", y="Chance of Admit")

#University Rating
#Boxplot
ggplot(data,aes(factor(University.Rating),Chance.of.Admit)) + geom_boxplot(aes(fill=University.Rating))+
  labs(x="University Rating", y="Chance of Admit", title = "University Rating VS Chance of Admit")
#Histogram
ggplot(data,aes(University.Rating)) + geom_histogram(aes(fill=..count..),color='black',bins=5)+ggtitle("University Rating")+
  labs(x="Univeristy Rating",y="Count")

#Research
#Boxplot
ggplot(data,aes(factor(Research),Chance.of.Admit)) + geom_boxplot(aes(fill=Research))+labs(x="Research Experience")+
  labs(x="Research Experience", y="Chance of Admit")+ggtitle("Research Experience Vs Chance of admit Boxplot")
#Histogram
ggplot(data,aes(Research)) + geom_histogram(aes(fill=..count..),color='black',bins=2)+ggtitle("Research Experience VS Chance of Admit")+labs(x="Research Experience", y="Count")
  
#Density Plot
ggplot(data,aes(GRE.Score,color=factor(Research)))+geom_density(size=2)+ggtitle("GRE vs Research Experience Distribution")

#LOR Barplot
ggplot(data, aes(x=LOR,y=Chance.of.Admit, fill = LOR))+geom_bar(stat = "identity")+ggtitle("LOR Rating VS Chance of Admit")+
  labs(x="LOR rating", y="Chance of Admit")
#CORRELATION ANALYSIS

library(corrplot)
corrplot(cor(data[2:8]), method='color')

#REGRESSION ANALYSIS

#Create Training and Test data 
set.seed(100)  # setting seed to reproduce results of random sampling

trainingRowIndex <- sample(1:nrow(data), 0.8*nrow(data))  # row indices for training data
trainingData <- data[trainingRowIndex, ]  # model training data
testData  <- data[-trainingRowIndex, ] #model test data
dim(trainingData)
dim(testData)
#predictive analysis and multiple linear model
lmMod <- lm(Chance.of.Admit ~., data=trainingData)  # build the model
summary (lmMod)
par(mfrow=c(2,2))
plot(lmMod)

AdmitPred <- predict(lmMod, testData)  # predict admit
actuals_preds <- data.frame(cbind(actuals=testData$Chance.of.Admit, predicteds=AdmitPred))
head(actuals_preds)

#accuracy of lm model
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max)) 
min_max_accuracy
#mean absolute percentage deviation
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)  
mape

#predictive analysis and random forest model
rf <- randomForest(Chance.of.Admit ~., data=trainingData,mtry = 2, ntree=500,importance=TRUE)  # build the model
importance(rf)
varImpPlot(rf, main="Importance plots for random forest model")#importance plots
summary(rf)
rf

AdmitPred <- predict(rf, testData)  # predict admit

actuals_preds <- data.frame(cbind(actuals=testData$Chance.of.Admit, predicteds=AdmitPred))
head(actuals_preds)
#accuracy for random forest model
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max)) 
min_max_accuracy
#mean absolute percentage deviation
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)  
mape


#Predictive analysis and logistic regression model
lg <- glm(Chance.of.Admit ~., data=trainingData, family="quasibinomial")  # build the model
summary (lg) #view the model summary
par(mfrow=c(2,2))
plot(lg)

AdmitPred <- predict(lg, testData)  # predict admit

actuals_preds <- data.frame(cbind(actuals=testData$Chance.of.Admit, predicteds=AdmitPred))
head(actuals_preds)
#accuracy for logistic regression model
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max)) 
min_max_accuracy
#mean absolute percentage deviation
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)  
mape
