#remove all the objects stored
rm(list = ls())
library(readr)
#set the working directory
setwd("C:/Users/lenovo/Desktop/Data Scientist")

#Check the current working directory
getwd()

#Reading the given CSV files
test_df <- read.csv("test.csv")
train <- read.csv("train.csv")

#viewing the data set
View(train)

#Compactly displays the contents of the data of each variable,their datatypes
str(train)

#Dimension of train and test data set i.e , count of observations and variables
dim(train)
dim(test_df)

#displays the first 5 rows of the data
head(train) 

#Creating a data frame of missing values of both train and test dataset
missing_val_train = data.frame(apply(train,2,function(x){sum(is.na(x))}))
missing_val_test = data.frame(apply(test_df,2,function(x){sum(is.na(x))}))

#Naming the missing value data set 
names(missing_val_train)[1] = "Missing_Values_train"
names(missing_val_test)[1] = "Missing_Values_test"

#Calculating the percentage of missing values
missing_per_train = (missing_val_train$Missing_Values_train/nrow(train))*100
missing_per_test = (missing_val_test$Missing_Values_test/nrow(test_df))*100

#We observe that there are no missing values in both the train and test data set

#convert the target var of train data to factor
train$target<-as.factor(train$target)

#load and attach add-on packages
require(gridExtra)

#Count of target classes
table(train$target)

#Percenatge counts of target classes
table(train$target)/length(train$target)*100

#Bar plot for count of target classes
library(ggplot2)
ggplot(train,aes(target))+theme_bw()+geom_bar(stat='count',fill='pink')
#We have a unbalanced data,where 90% of the data is the number of customers those will not make a transaction and 10% of the data is those who will make a transaction.

#Distribution of train attributes from 3 to 102

for (var in names(train)[c(3:102)]){
  target<-train$target
  plot<-ggplot(train, aes(x=train[[var]],fill=target)) +
    geom_density(kernel='gaussian') + ggtitle(var)+theme_classic()
  print(plot)
}

#Distribution of train attributes from 103 to 202
for (var in names(train)[c(103:202)]){
  target<-train$target
  plot<-ggplot(train, aes(x=train[[var]], fill=target)) +
    geom_density(kernel='gaussian') + ggtitle(var)+theme_classic()
  print(plot)
}
#We observed that there are considerable number of features which significantly have different distributions for two target variables. For example like var_0,var_1,var_9,var_198 var_180 etc.
#We observed that there are considerable number of features which significantly have same distributions for two target variables. For example like var_3,var_7,var_10,var_171,var_185 etc.

#Find mean values per row in train and test data.
train_mean<-apply(train[,-c(1,2)],MARGIN=1,FUN=mean)
test_mean<-apply(test_df[,-c(1)],MARGIN=1,FUN=mean)

ggplot()+
  #Distribution of mean values per row in train data
  geom_density(data=train[,-c(1,2)],aes(x=train_mean),kernel='gaussian',show.legend=TRUE,color='black')+theme_classic()+
  #Distribution of mean values per row in test data
  geom_density(data=test_df[,-c(1)],aes(x=test_mean),kernel='gaussian',show.legend=TRUE,color='purple')+
  labs(x='mean values per row',title="Distribution of mean values per row in train and test dataset")

#find mean values per column in train and test data.
train_mean<-apply(train[,-c(1,2)],MARGIN=2,FUN=mean)
test_mean<-apply(test_df[,-c(1)],MARGIN=2,FUN=mean)
ggplot()+
  #Distribution of mean values per column in train data
  geom_density(aes(x=train_mean),kernel='gaussian',show.legend=TRUE,color='red')+theme_classic()+
  #Distribution of mean values per column in test data
  geom_density(aes(x=test_mean),kernel='gaussian',show.legend=TRUE,color='yellow')+
  labs(x='mean values per column',title="Distribution of mean values per column in train and test dataset")

#find standard deviation values per row in train and test data.
train_sd<-apply(train[,-c(1,2)],MARGIN=1,FUN=sd)
test_sd<-apply(test_df[,-c(1)],MARGIN=1,FUN=sd)
ggplot()+
  #Distribution of sd values per row in train data
  geom_density(data=train[,-c(1,2)],aes(x=train_sd),kernel='gaussian',show.legend=TRUE,color='red')+theme_classic()+
  #Distribution of mean values per row in test data
  geom_density(data=test_df[,-c(1)],aes(x=test_sd),kernel='gaussian',show.legend=TRUE,color='blue')+
  labs(x='sd values per row',title="Distribution of sd values per row in train and test dataset")

#find sd values per column in train and test data.
train_sd<-apply(train[,-c(1,2)],MARGIN=2,FUN=sd)
test_sd<-apply(test_df[,-c(1)],MARGIN=2,FUN=sd)
ggplot()+
  #Distribution of sd values per column in train data
  geom_density(aes(x=train_sd),kernel='gaussian',show.legend=TRUE,color='red')+theme_classic()+
  #Distribution of sd values per column in test data
  geom_density(aes(x=test_sd),kernel='gaussian',show.legend=TRUE,color='yellow')+
  labs(x='sd values per column',title="Distribution of sd values per column in train and test dataset")

#Correlations in train data
#convert factor to int
train$target<-as.numeric(train$target)
train_correlations<-cor(train[,c(2:202)])
train_correlations
#We observed that the correlation between the train attributes is very small.

#Correlations in test data
test_correlations<-cor(test_df[,c(2:201)])
test_correlations
#We observed that the correlation between the test attributes is very small.

#Feature engineering
#Variable importance

#Variable importance is used to see top features in dataset based on mean decreses gini.

#Let us build simple model to find features which are more important.

#Split the training data using simple random sampling
train_index<-sample(1:nrow(train),0.75*nrow(train))
#train data
train_data<-train[train_index,]
#validation data
valid_data<-train[-train_index,]
#dimension of train and validation data
dim(train_data)
dim(valid_data)

# Random forest classifier
#Training the Random forest classifier
set.seed(2732)
#convert to int to factor
train_data$target<-as.factor(train_data$target)
#setting the mtry
mtry<-floor(sqrt(200))
#setting the tunegrid
tuneGrid<-expand.grid(.mtry=mtry)
#fitting the random forest
library(randomForest)
rf<-randomForest(target~.,train_data[,-c(1)],mtry=mtry,ntree=10,importance=TRUE)
#Feature importance by random forest
#Variable importance
VarImp<-importance(rf,type=2)
VarImp
#We can observe that the top important features are var_12, var_26, var_22,v var_174, var_198 and so on based on Mean decrease gini.

#calculate partial dependence plot using random forest
#Let us see the impact of the main features  

library(tidyverse)
library(moments)
library(DataExplorer)
library(caret)
library(Matrix)
library(pdp)
library(mlbench)
library(caTools)
library(glmnet)
library(mlr)
library(vita)
library(rBayesianOptimization)
library(lightgbm)
library(pROC)
library(DMwR)
library(ROSE)
library(yardstick)

#We will plot "var_13"
par.var_13 <- partial(rf, pred.var = c("var_13"), chull = TRUE)
plot.var_13 <- autoplot(par.var_13, contour = TRUE)
plot.var_13
#Handling of imbalanced data

#following are  different approaches for dealing with imbalanced datasets.

#Change the performance metric
#Oversample minority class
#Undersample majority class

#start with simple Logistic regression model.

#Split the data using CreateDataPartition
set.seed(689)
#train.index<-createDataPartition(train_df$target,p=0.8,list=FALSE)
train.index<-sample(1:nrow(train),0.8*nrow(train))
#train data
train.data<-train[train.index,]
#validation data
valid.data<-train[-train.index,]
#dimension of train data
dim(train.data)
#dimension of validation data
dim(valid.data) 
#target classes in train data
table(train.data$target)
#target classes in validation data
table(valid.data$target)
#Logistic Regression model

#Training and validation dataset

#Training dataset
X_t<-as.matrix(train.data[,-c(1,2)])
y_t<-as.matrix(train.data$target)
#validation dataset
X_v<-as.matrix(valid.data[,-c(1,2)])
y_v<-as.matrix(valid.data$target)
#test dataset
test<-as.matrix(test_df[,-c(1)])
#Logistic regression model
set.seed(667) # to reproduce results
lr_model <-glmnet(X_t,y_t, family = "binomial")
summary(lr_model)

#Cross validation prediction
set.seed(8909)
cv_lr <- cv.glmnet(X_t,y_t,family = "binomial", type.measure = "class")
cv_lr

#Plotting the misclassification error vs log(lambda) where lambda is regularization parameter
#Minimum lambda
cv_lr$lambda.min
#plot the auc score vs log(lambda)
plot(cv_lr)
#We observed that misclassification error increases as the log(Lambda) vaue increases.

#Model performance on validation dataset
set.seed(5363)
cv_predict.lr<-predict(cv_lr,X_v,s = "lambda.min", type = "class")
cv_predict.lr
#Accuracy of the model is not the best metric to use when evaluating the imbalanced datasets as it may be misleading. So, we are going to change the performance metric.

#Confusion matrix
set.seed(689)
#actual target variable
target<-valid.data$target
#convert to factor
target<-as.factor(target)
#predicted target variable
#convert to factor
cv_predict.lr<-as.factor(cv_predict.lr)
confusionMatrix(data=cv_predict.lr,reference=target)

#predict the model
set.seed(763)
lr_pred<-predict(lr_model,test,type='class')
lr_pred

#Oversample minority class:

#  It can be defined as adding more copies of minority class.
#It can be a good choice when we don't have a ton of data to work with.
#Drawback is that we are adding information.This may lead to overfitting and poor performance on test data.

#Undersample majority class:

#It can be defined as removing some observations of the majority class.
#It can be a good choice when we have a ton of data -think million of rows.
#Drawback is that we are removing information that may be valuable.This may lead to underfitting and poor performance on test data.
#Both Oversampling and undersampling techniques have some drawbacks. 

#Random Oversampling Examples(ROSE)

#It creates a sample of synthetic data by enlarging the features space of minority and majority class examples.

#Random Oversampling Examples(ROSE)

set.seed(699)
train.rose <- ROSE(target~., data =train.data[,-c(1)],seed=32)$data
#target classes in balanced train data
table(train.rose$target)
valid.rose <- ROSE(target~., data =valid.data[,-c(1)],seed=42)$data
#target classes in balanced valid data
table(valid.rose$target)

#Let us see how baseline logistic regression model performs on synthetic data points.
#Logistic regression model
set.seed(462)
lr_rose <-glmnet(as.matrix(train.rose),as.matrix(train.rose$target), family = "binomial")
summary(lr_rose)
#Cross validation prediction
set.seed(473)
cv_rose = cv.glmnet(as.matrix(valid.rose),as.matrix(valid.rose$target),family = "binomial", type.measure = "class")
cv_rose

#Plotting the misclassification error vs log(lambda) where lambda is regularization parameter
#Minimum lambda
cv_rose$lambda.min
#plot the auc score vs log(lambda)
plot(cv_rose)

#Model performance on validation dataset
set.seed(442)
cv_predict.rose<-predict(cv_rose,as.matrix(valid.rose),s = "lambda.min", type = "class")
cv_predict.rose

#Confusion matrix
set.seed(478)
#actual target variable
target<-valid.rose$target
#convert to factor
target<-as.factor(target)
#predicted target variable
#convert to factor
cv_predict.rose<-as.factor(cv_predict.rose)
#Confusion matrix
confusionMatrix(data=cv_predict.rose,reference=target)
#Therefore,We observe that ROSE model is performing well on imbalance data compared to baseline logistic regression which can be used for further purposes.
