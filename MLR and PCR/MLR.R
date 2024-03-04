## DEVELOPING LINEAR MOODEL

rm(list=ls())#clearing the environment
setwd("~/Documents/Essay with Prof Babatunde/prac/MLR and PCR")
getwd()
#load pls
library(pls)
#importing the data
df<-read.csv("data1 maize.csv",sep = ',')
names(df)#features
attach(df)
df <- df[,-1]#dropping the years column
head(df)#view first 6 rows
dim(df)#dimension of our dataset
str(df)
sum(is.null(df))#checking for the missing values
par(mfrow = c(1, 2))
boxplot(df[,1])#checking for outliers for each variable using boxplots
boxplot(df[,10])

#partioning the data 70% training 30% testing
df1<-sort(sample(nrow(df), nrow(df)*0.7, replace = F))
train <- df[df1,]
test <- df[-df1,]
dim(train)
dim(test)

#looking for the correlation among variables
library(correlation)
correlation(df)
cor(df)
length(Maize.yield)
res <- cor(train, method="pearson")
corrplot::corrplot(res, method= "color", order = "hclust", tl.pos = 'n')
#we can observe that the darker colors are more meaning there is correlation among variables

#we fit the linear regression model with the training data
mlr<-lm( Maize.yield~., data=train)
summary(mlr)
#lets predict using the linear regression model
library(Metrics)
mlr_pred<-predict(mlr,test)
mlr_pred
predplot(mlr)
abline(0,1,col='green')
# Calculate performance metrics for MLR model
rmse(actual = test$Maize.yield,predicted = as.numeric(mlr_pred))#the lower the error the better
r2_mlr <- cor(mlr_pred, test$Maize.yield)^2
r2_mlr
mse_mlr <- mean((mlr_pred - test$Maize.yield)^2)
mse_mlr

###########################################################################################
df2<-read.csv("data2 potato.csv",sep = ',')
names(df2)#features
attach(df2)
df2 <- df2[,-1]#dropping the years column
head(df2)#view first 6 rows
dim(df2)#dimension of our dataset

#partioning the data 70% training 30% testing
df3<-sort(sample(nrow(df2), nrow(df2)*0.7, replace = F))
train <- df2[df3,]
test <- df2[-df3,]
dim(train)
dim(test)
#looking for the correlation among variables
library(correlation)
cor(df)

#we fit the linear regression model with the training data
mlr<-lm( Potato.yield~., data=train)
summary(mlr)
#lets predict using the linear regression model
library(Metrics)
mlr_pred<-predict(mlr, test)
mlr_pred
predplot(mlr)
abline(0,1,col='brown')
# Calculate performance metrics for MLR model
rmse(actual = test$Potato.yield,predicted = as.numeric(lm_pred))#the lower the error the better
r2_mlr <- cor(lm_pred, test$Potato.yield)^2
r2_mlr
mse_mlr <- mean((lm_pred - test$Potato.yield)^2)
mse_mlr
#################################################################################################
df4<-read.csv("data3 orange.csv",sep = ',')
names(df4)#features
attach(df4)
df4 <- df4[,-1]#dropping the years column
head(df4)#view first 6 rows
dim(df4)#dimension of our dataset

#partioning the data 70% training 30% testing
df5<-sort(sample(nrow(df4), nrow(df4)*0.7, replace = F))
train <- df4[df5,]
test <- df4[-df5,]
dim(train)
dim(test)
#looking for the correlation among variables
library(correlation)
cor(df)

#we fit the linear regression model with the training data
mlr<-lm(Orange.yield ~., data=train)
summary(mlr)
#lets predict using the linear regression model
library(Metrics)
mlr_pred<-predict(mlr, test)
mlr_pred
predplot(mlr)
abline(0,1,col='orange')

# Calculate performance metrics for MLR model
rmse(actual = test$Orange.yield,predicted = as.numeric(mlr_pred))#the lower the error the better
r2_mlr <- cor(lm_pred, test$Orange.yield)^2
r2_mlr
mse_mlr <- mean((lm_pred - test$Orange.yield)^2)
mse_mlr
#############################################################################################
df6<-read.csv("data5 tomato.csv",sep = ',')
names(df6)#features
attach(df6)
df6 <- df6[,-1]#dropping the years column
head(df6)#view first 6 rows
dim(df6)#dimension of our dataset

#partioning the data 70% training 30% testing
df7<-sort(sample(nrow(df6), nrow(df6)*0.7, replace = F))
train <- df6[df7,]
test <- df6[-df7,]
dim(train)
dim(test)
#looking for the correlation among variables
library(correlation)
cor(df)

#we fit the linear regression model with the training data
mlr<-lm( Tomato.yield~., data=train)
summary(mlr)
#lets predict using the linear regression model
library(Metrics)
mlr_pred<-predict(mlr, test)
mlr_pred
predplot(mlr)
abline(0,1,col='red')

# Calculate performance metrics for MLR model
rmse(actual = test$Tomato.yield,predicted = as.numeric(lm_pred))#the lower the error the better
r2_mlr <- cor(lm_pred, test$Tomato.yield)^2
r2_mlr
mse_mlr <- mean((lm_pred - test$Tomato.yield)^2)
mse_mlr
#############################################################################################
df8<-read.csv("data6 wheat.csv",sep = ',')
names(df8)#features
attach(df8)
df8 <- df8[,-1]#dropping the years column
head(df8)#view first 6 rows
dim(df8)#dimension of our dataset

#partioning the data 70% training 30% testing
df9<-sort(sample(nrow(df8), nrow(df8)*0.8, replace = F))
train <- df8[df9,]
test <- df8[-df9,]
dim(train)
dim(test)
#looking for the correlation among variables
library(correlation)
library(caret)
cor(df)
#we fit the linear regression model with the training data
mlr<-lm(Wheat.yield~., data=train)
summary(mlr)
#lets predict using the linear regression model
library(Metrics)
mlr_pred<-predict(mlr, test)
mlr_pred
predplot(mlr)
abline(0,1,col='blue')

# Calculate performance metrics for MLR model
rmse(actual = test$Wheat.yield,predicted = as.numeric(mlr_pred))#the lower the error the better
r2_mlr <- cor(mlr_pred, test$Wheat.yield)^2
r2_mlr
mse_mlr <- mean((mlr_pred - test$Wheat.yield)^2)
mse_mlr
corr_coef <- cor(mlr_pred, test$Wheat.yield)
print(corr_coef)
###############################################################################################
## DEVELOPING A PCR MODEL

da<-read.csv("data1 maize.csv",sep = ',')
names(da)#features
attach(da)
da <- da[,-1]#dropping the years column
head(da)#view first 6 rows
dim(da)#dimension of our dataset

#partioning the data 70% training 30% testing
db<-sort(sample(nrow(da), nrow(da)*0.7, replace = F))
train <- da[db,]
test <- da[-db,]
dim(train)
dim(test)

# Fit principal component regression model
library(pls)
pcr_model <- pcr(Maize.yield ~., data=train, scale=TRUE, validation="CV")
summary(pcr_model)#the less the better which tells us the number of principal components to use
#In the training we can see the variance explained by the principal components

validationplot(pcr_model, val.type="RMSEP", cex.axis=0.7)
axis(side = 1, at = c(8), cex.axis=0.7)
abline(v = 8, col = "blue", lty = 3)

validationplot(pcr_model, val.type="R2", cex.axis=0.7)
axis(side = 1, at = c(8), cex.axis=0.7)
abline(v = 8, col = "blue", lty = 3)

plot(pcr_model,'validation',val.type='R2',cex.axis=0.7)
axis(side = 1, at = c(8), cex.axis=0.7)
abline(v = 8, col = "blue", lty = 3)


#predicting based on the principal component regression
pcr_pred<-predict(pcr_model,test,ncomp = 2)#we are using 4 PCs
pcr_pred

predplot(pcr_model)#prediction plot
abline(0,1, col = 'green')
#Calculate root mean squared error (RMSE)
rmse(actual = test$Maize.yield,predicted = as.numeric(pcr_pred))#calculating root mean squared error

# Calculate R-squared value
r2 <- cor(pcr_pred, test$Maize.yield)^2
# Calculate mean squared error (MSE)
mse <- mean((pcr_pred - test$Maize.yield)^2)

############################################################################################
dc<-read.csv("data2 potato.csv",sep = ',')
names(dc)#features
attach(dc)
dc <- dc[,-1]#dropping the years column
head(dc)#view first 6 rows
dim(dc)#dimension of our dataset

#partioning the data 70% training 30% testing
dd<-sort(sample(nrow(dc), nrow(dc)*0.7, replace = F))
train <- dc[dd,]
test <- dc[-dd,]
dim(train)
dim(test)

# Fit principal component regression model
library(pls)
pcr_model <- pcr(Potato.yield~., data=train, scale=TRUE, validation="CV")
summary(pcr_model)#the less the better which tells us the number of principal components to use
#In the training we can see the variance explained by the principal components

validationplot(pcr_model, val.type="RMSEP", cex.axis=0.7)
axis(side = 1, at = c(8), cex.axis=0.7)
abline(v = 8, col = "blue", lty = 3)

validationplot(pcr_model, val.type="R2", cex.axis=0.7)
axis(side = 1, at = c(8), cex.axis=0.7)
abline(v = 8, col = "blue", lty = 3)

plot(pcr_model,'validation',val.type='R2')

#predicting based on the principal component regression
pcr_pred<-predict(pcr_model,test,ncomp = 6)#we are using 4 PCs
pcr_pred
predplot(pcr_model)#prediction plot
abline(0,1, col = 'brown')

#Calculate root mean squared error (RMSE)
rmse(actual = test$Potato.yield,predicted = as.numeric(pcr_pred))
# Calculate R-squared value
r2 <- cor(pcr_pred, test$Potato.yield)^2
# Calculate mean squared error (MSE)
mse <- mean((pcr_pred - test$Potato.yield)^2)

############################################################################################
de<-read.csv("data3 orange.csv",sep = ',')
names(de)#features
attach(de)
de <- de[,-1]#dropping the years column
head(de)#view first 6 rows
dim(de)#dimension of our dataset

#partioning the data 70% training 30% testing
df<-sort(sample(nrow(de), nrow(de)*0.7, replace = F))
train <- de[df,]
test <- de[-df,]
dim(train)
dim(test)

#we perform PCR model
library(pls)
pcr_model <- pcr(Orange.yield~., data=train, scale=TRUE, validation="CV")
summary(pcr_model)#the less the better which tells us the number of principal components to use
#In the training we can see the variance explained by the principal components

validationplot(pcr_model, val.type="RMSEP", cex.axis=0.7)
axis(side = 1, at = c(8), cex.axis=0.7)
abline(v = 8, col = "blue", lty = 3)

validationplot(pcr_model, val.type="R2", cex.axis=0.7)
axis(side = 1, at = c(8), cex.axis=0.7)
abline(v = 8, col = "blue", lty = 3)

plot(pcr_model,'validation',val.type='R2')

#predicting based on the principal component regression
pcr_pred<-predict(pcr_model,test,ncomp = 2)#we are using 4 PCs
pcr_pred
predplot(pcr_model)#prediction plot
abline(0,1, col = 'orange')

#Calculate root mean squared error (RMSE)
rmse(actual = test$Orange.yield,predicted = as.numeric(pcr_pred))
# Calculate R-squared value
r2 <- cor(pcr_pred, test$Orange.yield)^2
# Calculate mean squared error (MSE)
mse <- mean((pcr_pred - test$Orange.yield)^2)


###########################################################################################
dg<-read.csv("data5 tomato.csv",sep = ',')
names(dg)#features
attach(dg)
dg <- dg[,-1]#dropping the years column
head(dg)#view first 6 rows
dim(dg)#dimension of our dataset

#partioning the data 70% training 30% testing
dh<-sort(sample(nrow(dg), nrow(dg)*0.7, replace = F))
train <- dg[dh,]
test <- dg[-dh,]
dim(train)
dim(test)

#we perform PCR model
library(pls)
pcr_model <- pcr(Tomato.yield~., data=train, scale=TRUE, validation="CV")
summary(pcr_model)#the less the better which tells us the number of principal components to use
#In the training we can see the variance explained by the principal components

validationplot(pcr_model, val.type="RMSEP", cex.axis=0.7)
axis(side = 1, at = c(8), cex.axis=0.7)
abline(v = 8, col = "blue", lty = 3)

validationplot(pcr_model, val.type="R2", cex.axis=0.7)
axis(side = 1, at = c(8), cex.axis=0.7)
abline(v = 8, col = "blue", lty = 3)

plot(pcr_model,'validation',val.type='R2')

#predicting based on the principal component regression
pcr_pred<-predict(pcr_model,test,ncomp = 3)#we are using 4 PCs
pcr_pred
predplot(pcr_model)#prediction plot
abline(0,1, col = 'red')

#Calculate root mean squared error (RMSE)
rmse(actual = test$Tomato.yield,predicted = as.numeric(pcr_pred))
# Calculate R-squared value
r2 <- cor(pcr_pred, test$Tomato.yield)^2
# Calculate mean squared error (MSE)
mse <- mean((pcr_pred - test$Tomato.yield)^2)




############################################################################################
dj<-read.csv("data6 wheat.csv",sep = ',')
names(dj)#features
attach(dj)
dj <- dj[,-1]#dropping the years column
head(dj)#view first 6 rows
dim(dj)#dimension of our dataset

#partioning the data 70% training 30% testing
dk<-sort(sample(nrow(dj), nrow(dj)*0.7, replace = F))
train <- dj[dk,]
test <- dj[-dk,]
dim(train)
dim(test)

#we perform PCR model
library(pls)
pcr_model <- pcr(Wheat.yield~., data=train, scale=TRUE, validation="CV")
summary(pcr_model)#the less the better which tells us the number of principal components to use
#In the training we can see the variance explained by the principal components
validationplot(pcr_model, val.type="RMSEP", cex.axis=0.7)
axis(side = 1, at = c(8), cex.axis=0.7)
abline(v = 8, col = "blue", lty = 3)

validationplot(pcr_model, val.type="R2", cex.axis=0.7)
axis(side = 1, at = c(8), cex.axis=0.7)
abline(v = 8, col = "blue", lty = 3)

plot(pcr_model,'validation',val.type='R2')

#predicting based on the principal component regression
pcr_pred<-predict(pcr_model,test,ncomp = 5)#we are using 4 PCs
pcr_pred
predplot(pcr_model)#prediction plot
abline(0,1,col="blue")

#Calculate root mean squared error (RMSE)
rmse(actual = test$Wheat.yield,predicted = as.numeric(pcr_pred))
# Calculate R-squared value
r2 <- cor(pcr_pred, test$Wheat.yield)^2
# Calculate mean squared error (MSE)
mse <- mean((pcr_pred - test$Wheat.yield)^2)

# Calculate correlation coefficient between predicted and actual values
corr_coef <- cor(pcr_pred, test$Wheat.yield)
# Print correlation coefficient
print(corr_coef)







# Create scatter plot of actual versus predicted values
plot(test$Wheat.yield, pcr_pred, xlab = "Actual Values", ylab = "Predicted Values")
abline(0, 1, col = "red") # add a 45-degree line for reference


