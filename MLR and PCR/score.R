rm(list=ls())#clearing the environment
setwd("~/Documents/Essay with Prof Babatunde/prac/MLR and PCR")
getwd()
#load pls
library(pls)
#importing the data
df<-read.csv("detrended_climate1.csv",sep = ',')
# Scale the columns from the second column to the last column
df[, 2:ncol(df)] <- scale(df[, 2:ncol(df)])
boxplot(df[,5])

names(df)#features
attach(df)
df <- df[,-1]#dropping the years column
head(df)#view first 6 rows
dim(df)#dimension of our dataset
str(df)
sum(is.null(df))#checking for the missing values
# outliers<-boxplot(df[,2],plot=FALSE)$out
# df<-df[-which(df[,2] %in% outliers),]
par(mfrow = c(1, 2))
set.seed(100)
#partitioning the data 70% training 30% testing
df1<-sort(sample(nrow(df), nrow(df)*0.7, replace = F))
train <- df[df1,]
test <- df[-df1,]
dim(train)
dim(test)


#looking for the correlation among variables
library(correlation)
correlation(df)
#cor(df)

res <- cor(train, method="pearson")
corrplot::corrplot(res, method= "color", order = "hclust", tl.pos = 'n')
#we can observe that the darker colors are more meaning there is correlation among variables

# Fit MLR model 
# Fit the linear regression model
mlr <- lm(PC1 ~ ., data = train)

# Print the model summary
summary(mlr)

# Predict using the linear regression model
mlr_pred <- predict(mlr, train)
mlr_pred
par(mfrow = c(1, 2))
# # Create a square predplot
# library(Metrics)
# predplot(mlr)
# abline(a = 0, b = 1, col = "green")

# set.seed(110)
# # Calculate performance metrics for MLR model
# rmse(actual = test$Maize.yield,predicted = as.numeric(mlr_pred))#the lower the error the better
# r2_mlr <- cor(mlr_pred, test$Maize.yield)^2
# r2_mlr
# mse_mlr <- mean((mlr_pred - test$Maize.yield)^2)
# mse_mlr
# # Calculate correlation coefficient between predicted and actual values
# corr_coef <- cor(mlr_pred, test$Maize.yield)
# corr_coef



#predplot(mlr)

set.seed(110)
# Get the predicted and actual values for the scatter plot
pred <- as.numeric(mlr_pred)
actual <- train$PC1

# Fit a linear model and get the intercept and slope of the line of best fit
model <- lm(actual ~ pred)
intercept <- coef(model)[1]
slope <- coef(model)[2]

# Set the aspect ratio to 1 to create a square plot
plot(pred, actual, asp=1,title("PC1"))

# Add the line of best fit to the plot
abline(a=intercept, b=slope, col="red")

# Calculate performance metrics for MLR model
rmse(actual = train$PC1,predicted = as.numeric(mlr_pred))#the lower the error the better
r2_mlr <- cor(mlr_pred, train$PC1)^2
r2_mlr
mse_mlr <- mean((mlr_pred - train$PC1)^2)
mse_mlr
# Calculate correlation coefficient between predicted and actual values
corr_coef <- cor(mlr_pred, train$PC1)
corr_coef







###########################################################################################
df2<-read.csv("detrended_climate2.csv",sep = ',')
#df2[, 2:ncol(df2)] <- apply(df2[, 2:ncol(df2)], 2, function(x) (x - mean(x))/mean(x))*100
names(df2)#features
attach(df2)
df2 <- df2[,-1]#dropping the years column
head(df2)#view first 6 rows
dim(df2)#dimension of our dataset
df2[, 2:ncol(df2)] <- scale(df2[, 2:ncol(df2)])
par(mfrow = c(1, 2))

# outliers<-boxplot(df2[,2],plot=FALSE)$out
# df2<-df2[-which(df2[,2] %in% outliers),]

set.seed(120)
#partioning the data 70% training 30% testing
df3<-sort(sample(nrow(df2), nrow(df2)*0.7, replace = F))
train <- df2[df3,]
test <- df2[-df3,]
dim(train)
dim(test)
#looking for the correlation among variables
library(correlation)
cor(df)

# Fit MLR model
mlr<-lm( PC2~., data=train)
summary(mlr)
#lets predict using the linear regression model
library(Metrics)
mlr_pred<-predict(mlr, train)
mlr_pred
# predplot(mlr)
# abline(0,1,col="brown")

set.seed(130)

# Get the predicted and actual values for the scatter plot
pred <- as.numeric(mlr_pred)
actual <- train$PC2

# Fit a linear model and get the intercept and slope of the line of best fit
model <- lm(actual ~ pred)
intercept <- coef(model)[1]
slope <- coef(model)[2]

# Set the aspect ratio to 1 to create a square plot
plot(pred, actual, asp=1,title("PC2"))

# Add the line of best fit to the plot
abline(a=intercept, b=slope, col="red")

# Calculate performance metrics for MLR model
rmse(actual = train$PC2,predicted = as.numeric(mlr_pred))#the lower the error the better
r2_mlr <- cor(mlr_pred, train$PC2)^2
r2_mlr
mse_mlr <- mean((mlr_pred - train$PC2)^2)
mse_mlr
# Calculate correlation coefficient between predicted and actual values
corr_coef <- cor(mlr_pred, train$PC2)
corr_coef





# # Calculate performance metrics for MLR model
# rmse(actual = test$Potato.yield,predicted = as.numeric(mlr_pred))#the lower the error the better
# r2_mlr <- cor(mlr_pred, test$Potato.yield)^2
# r2_mlr
# mse_mlr <- mean((mlr_pred - test$Potato.yield)^2)
# mse_mlr
# # Calculate correlation coefficient between predicted and actual values
# corr_coef <- cor(mlr_pred, test$Potato.yield)
# corr_coef
#################################################################################################
##################################################################################################
##################################################################################################
##DEVELOPING A PCR MODEL
#we fit a PCR model on maize yield
da<-read.csv("detrended_climate1.csv",sep = ',')
# Scale the columns from the second column to the last column
#da[, 2:ncol(da)] <- scale(da[, 2:ncol(da)])

attach(da)
da <- da[,-1]#dropping the years column
head(da)#view first 6 rows
dim(da)#dimension of our dataset
str(da)
sum(is.null(da))#checking for the missing values
da[, 2:ncol(da)] <- scale(da[, 2:ncol(da)])
par(mfrow = c(1, 2))

set.seed(260)
#partitioning the data 70% training 30% testing
dfb<-sort(sample(nrow(da), nrow(da)*0.7, replace = F))
train <- da[dfb,]
test <- da[-dfb,]
dim(train)
dim(test)

library(pls)
pcr_model <- pcr(PC1~., data=train, scale=TRUE, validation="CV")
summary(pcr_model)#the less the better which tells us the number of principal components to use
#In the training we can see the variance explained by the principal components

set.seed(200)
validationplot(pcr_model, val.type="RMSEP", cex.axis=0.7)
axis(side = 1, at = c(8), cex.axis=0.7)
abline(v = 8, col = "blue", lty = 3)

validationplot(pcr_model, val.type="R2", cex.axis=0.7)
axis(side = 1, at = c(8), cex.axis=0.7)
abline(v = 8, col = "blue", lty = 3)

#predicting based on the principal component regression
pcr_pred<-predict(pcr_model,train,ncomp = 8)#we are using 4 PCs
pcr_pred

# predplot(pcr_model)#prediction plot
# abline(0,1, col = 'red')

set.seed(230)
# Get the predicted and actual values for the scatter plot
pred <- as.numeric(pcr_pred)
actual <- train$PC1
par(mfrow = c(1, 2))
# Fit a linear model and get the intercept and slope of the line of best fit
model <- lm(actual ~ pred)
intercept <- coef(model)[1]
slope <- coef(model)[2]

# Set the aspect ratio to 1 to create a square plot
plot(pred, actual, asp=1,title("PC1"))

# Add the line of best fit to the plot
abline(a=intercept, b=slope, col="red")

# Calculate performance metrics for MLR model
rmse(actual = train$PC1,predicted = as.numeric(pcr_pred))#the lower the error the better
r2_mlr <- cor(pcr_pred, train$PC1)^2
r2_mlr
mse_mlr <- mean((pcr_pred - train$PC1)^2)
mse_mlr
# Calculate correlation coefficient between predicted and actual values
corr_coef <- cor(pcr_pred, train$PC1)
corr_coef




# #calculate root mean squared error
# rmse(actual = test$Maize.yield,predicted = as.numeric(pcr_pred))
# # Calculate R-squared value
# r2 <- cor(pcr_pred, test$Maize.yield)^2
# r2
# # Calculate mean squared error (MSE)
# mse <- mean((pcr_pred - test$Maize.yield)^2)
# mse
# # Calculate correlation coefficient between predicted and actual values
# corr_coef <- cor(pcr_pred, test$Maize.yield)
# corr_coef
##############################################################################################
#we fit a PCR model on potato yield
dc<-read.csv("detrended_climate2.csv",sep = ',')
#dc[, 2:ncol(dc)] <- apply(dc[, 2:ncol(dc)], 2, function(x) (x - mean(x))/mean(x))*100
names(dc)#features
attach(dc)
dc <- dc[,-1]#dropping the years column
head(dc)#view first 6 rows
dim(dc)#dimension of our dataset
str(dc)
sum(is.null(dc))#checking for the missing values
dc[, 2:ncol(dc)] <- scale(dc[, 2:ncol(dc)])
par(mfrow = c(1, 2))

set.seed(123)
#partioning the data 70% training 30% testing
dd<-sort(sample(nrow(dc), nrow(dc)*0.7, replace = F))
train <- dc[dd,]
test <- dc[-dd,]
dim(train)
dim(test)

library(pls)
pcr_model <- pcr(PC2~., data=train, scale=TRUE, validation="CV")
summary(pcr_model)#the less the better which tells us the number of principal components to use
#In the training we can see the variance explained by the principal components

set.seed(300)
validationplot(pcr_model, val.type="RMSEP", cex.axis=0.7)
axis(side = 1, at = c(8), cex.axis=0.7)
abline(v = 8, col = "blue", lty = 3)

validationplot(pcr_model, val.type="R2", cex.axis=0.7)
axis(side = 1, at = c(8), cex.axis=0.7)
abline(v = 8, col = "blue", lty = 3)

#predicting based on the principal component regression
pcr_pred<-predict(pcr_model,train,ncomp = 7)#we are using 4 PCs
pcr_pred

# predplot(pcr_model)#prediction plot
# abline(0,1, col = 'red')

set.seed(302)
# Get the predicted and actual values for the scatter plot
pred <- as.numeric(pcr_pred)
actual <- train$PC2
# Fit a linear model and get the intercept and slope of the line of best fit
model <- lm(actual ~ pred)
intercept <- coef(model)[1]
slope <- coef(model)[2]

# Set the aspect ratio to 1 to create a square plot
plot(pred, actual, asp=1,title("PC2"))

# Add the line of best fit to the plot
abline(a=intercept, b=slope, col="red")

# Calculate performance metrics for MLR model
rmse(actual = train$PC2,predicted = as.numeric(pcr_pred))#the lower the error the better
r2_mlr <- cor(pcr_pred, train$PC2)^2
r2_mlr
mse_mlr <- mean((pcr_pred - train$PC2)^2)
mse_mlr
# Calculate correlation coefficient between predicted and actual values
corr_coef <- cor(pcr_pred, train$PC2)
corr_coef


# #calculate root mean squared error
# rmse(actual = test$Potato.yield,predicted = as.numeric(pcr_pred))
# # Calculate R-squared value
# r2 <- cor(pcr_pred, test$Potato.yield)^2
# r2
# # Calculate mean squared error (MSE)
# mse <- mean((pcr_pred - test$Potato.yield)^2)
# mse
# # Calculate correlation coefficient between predicted and actual values
# corr_coef <- cor(mlr_pred, test$Potato.yield)
# corr_coef
############################################################################################
#we fit a PCR model on orange yield
de<-read.csv("data3 Orange.csv",sep = ',')
#de[, 2:ncol(de)] <- apply(de[, 2:ncol(de)], 2, function(x) (x - mean(x))/mean(x))*100
names(de)#features
attach(de)
de <- de[,-1]#dropping the years column
head(de)#view first 6 rows
dim(de)#dimension of our dataset
str(de)
sum(is.null(de))#checking for the missing values
de[, 2:ncol(de)] <- scale(de[, 2:ncol(de)])
par(mfrow = c(1, 2))

set.seed(321)
#partioning the data 70% training 30% testing
df<-sort(sample(nrow(de), nrow(de)*0.7, replace = F))
train <- de[df,]
test <- de[-df,]
dim(train)
dim(test)

library(pls)
pcr_model <- pcr(Orange.yield~., data=train, scale=TRUE, validation="CV")
summary(pcr_model)#the less the better which tells us the number of principal components to use
#In the training we can see the variance explained by the principal components

set.seed(330)
validationplot(pcr_model, val.type="RMSEP", cex.axis=0.7)
axis(side = 1, at = c(8), cex.axis=0.7)
abline(v = 8, col = "blue", lty = 3)

validationplot(pcr_model, val.type="R2", cex.axis=0.7)
axis(side = 1, at = c(8), cex.axis=0.7)
abline(v = 8, col = "blue", lty = 3)

#predicting based on the principal component regression
pcr_pred<-predict(pcr_model,train,ncomp = 1)#we are using 4 PCs
pcr_pred

# predplot(pcr_model)#prediction plot
# abline(0,1, col = 'red')

set.seed(336)
# Get the predicted and actual values for the scatter plot
pred <- as.numeric(pcr_pred)
actual <- train$Orange.yield
# Fit a linear model and get the intercept and slope of the line of best fit
model <- lm(actual ~ pred)
intercept <- coef(model)[1]
slope <- coef(model)[2]

# Set the aspect ratio to 1 to create a square plot
plot(pred, actual, asp=1,title("orange yield"))

# Add the line of best fit to the plot
abline(a=intercept, b=slope, col="red")

# Calculate performance metrics for MLR model
rmse(actual = train$Orange.yield,predicted = as.numeric(pcr_pred))#the lower the error the better
r2_mlr <- cor(pcr_pred, train$Orange.yield)^2
r2_mlr
mse_mlr <- mean((pcr_pred - train$Orange.yield)^2)
mse_mlr
# Calculate correlation coefficient between predicted and actual values
corr_coef <- cor(pcr_pred, train$Orange.yield)
corr_coef

# #calculate root mean squared error
# rmse(actual = test$Orange.yield,predicted = as.numeric(pcr_pred))
# # Calculate R-squared value
# r2 <- cor(pcr_pred, test$Orange.yield)^2
# r2
# # Calculate mean squared error (MSE)
# mse <- mean((pcr_pred - test$Orange.yield)^2)
# mse
# # Calculate correlation coefficient between predicted and actual values
# corr_coef <- cor(mlr_pred, test$Orange.yield)
# corr_coef
##############################################################################################
#we fit a PCR model on totato yield
dg<-read.csv("data4 Tomato.csv",sep = ',')
#dg[, 2:ncol(dg)] <- apply(dg[, 2:ncol(dg)], 2, function(x) (x - mean(x))/mean(x))*100
names(dg)#features
attach(dg)
dg <- dg[,-1]#dropping the years column
head(dg)#view first 6 rows
dim(dg)#dimension of our dataset
str(dg)
sum(is.null(dg))#checking for the missing values
dg[, 2:ncol(dg)] <- scale(dg[, 2:ncol(dg)])
par(mfrow = c(1, 2))

set.seed(123)
#partitioning the data 70% training 30% testing
dh<-sort(sample(nrow(dg), nrow(dg)*0.7, replace = F))
train <- dg[dh,]
test <- dg[-dh,]
dim(train)
dim(test)

library(pls)
pcr_model <- pcr(Tomato.yield~., data=train, scale=TRUE, validation="CV")
summary(pcr_model)#the less the better which tells us the number of principal components to use
#In the training we can see the variance explained by the principal components

set.seed(123)
validationplot(pcr_model, val.type="RMSEP", cex.axis=0.7)
axis(side = 1, at = c(8), cex.axis=0.7)
abline(v = 8, col = "blue", lty = 3)

validationplot(pcr_model, val.type="R2", cex.axis=0.7)
axis(side = 1, at = c(8), cex.axis=0.7)
abline(v = 8, col = "blue", lty = 3)

#predicting based on the principal component regression
pcr_pred<-predict(pcr_model,train,ncomp = 1)#we are using 4 PCs
pcr_pred

# predplot(pcr_model)#prediction plot
# abline(0,1, col = 'red')

set.seed(123)
# Get the predicted and actual values for the scatter plot
pred <- as.numeric(pcr_pred)
actual <- train$Tomato.yield
# Fit a linear model and get the intercept and slope of the line of best fit
model <- lm(actual ~ pred)
intercept <- coef(model)[1]
slope <- coef(model)[2]

# Set the aspect ratio to 1 to create a square plot
plot(pred, actual, asp=1,title("tomato yield"))

# Add the line of best fit to the plot
abline(a=intercept, b=slope, col="red")

# Calculate performance metrics for MLR model
rmse(actual = train$Tomato.yield,predicted = as.numeric(pcr_pred))#the lower the error the better
r2_mlr <- cor(pcr_pred, train$Tomato.yield)^2
r2_mlr
mse_mlr <- mean((pcr_pred - train$Tomato.yield)^2)
mse_mlr
# Calculate correlation coefficient between predicted and actual values
corr_coef <- cor(pcr_pred, train$Tomato.yield)
corr_coef
# #calculate root mean squared error
# rmse(actual = test$Tomato.yield,predicted = as.numeric(pcr_pred))
# # Calculate R-squared value
# r2 <- cor(pcr_pred, test$Tomato.yield)^2
# r2
# # Calculate mean squared error (MSE)
# mse <- mean((pcr_pred - test$Tomato.yield)^2)
# mse
# # Calculate correlation coefficient between predicted and actual values
# corr_coef <- cor(mlr_pred, test$Tomato.yield)
# corr_coef
#####################################################################################################
#we fit a PCR model on wheat yield
dj<-read.csv("data5 Wheat.csv",sep = ',')
#dj[, 2:ncol(dj)] <- apply(dj[, 2:ncol(dj)], 2, function(x) (x - mean(x))/mean(x))*100
names(dj)#features
attach(dj)
dj <- dj[,-1]#dropping the years column
head(dj)#view first 6 rows
dim(dj)#dimension of our dataset
str(dj)
sum(is.null(dj))#checking for the missing values
dj[, 2:ncol(dj)] <- scale(dj[, 2:ncol(dj)])
par(mfrow = c(1, 2))

set.seed(123)
#partioning the data 70% training 30% testing
dk<-sort(sample(nrow(dj), nrow(dj)*0.7, replace = F))
train <- dj[dk,]
test <- dj[-dk,]
dim(train)
dim(test)

library(pls)
pcr_model <- pcr(Wheat.yield~., data=train, scale=TRUE, validation="CV")
summary(pcr_model)#the less the better which tells us the number of principal components to use
#In the training we can see the variance explained by the principal components

set.seed(123)
validationplot(pcr_model, val.type="RMSEP", cex.axis=0.7)
axis(side = 1, at = c(8), cex.axis=0.7)
abline(v = 8, col = "blue", lty = 3)

validationplot(pcr_model, val.type="R2", cex.axis=0.7)
axis(side = 1, at = c(8), cex.axis=0.7)
abline(v = 8, col = "blue", lty = 3)

#predicting based on the principal component regression
pcr_pred<-predict(pcr_model,train,ncomp = 6)#we are using 4 PCs
pcr_pred

# predplot(pcr_model)#prediction plot
# abline(0,1, col = 'red')

set.seed(123)
# Get the predicted and actual values for the scatter plot
pred <- as.numeric(pcr_pred)
actual <- train$Wheat.yield
# Fit a linear model and get the intercept and slope of the line of best fit
model <- lm(actual ~ pred)
intercept <- coef(model)[1]
slope <- coef(model)[2]

# Set the aspect ratio to 1 to create a square plot
plot(pred, actual, asp=1,title("wheat yield"))

# Add the line of best fit to the plot
abline(a=intercept, b=slope, col="red")

# Calculate performance metrics for MLR model
rmse(actual = train$Wheat.yield,predicted = as.numeric(pcr_pred))#the lower the error the better
r2_mlr <- cor(pcr_pred, train$Wheat.yield)^2
r2_mlr
mse_mlr <- mean((pcr_pred - train$Wheat.yield)^2)
mse_mlr
# Calculate correlation coefficient between predicted and actual values
corr_coef <- cor(pcr_pred, train$Wheat.yield)
corr_coef



# #calculate root mean squared error
# rmse(actual = test$Wheat.yield,predicted = as.numeric(pcr_pred))
# # Calculate R-squared value
# r2 <- cor(pcr_pred, test$Wheat.yield)^2
# r2
# # Calculate mean squared error (MSE)
# mse <- mean((pcr_pred - test$Wheat.yield)^2)
# mse
# # Calculate correlation coefficient between predicted and actual values
# corr_coef <- cor(mlr_pred, test$Wheat.yield)
# corr_coef
################################################################################
#confirmation of the principal components
library(caret)

# Create a range of principal components to test
n_components <- 1:10

# Create a PCR model with 10-fold cross-validation
ctrl <- trainControl(method = "cv", number = 10)
pcr_model <- train(Maize.yield~ ., train, method = "pcr", tuneGrid = data.frame(ncomp = n_components), trControl = ctrl)

# Plot the RMSE values
plot(pcr_model$results$ncomp, pcr_model$results$RMSE, type = "b", xlab = "Number of principal components", ylab = "RMSE", main = "PCR RMSE vs. number of principal components")



# Create a range of principal components to test
n_components <- 1:10

# Create a PCR model with 10-fold cross-validation
ctrl <- trainControl(method = "cv", number = 10)
pcr_model <- train(Maize.yield ~ ., train, method = "pcr", tuneGrid = data.frame(ncomp = n_components), trControl = ctrl, metric = "R2")

# Plot the R-squared values
plot(pcr_model$results$ncomp, pcr_model$results$Rsquared, type = "b", xlab = "Number of principal components", ylab = "R-squared", main = "PCR R-squared vs. number of principal components")



