library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(MASS)
library(gmodels)
library(RVAideMemoire)
library(psych)
library(corrplot)
library(tidymodels)
library(caret)
library(vip)

setwd("/Users/Project_path")

#Reading File
fpl <- read.csv(file = "fpl1.csv")

fpl_test <- fpl[, c(5:6,8:9,12:13,17,20:23,25:34,36:37)]

dim(fpl)
View(fpl)
View(fpl_test)
#Display the column names
colnames(fpl)

#Check Missing Data (In this case no missing data)
sum(is.na(fpl))

#Solve for the error

sapply(lapply(fpl, unique), length)


#Regularized Regression

x=model.matrix(total_points ~ ., data=fpl_test)
y=fpl_test$total_points

library(glmnet)

#plot variable feature coefficients against shrinkage parameter lambda

glmmod <- glmnet(x, y, alpha = 0)
plot(glmmod, xvar="lambda")
grid()

#Lasso Regression

cv.lasso <- cv.glmnet(x,y, typemeasure="mse", alpha=1)
cv.lasso

ls(cv.lasso)

plot(cv.lasso)

Lambda.best <- cv.lasso$lambda.min

predict(cv.lasso, s = Lambda.best, type = "coefficients")

fit<-cv.lasso$glmnet.fit
fit

#Linear Regression

null = lm(total_points ~ 1-total_points, data=fpl_test)
null

full = lm(total_points ~ .-total_points, data=fpl_test)
full

#Forward Regression
train_Forward = step(null, scope = list(lower=null, upper=full), direction="forward")
summary(train_Forward)

#Backward Regression
train_Backward = step(full, direction="backward")
summary(train_Backward)

#Stepwise Regression
train_Step = step(null, scope = list(upper=full), direction="both")
summary(train_Step)

#Using Manual Multiple Linear Regression

#Create Initial Linear Regression Model with Enter Method

model1 <- lm(total_points ~ ., data=fpl_test)
model1

model2 <- lm(total_points ~ ., data=fpl_test)
model2

cor(fpl_test)

#Check VIF
library(DescTools)
VIF(model2)

#Diagnostic Plots for Model Fit
par(mfrow = c(2, 2))
plot(model1)

library(ggfortify)
autoplot(model1)

par(mfrow = c(1, 1))
