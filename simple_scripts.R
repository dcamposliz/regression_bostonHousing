# loading libraries

library(usdm) # for testing collinearity
library(car) # for testing outliers
library(MASS) # for testing studentized residuals
library(DAAG) # for cross validation of model
library(lmtest) # for checking homoskedasticity / heteroskedasticity
library(astsa) # for checking autocorrelation

# loading data

setwd("/home/dc/myProjects/inertia7_projects/regression_bostonHousing")

boston <- read.csv("data/boston_housing_data.csv", header = TRUE, sep = ",")
attach(boston)
str(boston)
CHAS <- as.factor(CHAS)
boston.df <- data.frame(CRIM, ZN, INDUS, CHAS, NOX, RM, AGE, DIS, RAD, TAX, PTRATIO, B, LSTAT, MEDV)

# exploratory analysis

str(boston.df)
pairs(boston.df)

# model 1

  fit1 <- lm(MEDV ~ ., data = boston.df)
  summary(fit1)
  vif(fit1)
  
# model 2
  
  # for this iteration of the model we are removing TAX because of its high collinearity with another one of the variables in the model

  fit2 <- update(fit1, ~ . - TAX)
  summary(fit2)
  vif(fit2)
  
# model 3
  
  # for this iteration of the model we remove AGE, INDUS, and RAD because of their statistical non-significance in the model, as per their p-values
  
  fit3 <- update(fit2, ~ . - AGE - INDUS - RAD)
  summary(fit3)
  vif(fit3)
  
  par(mfrow = c(2,2))
  plot(fit3)
  
# checking for outliers
  
outlierTest(fit3, cutoff = Inf, n.max = 15)
  
  boston.df <- boston.df[-c(369, 372, 373, 370, 413, 365, 371, 366, 187, 375),]
  
  
  fit4 <- lm(MEDV ~ . - TAX - AGE - INDUS - RAD, data = boston.df)
  summary(fit4)
  vif(fit4)
  
  par(mfrow = c(2,2))
  plot(fit4)
  
  # checking for outliers
  # looking for normal distribution of our studentized residuals
  studentizedResiduals <- studres(fit4)
  par(mfrow = c(1,1))
  hist(studentizedResiduals, freq = FALSE, main = "Distribution of Studentized Residuals")
  xfit <- seq(min(studentizedResiduals), max(studentizedResiduals), length = 40)
  yfit <- dnorm(xfit)
  lines(xfit, yfit)
  
  
  # now fixing non-linearities
  
  # we saw that LSTAT and MEDV have a non-linear
  # let's see that again
  
  par(mfrow = c(1,1))
  plot(MEDV ~ LSTAT)
  
  # fiting exponential term to our model
  fit5 <- lm(MEDV ~ . - TAX - AGE - INDUS + I(LSTAT^2), data = boston.df)
  summary(fit5)
  par(mfrow = c(2,2))
  plot(fit5)
  
  bptest(fit5) # checking for homoskedasticity
  # p-value is 0.0001613 so our assumption for homoskedasticity is valid
  
  errors <- resid(fit5)
  plot(fit5, errors)
  
  hatvalues(fit5)
  cooks.distance(fit5)
  
  # cross-validation
  
#potential problems
  # non-linearity --- DONE
  # correlation of error terms --- 
  # heteroskedasticity --- DONE
  # outliers --- DONE
  # high-leverage points ---
  # collinearity --- DONE
  