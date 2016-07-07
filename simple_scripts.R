# loading libraries

library(usdm) # for testing collinearity
library(car) # for testing outliers
library(MASS) # for testing studentized residuals
library(DAAG) # for cross validation of model
library(lmtest) # for checking homoskedasticity / heteroskedasticity

# loading data

setwd("/home/dc/myProjects/inertia7_projects/regression_bostonHousing/")

boston <- read.csv("data/boston_housing_data.csv", header = TRUE, sep = ",")
attach(boston)
str(boston)
# turning CHAS into factor variable as it only can only take two values
CHAS <- as.factor(CHAS)
boston.df <- data.frame(CRIM, ZN, INDUS, CHAS, NOX, RM, AGE, DIS, RAD, TAX, PTRATIO, B, LSTAT, MEDV)

# exploratory analysis

str(boston.df)
pairs(boston.df)

# model 1

fit1 <- lm(MEDV ~ ., data = boston.df)
summary(fit1)

bptest(fit1) # checking for homoskedasticity
# p-value is very small so our assumption for homoskedasticity is valid

vif(fit1)

# model 2

# for this iteration of the model we are removing TAX because of its high collinearity with another one of the variables in the model

fit2 <- update(fit1, ~ . - TAX)
vif(fit2) # collinearity seems to no longer be an issue
summary(fit2)

# model 3

# for this iteration of the model we remove AGE, INDUS, and RAD because of their statistical non-significance in the model, as per their p-values

fit3 <- update(fit2, ~ . - AGE - INDUS - RAD)
summary(fit3)

# checking for outliers and high-leverage points

outlierTest(fit3, cutoff = Inf, n.max = 15)

# removing rows with outliers and high-leverage points

boston.df <- boston.df[-c(369, 372, 373, 370, 413, 365, 371, 366, 187, 375),]
  
# model 4

fit4 <- lm(MEDV ~ . - TAX - AGE - INDUS - RAD, data = boston.df)
summary(fit4)
par(mfrow = c(2,2))
plot(fit4)


# checking for outliers --- looking for normal distribution of our studentized residuals

studentizedResiduals <- studres(fit4)
par(mfrow = c(1,1))
hist(studentizedResiduals, freq = FALSE, main = "Distribution of Studentized Residuals")
xfit <- seq(min(studentizedResiduals), max(studentizedResiduals), length = 40)
yfit <- dnorm(xfit)
lines(xfit, yfit)


# now fixing non-linearities

# we saw in scatterplot matrix that LSTAT and MEDV have a non-linear relationship

# let's see that again

par(mfrow = c(1,1))
plot(MEDV ~ LSTAT)
#### ADD SMOOTHER HERE
  
# fiting exponential term to our model

fit5 <- lm(MEDV ~ . - TAX - AGE - INDUS - RAD + I(LSTAT^2), data = boston.df)
summary(fit5)
par(mfrow = c(2,2))
plot(fit5)

# fiting level three polynomial

fit6 <- lm(MEDV ~ . - TAX - AGE - INDUS - RAD + I(LSTAT^2) + I(LSTAT^3), data = boston.df)
summary(fit6)
par(mfrow = c(2,2))
plot(fit6)

# cross-validation

par(mfrow = c(1,1))
fit4_CV <- CVlm(data = boston.df, form.lm = formula(fit4), m = 10, dots = FALSE, seed = 1, plotit = c("Observed", "Residual"), main = "Cross-validation for fit4", legend.pos="topleft")
summary(fit4_CV)
attributes(fit4_CV)
# mean squared: 15.8

par(mfrow = c(1,1))
fit5_CV <- CVlm(data = boston.df, form.lm = formula(fit5), m = 10, dots = FALSE, seed = 1, plotit = c("Observed", "Residual"), main = "Cross-validation for fit5", legend.pos="topleft")
summary(fit5_CV)
attributes(fit5_CV)
# mean squared: 13.7

par(mfrow = c(1,1))
fit6_CV <- CVlm(data = boston.df, form.lm = formula(fit6), m = 10, dots = FALSE, seed = 1, plotit = c("Observed", "Residual"), main = "Cross-validation for fit6", legend.pos="topleft")
summary(fit6_CV)
attributes(fit6_CV)
# mean squared: 13.7




#potential problems
# non-linearity --- DONE --- ADDED EXPONENTIAL TERM TO MODEL
# correlation of error terms --- DONE --- vif()
# heteroskedasticity --- DONE --- bptest()
# outliers --- DONE --- outlierTest()
# high-leverage points --- outlierTest()
# collinearity --- DONE --- vif()
