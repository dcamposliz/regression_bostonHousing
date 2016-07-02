#####################################################################################################################################################
#####################################################################################################################################################
#####################################################################################################################################################
####
####	SETTING WORKING DIRECTORY & OUTPUT_FILES
####	LOADING DATA
####	INSTALL MISSING PACKAGES
####
#####################################################################################################################################################
#####################################################################################################################################################
#####################################################################################################################################################

	# setting working directory
	setwd("/home/dxc/myProjects/regression_bostonHousing/")
	# setting terminal_output file
	sink("output/output.txt")
	# setting pdf_output file
	pdf("output/output.pdf")
	
	# loading data
	boston <- read.csv("data/boston_housing_data.csv", header = TRUE, sep = ",")
	# attaching dimension names from data
	attach(boston)

	CHAS <- as.factor(CHAS)

	# creating data.frame for easier data_wrangling
	boston_df <- data.frame(CRIM, ZN, INDUS, CHAS, NOX, RM, AGE, DIS, RAD, TAX, PTRATIO, B, LSTAT, MEDV)


	# installing missing packages
		list.of.packages <- c("usdm", "car", "MASS", "DAAG")
		new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
		if(length(new.packages)) install.packages(new.packages, repos = "https://cran.cnr.berkeley.edu/")
		# source:	http://stackoverflow.com/questions/4090169/elegant-way-to-check-for-missing-packages-and-install-them

#####################################################################################################################################################
#####################################################################################################################################################
#####################################################################################################################################################
####
####	EXPLORATORY ANALYSIS
####
#####################################################################################################################################################
#####################################################################################################################################################
#####################################################################################################################################################

	# structure
	print("---------------------- Structure of boston_df ----------------------")
	str(boston_df)
	
	# scoping out first rows of data
	print("---------------------- Head of boston_df ----------------------")
	head(boston_df)

	# scoping out last rows of data
	print("---------------------- Tail of boston_df ----------------------")
	tail(boston_df)
	
	# getting a descriptive stats summary of our data
	print("---------------------- Descriptive Statistics of boston_df ----------------------")
	summary(boston_df)

	# getting scatterplot of all data vectors
		# outputting image_file
		png(filename="output/pairs_all.png", width = 1000, height = 1000, units = "px", pointsize = 12, bg = "white", res = 100)
	pairs( ~ MEDV + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + TAX + PTRATIO + B + LSTAT +  CRIM)
		# turning off png()
		dev.off()

	# loading usdm library, which vif() method is a dependency of
	library(usdm)

	# variance inflation factor (VIF) - which tells us about the correlation across variables in a data.frame so we can clean up our data
	print("---------------------- Printing VIF boston_df ----------------------")
	vif(boston_df)

#####################################################################################################################################################
#####################################################################################################################################################
#####################################################################################################################################################
####
####	BUILDING MODELS
####
#####################################################################################################################################################
#####################################################################################################################################################
#####################################################################################################################################################

	print("---------------------- Model 1 ----------------------")
	fit_1 <- lm(MEDV ~ ., data = boston_df)
	fit_1
	print("---------------------- Summary: Model 1 ----------------------")
	summary(fit_1)

	n <- nrow(boston_df)
	drop1(fit_1, k=log(n))

	# AGE has both the highest p-value as well as lowest AIC value, so we drop it
	print("AGE is getting dropped because it has the lowest AIC")

	print("---------------------- Model 2 ----------------------")
	fit_2 <- update(fit_1, ~ . - AGE)
	fit_2
	summary(fit_2)
	print("---------------------- Summary: Model 2 ----------------------")
	drop1(fit_2, k=log(n))

	# now INDUS has both the highest p-value as well as lowest AIC value, so we drop it
	print("INDUS is getting dropped because it has the lowest AIC")

	print("---------------------- Model 3 ----------------------")
	fit_3 <- update(fit_2, ~ . - INDUS)
	fit_3
	print("---------------------- Summary: Model 3 ----------------------")
	summary(fit_3)
	drop1(fit_3, k=log(n))

	#####################################################################################################################################################

	print("---------------------- Creating New DataFrame with MEDV, CRIM, ZN, CHAS, NOX, RM, DIS, RAD, TAX, PTRATIO, B, LSTAT ----------------------")

	# creating new data.frame with relevant variables
	boston_df_2 <- data.frame(MEDV, CRIM, ZN, CHAS, NOX, RM, DIS, RAD, TAX, PTRATIO, B, LSTAT)
	
	# checking colinearity of new data.frame
	print("---------------------- Printing VIF for our new DataFrame ----------------------")
	vif(boston_df_2)

	#####################################################################################################################################################

	# now we drop TAX to see what happens
	print("now we drop TAX because it has the highest VIF value ")

	print("---------------------- Model 4 ----------------------")
	fit_4 <- update(fit_3, ~ . - TAX)
	fit_4
	print("---------------------- Summary: Model 4 ----------------------")
	summary(fit_4)
	drop1(fit_4, k=log(n))


	#####################################################################################################################################################

	# creating new data.frame with relevant variables
	boston_df_3 <- data.frame(MEDV, CRIM, ZN, CHAS, NOX, RM, DIS, RAD, PTRATIO, B, LSTAT)
	# checking colinearity of new data.frame
	vif(boston_df_3)


	# outputting image_file
	png(filename="output/outlierTest_fit_6.png", width = 1000, height = 1000, units = "px", pointsize = 12, bg = "white", res = 100)
	pairs(~ MEDV + CRIM + ZN + CHAS + NOX + RM + DIS + RAD + PTRATIO + B + LSTAT)
	dev.off()
	# turning off png()

#####################################################################################################################################################

	print("---------------------- Model 5 ----------------------")
	fit_5 <- lm(MEDV ~ . + I(LSTAT^2), data = boston_df_3)
	fit_5
	print("---------------------- Summary: Model 5 ----------------------")
	summary(fit_5)
	drop1(fit_5, k=log(n))

#####################################################################################################################################################

	print("---------------------- Model 6 ----------------------")
	fit_6 <- lm(MEDV ~ . + poly(LSTAT ,5), data = boston_df_3)
	fit_6
	print("---------------------- Summary: Model 6 ----------------------")
	summary(fit_6)
	drop1(fit_6, k=log(n))
	#		
	#		#####################################################################################################################################################
	#		#####################################################################################################################################################
	#		#####################################################################################################################################################
	#		####
	#		####	RUNNING RESIDUE DIAGNOSTICS
	#		####
	#		#####################################################################################################################################################
	#		#####################################################################################################################################################
	#		#####################################################################################################################################################
	#		
	#		
	#			library(car)
	#		
	#			# assessing outliers
	#			outlierTest(fit_6) # Bonferonni p-value for most extreme obs
	#		
	#			#qq plot for studentized resid
	#				# outputting image_file
	#				png(filename="output/qqPlot_fit_6.png", width = 1000, height = 1000, units = "px", pointsize = 12, bg = "white", res = 100)
	#			qqPlot(fit_6, main="QQ Plot")
	#				dev.off()
	#				# turning off png()
	#		
	#		
	#			# normality of residuals
	#			# distribution of studentized residuals
	#			library(MASS)
	#			sresid_fit_6 <- studres(fit_6)	
	#				# outputting image_file
	#				png(filename="output/hist_sresid_fit_6.png", width = 1000, height = 1000, units = "px", pointsize = 12, bg = "white", res = 100)
	#			hist(sresid_fit_6, freq=FALSE, main="Distribution of Studentized Residuals for Model 6")
	#			xfit_6<-seq(min(sresid_fit_6),max(sresid_fit_6),length=40) 
	#			yfit_6<-dnorm(xfit_6)
	#			lines(xfit_6, yfit_6)
	#				# turning off png()
	#				dev.off()
	#		
	#		#####################################################################################################################################################
	#		#####################################################################################################################################################
	#		#####################################################################################################################################################
	#		####
	#		####	CROSS VALIDATION
	#		####
	#		#####################################################################################################################################################
	#		#####################################################################################################################################################
	#		#####################################################################################################################################################
	#		
	#		
	#			library(DAAG)
	#		
	#			print("CROSS VALIDATION FOR MODEL 4")
	#		
	#				# outputting image
	#				png(filename="output/CV_f4.png", width = 1000, height = 1000, units = "px", pointsize = 12, bg = "white", res = 100)
	#			f4_CV <- CVlm(data = boston_df_2, form.lm = formula(fit_4), m = 10, dots = FALSE, seed = 1, plotit = c("Observed", "Residual"), main = "Cross-validation for fit_4", legend.pos="topleft", printit = TRUE)
	#				dev.off()
	#				# turning off png()
	#			summary(f4_CV)
	#			attributes(f4_CV)
	#			# mean squared: 24.2
	#		
	#			#####################################################################################################################################################
	#		
	#			print("CROSS VALIDATION FOR MODEL 5")
	#		
	#				# outputting image
	#				png(filename="output/CV_f5.png", width = 1000, height = 1000, units = "px", pointsize = 12, bg = "white", res = 100)
	#			f5_CV <- CVlm(data = boston_df_3, form.lm = formula(fit_5), m = 10, dots = FALSE, seed = 1, plotit = c("Observed", "Residual"), main = "Cross-validation for fit_5", legend.pos="topleft", printit = TRUE)
	#				dev.off()
	#				# turning off png()
	#			summary(f5_CV)
	#			attributes(f5_CV)
	#			# mean squared: 19.8
	#		
	#			#####################################################################################################################################################
	#		
	#			print("CROSS VALIDATION FOR MODEL 6")
	#		
	#				# outputting image
	#				png(filename="output/CV_f6.png", width = 1000, height = 1000, units = "px", pointsize = 12, bg = "white", res = 100)
	#			f6_CV <- CVlm(data = boston_df_3, form.lm = formula(fit_6), m = 10, dots = FALSE, seed = 1, plotit = c("Observed", "Residual"), main = "Cross-validation for fit_6", legend.pos="topleft", printit = TRUE)
	#				dev.off()
	#				# turning off png()
	#			summary(f6_CV)
	#			attributes(f6_CV)
	#			# mean squared: 18.4
	#		
	#		#####################################################################################################################################################