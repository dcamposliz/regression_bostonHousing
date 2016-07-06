#####################################################################################################################################################
#####################################################################################################################################################
#####################################################################################################################################################
####
####	SETTING WORKING DIRECTORY & OUTPUT_FILES
####	LOADING DATA
####	LOADING PACKAGES
####
#####################################################################################################################################################
#####################################################################################################################################################
#####################################################################################################################################################
	
####
####	SETTING WORKING DIRECTORY & OUTPUT_FILES
####

	# setting working directory
	setwd("/home/dc/myProjects/inertia7_projects/regression_bostonHousing")
	# setting terminal_output file
	sink("output/output.txt")
	# setting pdf_output file
	pdf("output/output.pdf")

####
####	LOADING DATA
####

	# loading data
	boston <- read.csv("data/boston_housing_data.csv", header = TRUE, sep = ",")
	# attaching dimension names from data
	attach(boston)

	# turning CHAS variable into factor
	CHAS <- as.factor(CHAS)

	# creating data.frame for easier data_wrangling
	boston_df <- data.frame(CRIM, ZN, INDUS, CHAS, NOX, RM, AGE, DIS, RAD, TAX, PTRATIO, B, LSTAT, MEDV)

####
####	LOADING PACKAGES
####

	# installing missing packages
	list.of.packages <- c("usdm", "car", "MASS", "DAAG")
	new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
	if(length(new.packages)) install.packages(new.packages, repos = "https://cran.cnr.berkeley.edu/")
	
	#loading packages
	library(usdm)	# vif() is dependency
	library(car)
	library(MASS)
	library(DAAG)

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

#####################################################################################################################################################
#####################################################################################################################################################
#####################################################################################################################################################
####
####	BUILDING MODELS
####
#####################################################################################################################################################
#####################################################################################################################################################
#####################################################################################################################################################

	# discarding non-significant variables using backward elimination

####
####	MODEL 1
####

	print("---------------------- Model 1 ----------------------")
	fit_1 <- lm(MEDV ~ ., data = boston_df)
	fit_1
	print("---------------------- Summary: Model 1 ----------------------")
	summary(fit_1)
	print("---------------------- VIF: Model 1 ----------------------")
	vif(fit_1)

	n <- nrow(boston_df)
	print("---------------------- AIC: Model 1 ----------------------")
	drop1(fit_1, k=log(n))

####
####	MODEL 2
####
		
		# AGE has both the highest p-value as well as lowest AIC value, so we drop it
		print("AGE is getting dropped because it has the lowest AIC")


	print("---------------------- Model 2 ----------------------")
	fit_2 <- update(fit_1, ~ . - AGE)
	fit_2
	print("---------------------- Summary: Model 2 ----------------------")
	summary(fit_2)
	print("---------------------- VIF: Model 2 ----------------------")
	vif(fit_2)
	print("---------------------- AIC: Model 2 ----------------------")
	drop1(fit_2, k=log(n))

####
####	MODEL 3
####

		# now INDUS has both the highest p-value as well as lowest AIC value, so we drop it
		print("INDUS is getting dropped because it has the lowest AIC")

	print("---------------------- Model 3 ----------------------")
	fit_3 <- update(fit_2, ~ . - INDUS)
	fit_3
	print("---------------------- Summary: Model 3 ----------------------")
	summary(fit_3)
	print("---------------------- VIF: Model 3 ----------------------")
	vif(fit_3)
	print("---------------------- AIC: Model 2 ----------------------")
	drop1(fit_3, k=log(n))

