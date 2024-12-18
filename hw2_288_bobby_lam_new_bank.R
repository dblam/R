# Duy Bobby Lam
# dblam@uci.edu
# FE288 - Predictive Analytics - Ken Murphy

# Homework 2 - 10/20/2024

# DATA SET
# The Bank Salaries data set (revised) has 208 rows each of which represents the 
# employees of a small multi-branch banking entity.  Studying this data will reveal 
# relationships between various demographic, experience and skills variables and the 
# employees’ salary (Salary).  In the later portion of the assignment, one can investigate 
# an issue of discrimination at this bank.


# 1.	Read the Bank Salary data into RStudio using the .csv file. 
# Name the resulting data frame “dat”.  
dat = read.csv("wk3_288_salaries.csv")

# Examine the data.  
# How many columns of data are there?  
dim(dat) # there are 9 columns

# Run the “str” command on the data frame “dat” to examine the structure of the data “str(dat)”. 
str(dat)

# What are the variable types in the data set?
# there are 2 variable types in the data set = character type and integer type

# Convert the Gender variable to a factor first and 
# then, with the second command, to a binary (0/1) variable

dat$Gender = as.factor(dat$Gender) ; 
dat$Gender

dat$Gender = as.numeric(dat$Gender) -1 ; 
dat$Gender

names(dat)[6] = 'Male' 
str(dat)

# In order to eliminate errors in analysis, we will also need to convert char type of AnlExp in the data set..
# Code block below converts dat$AnlExp from char>factor>numeric-1 , then renames column to HasAnlExp 
dat$AnlExp = as.factor(dat$AnlExp)
dat$AnlExp = as.numeric(dat$AnlExp) - 1
names(dat)[8] = 'HasAnlExp'
str(dat)

# 2.	In this assignment, we will build models to predict the Salary of an employee at the bank.  
# Thus, it is useful to consider which variables might make sense to use a priori.  
# Write one or two sentences on three variables that one might suspect would be useful 
# in predicting the employees’ salary (Salary).  

# The 3 variables that might be useful in predicting employees salary include:
#   Education Level , Job Grade , and Years of Experience
# Explain why a positive or negative relationship makes sense.
#   I feel inclined to think a good education, mid-senior job level, and years of experience should yield good income.
#   Lower education level at an entry level job with almost zero work experience does not generally pay as much as the opposite extreme.

# Compute the correlation of all independent variables with the Salary variable.  

# After excluding employee ID numbers from the original 'dat' dataset & saving it to 'dat1' .. then verifying results, 
str(dat);dim(dat) 

# Resaving dat to dat1 to exclude EmployeeID
dat1 = dat[,-1];str(dat1);dim(dat1)

# we can run cor(dat1[,8],dat1[,-8]) to establish a correlation measure between the potential X variable against Y = Salary
cor(dat1[,8],dat1[,-8])

# Results:
#       EducLev  JobGrade  YrsExper      Age      Male    YrsPrior   HasAnlExp
# [1,] 0.524963 0.9226045 0.5779703 0.349934 0.3675277 -0.06217023 -0.02766135

# Which two variables are the most strongly correlated?  Is this surprising or expected?  Why?
# Strong correlation in EducLev , JobGrade, YrsExper. 
# This this expected; jobs that require a higher education, at a more midlevel, and with a few years 
# of experience should pay more based on expected career growth; year over year.
# Based on a 1to1 relationship between Salary (Y) and the other (Xn) variables in our table,
# the 3 potentially (X1,X2,X3) variables selected have the highest correlation values to Y

# Use the command:
  cor(dat)
# The result of the above command is a lot of numbers.  
# Indeed, it is a 9 by 9 matrix.  Do you know why?  
# The function cor(dat) will use matrix in dat to find correlation between the column variables as 2 identical vectors; unless specified.
# To just see the 9th row, try
cor(dat)[9,]
# What does the following command do? .. show results for the 9th column
cor(dat)[,9]
# Why does it give the same result? .. we are looking at the same column and row headings between 1-9

# 3.	Run the simple linear regression model that predicts Salary using the years of experience variable (YrsExper).  
# Store the result in an object called “reg3”.  
reg3 = lm(Salary ~ dat1$YrsExper,data=dat1); summary(reg3)
# a.	Interpret the estimated coefficient of years of experience.
# Salary (Y Variable) increases by $1494 for each additional year of experience (X Variable)
# b.	Interpret the estimated intercept.
# For an individual who has 0 year(s) of experience, we can expect their Salary to start at 39,263.
# c.	Does the intercept make sense in this case?  (Explain)
# Yes. We typically see this in minimum wage jobs that do not require a specialized skill or work experience.
# d.	How strong is the estimated model?  Interpret this value in the context of the problem.
# Not very strong given that our R-Squared is 0.334; meaning, only 33.4% of variation in Y=Salary is explained by X=YrsExp variable.
# e.	Is the coefficient of YrsExper significant at the alpha (α) = 0.05 level?
# Yes. The '***' on dat1$YrsExper row in summary output represent coefficient significant level of below alpha = 0.1% which is below 5%

# 4.	Run the regression that predicts Salary using the three most highly correlated independent variables as predictors.  
# Store the result in an object called “reg4”.  
reg4 = lm(Salary ~ EducLev+JobGrade+YrsExper, data=dat1);summary(reg4)
# a.	What is the estimated model?  (Type it in)
# Salary = 19900.72 + 1129.70*EducLev + 8759.90*JobGrade + 628.24*YrsExper
# b.	How strong is the estimated model?  Interpret this value in the context of the problem.
# Strong.. R-Squared=88.92; meaning that 88.92% of the variation in Salary prediction is explained by the 3 variables: EducLev,JobGrade,YrsExper
# c.	Which of the variables are significant at the alpha (α) = 0.05 level?
# All the variables included are significant at the alpha (a) = 0.05 level.

# Hint:  The command to run a regression with Salary as the Y variable and three variables X1, X2, and X3 as the three predictors is:
# reg3 <- lm(Salary ~ X1 + X2 + X3, data = dat)
# summary(reg3)
# 
# Don’t forget to replace X1, X2, and X3 with the actual variable names or this will give you an error!  
# R doesn’t know what X1, X2 and X3 are unless these are variables in your data set.  
# If you forget the names of the variables in the data frame, “dat”, use
# names(dat)

# Note:  Are there any variables in this data set that we should not be using in our prediction models?  
#   (Hint:  There is!)  Rhetorical question:  Why shouldn’t be using that variable? .. 
# EmployeeID should not be used in our model because it an integer variable type that is only used for labeling unique profiles.
  
# 5.	Compare the models of the two prior questions (Q3 and Q4).
# a.	Compare the coefficient of determination (R2) for “reg3” and “reg4”.
# From reg3 to reg4 the R-Squared value went from 33.4% to 88.92%, 
# single to multi-variables regression that included 3 variables with strong positive correlation. 
# Thus, our prediction model has improved with higher % of variablility in Y explained by Xi variables.  
# b.	Compare the adjusted-R2 for “reg3” and “reg4”.  
# reg3 Adusted R-Squared = 33.08 , reg4 Adjusted R-Square = 88.76
# A higher adjusted R-Squared value translate to better fit of the data on predicting Y=Salary.
# Why might the adjusted-R2 important in comparing multiple regression models?
# Adjusted R-Squared is better when comparing multiple regression models because 
# as the number of independent (X) variables added to the 'multi-linear' model, R-Squared tends to improve in nature.
# In a more extreme case, R-Square can be overly influenced with more variables added; leading to overfitting.  
# Therefore, Adjusted R-Square attempts to discount the increase based on a weighted index scale based on the number of independent variables used.
# c.	Obtain the residual sum of squares, RSS, (also, known as the sum of squared errors) for both models of Q3 and Q4. 
# reg3 RSS = 4.5010e+10 = 45009508036
# reg4 RSS = 7.4858e+09 = 7485812017
# Which model has smaller (RSS = SSE)?  Does this make sense?  Explain.  
# reg4 has a smaller RSS .. Yes , RSS is the sum of the squared differences between the predicted and actual values of data.
# This means that we have a better fit for reg4 model and with more variability being explained by additional X independent variables,
# we are able to better predict Y_hat when compared to actual Y values.

# Hint:  For part c, Use the ANOVA command to obtain the RSS for regression from Q3, reg3:  
#   anova(reg3) 

# Note:  For part c, one can often “store” the result of a function to obtain the exact values from that stored object.  
# What does this do:  
  anov3 <- anova(reg3) #saves anova analysis summary to anov3 
anov3$'Sum Sq'[2] #previews the RSS from anova analysis [2] is the residual of the single lm
rss3 <- anov3$'Sum Sq'[2] #saves the residual value of anov3 into rss3
rss3

# 6.	Run the regression that predicts Salary using all independent variables provided as predictors, 
# (that is, all that make sense to use-see Q4).  
# Call this model, “reg.all”.  
reg.all = lm(Salary ~ . - Salary, data=dat1);summary(reg1)
# a.	How strong is the estimated model?  Interpret this value in the context of the problem.
# Seems to be a more accurate model than reg4 with 3 independent variables.
# reg.all model involves all column variables as independent (X) variables (excluding Salary & EmployingID) to predict (Y= Salary)
# Estimated Equation = 18381.90 + 938.14*EducLevel + 8458.76*JobGrade + 658.44*YrsExper + 19.96*Age + 3751.63*Male + 72.79*YrsPrior + 5003.33*HasAnlExper
# b.	Which of the variables are significant at the alpha (α) = 0.05 level?
# Variables Significant at alpha = 0.05 : EducLevel,JobGrade,YrsExper,Male,&HasAnlExp
# c.	Compare the R2 and adjusted R2 found here in “reg.all” to that of the two earlier models.  
# Which model is preferred of the three based on these measures of fit?
# [R-Squared & Adjusted R-Squared] of reg4 = [0.8892 & 0.8876]
# [R-Squared & Adjusted R-Squared] of reg.all = [0.9006 & 0.8971]
# reg.all is preferred and seems to have a better fit
# d.	Qualitatively compare the three models with respect to bias and variance.  Which is the highest in bias?  Which likely has the greatest variance?  Discuss.

# Hint:  For this question, an easy way to run the regression without the “nuisance” variable is:  
#   reg.all <- lm(Salary ~ . -Employee , data = dat) 
# summary(reg.all)
# 
# The notation “.” in the above command uses all the variables in the data frame “dat”, 
# except we did not use the “Employee” variable. We “subtracted” it out – note not mathematically.

# 7.	Consider the job grade (JobGrade) and education level (EducLev) variables.  
# Throughout the assignment we’ve simply used these variables as they were provided, 
# that is, we’ve used these variables with the numerical values that they came with.  
# Does it make sense to incorporate these variables in this way?
# No. EducLev and JobGrade should be analyzed as (categorical) qualitative data type rather than quantitative.
# Asked another way, what assumptions are we implicitly making 
# about the difference in Salary between different job grades?  
# With data types of EducLev and JobGrade being numeric, 
# we assume that there is a relation to how Salary based on levels of education and/or job grade; 
# the 2 variables should not be calculated based on their quantitative values.
# Between different education levels?  
# A person with a college degree should have a higher salary than a person without.
# Convert the Job Grade and Education Level variables to factors in the data set 
# and use this updated data set for the remainder of the assignment.
str(dat1)
dat1$EducLev = as.factor(dat1$EducLev)
dat1$JobGrade = as.factor(dat1$JobGrade)

# Hint:  See Q1 for the command to convert a variable to a factor.  
# Here’s the reminder for how to do this with the job grade variable:
#   dat$JobGrade <- as.factor(dat$JobGrade) 
# 
# It’s always a good idea after changing data types to run the “str” command on 
# the data frame to make sure you have what you think you have.  

# 8.	Instead of using all the data to build regression models to predict Salary, 
# use just 128 observations that are randomly sampled from this data frame, 
# “dat” to create a training data set.  
set.seed(987654)
train <- sample(1:208,128)
train
str(train)
# To this end, create a training data set with 128 randomly selected observations 
# and a test data set with remainder of the observations. 
dat1.train <- dat1[train,]
dat1.train[1:10,]
dat1.test <- dat1[-train,]
dat1.test[1:10,]
dat1[1:10,]
# Then rerun the three models from Q3, Q4 and Q6 above using the training data frame (“dat.train”) 
# created here instead of the data frame “dat”.
reg83 = lm(Salary ~ YrsExper,data=dat1.train) 
reg84 = lm(Salary ~ EducLev+JobGrade+YrsExper, data=dat1.train)
reg8all = lm(Salary ~ . - Salary, data=dat1.train)
# Call these new models, “reg83”, “reg84”, and “reg8all”, respectively.   
# How well do these three models fit now as compared to those computed in Q3, Q4 and Q6, respectively, above?  
summary(reg83) ; summary(reg3)
summary(reg84) ; summary(reg4)
summary(reg8all) ; summary(reg.all)
# [R-Squared, Adjusted R-Squared]
# reg3 = [0.334,0.3308]
# reg83 = [0.1654,0.1587]
# reg4 = [0.9006,0.8956]
# reg84 = [0.9149,0.9077]
# reg.all = [0.9089,0.9023]
# reg8all = [0.9237,0.9143]
# Training data seem to provide a better fit for multiple regression but not single regressing with improved R-Squared and Adjusted R-Squared

# Hint:  Use the following set of commands to create training and test data sets:
# set.seed(987654)
# train <- sample(1:208,128)
# train
# dat1.train <- dat1[train,]
# dat1.train
# dat1.test <- dat1[-train,]
# dat1.test
# reg3 = lm(Salary ~ YrsExper,data=dat1); summary(reg3)
# reg4 = lm(Salary ~ EducLev+JobGrade+YrsExper, data=dat1);summary(reg4)
# reg.all = lm(Salary ~ . - Salary, data=dat1);summary(reg.all)

# Rhetorical questions:  Why do we set a seed?  to get repeatable results with our ramdomize function
# Does the number inside the parentheses matter?  yes it serves as an entry point to generate samples
# What does the sample command do?  Remember you can Google this 
# sample takes a sample of the specified size from the elements of x using either with or without replacement.
# or try “?sample” at the R prompt to see if you can understand the documentation.  
# What is the output of the “sample” command “train”.  
# 128 samples of integer values between 1-208
# Try just typing “train” at the R prompt to see.  
# What do the last two commands do?  
# We took the 128 random integers from 1-208 to extract random rows of data from dat1.
# Then insert them into a new set call train for the training data set
# For the remaining 208-128 rows, we copied them into the test set for model evaluation
# Why does this all work?
# Setting the seed and using sample function enables us to do so.


# 9.	Now using the models built in Q8, on the training data to make predictions on the test data.  
# Then compute how effective the predictions were by computing errors or residuals.  
TSS <- sum((dat1$Salary-mean(dat1$Salary))^2)
TSS # = 37654852669
a83=anova(reg83);a84=anova(reg84);a8all=anova(reg8all)
a83$`Sum Sq`;a84$`Sum Sq`;a8all$`Sum Sq`

# RSS = Residuals of Trained Models : from anova()
# reg83 = 24829200858
# reg84 = 2530187905
# reg8all = 2268328628

# Finally, compute the mean squared errors (MSEs) in each case, which provides a summary estimate 
# of the average error of the prediction.  
# Report the MSEs which of the three predictions performs best?  
# Do this for each of the three models.     
# 
# Hint:  To obtain predicted Y values on any data frame “dat.test” using a specific regression “reg”, use: 
#   yhat83.tst <- predict(reg83, dat.test)

# OUT OF SAMPLE PREDICTION 
y83.hat = predict(reg83,dat1.test)
y84.hat = predict(reg84,dat1.test)
y8all.hat = predict(reg8all,dat1.test)

# Once the predicted values, yhat, are computed, the errors (MSEs) can be calculated.  
# This is done by comparing “yhat” to “y”, that is, by comparing the predicted and 
# actual values of the response or dependent variable-here it is Salary.  

# Again, assuming the data frame is “dat.test” and the response variable is “Salary”, then one could use:  
#   resid83.tst <- dat.test$Salary - yhat83.tst
# to compute the distance between the actual Salary (“y”) values 
# and the estimated Salary (“yhat”) values on the test data set.  


# COMPUTING RSS - Test Set
RSS83test <- sum((dat1.test$Salary-y83.hat)^2)
RSS84test <- sum((dat1.test$Salary-y84.hat)^2)
RSS8alltest <- sum((dat1.test$Salary-y8all.hat)^2)
RSS83test;RSS84test;RSS8alltest
# compute the RSS for the three training regressions
RSS83train <- sum(reg83$residuals^2)
RSS84train <- sum(reg84$residuals^2)
RSS8alltrain <- sum(reg8all$residuals^2)
RSS83train;RSS84train;RSS8alltrain

# Finally, to compute a summary measure of performance of a model on the test data, 
# for example, the mean square error, MSE, use
# MSE83.tst <- mean(resid83.tst^2)
# MSE83.tst

## Calculating RSE - root square to error .. MSE is calculated here .. 1/N *RSS
# 1,3,14 = # of coefficients in model
RSE83train <- sqrt(RSS83train/(nrow(dat1.train)-1-1))
RSE84train <- sqrt(RSS84train/(nrow(dat1.train)-3-1))
RSE8alltrain <- sqrt(RSS8alltrain/(nrow(dat1.train)-14-1))
RSE83test <- sqrt(RSS83test/nrow(dat1.test))
RSE84test <- sqrt(RSS84test/nrow(dat1.test))
RSE8alltest <- sqrt(RSS8alltest/nrow(dat1.test))

## Creating a table to show results
tab <- matrix(c(RSE83train, RSE84train, RSE8alltrain, 
                RSE83test, RSE84test, RSE8alltest), ncol=2, byrow=FALSE)
colnames(tab) <- c("Training", "Test")
rownames(tab) <- c("1 variable model", "3 variable model", 
                   "14 variable model")
tab
# 
#                   Training      Test
# 1 variable model  14037.705 16904.185
# 3 variable model   4517.161  8979.041
# 14 variable model  4480.369  8678.964

# In this case R matches the variable names used in the regression with the variables 
# in the columns of dat.test to provide the predicted values, “yhat”.  
# If these names don’t match, R will likely return an error.  
# Remember in this case, “dat.test” will represent the TEST data frame, not the training data frame.   
# Also, don’t forget that we did not create a model named “reg” in this assignment.  
# We just created “reg83”, “reg84” and “reg8all”, so this means you may want to also name your objects, 
# like “yhat” accordingly, e.g., “yhat83”.  

# 
# 10.	It is common to compare the performance of the model on the training data 
# to the performance of the model estimated using the training data in predicting 
# on the test data set.  
# If the model built on the training data has similar performance with respect to 
# accuracy on both training and test data sets, it is a “low variance” model.  
# If the performance on predictions is worse when evaluated on the test data 
# as compared to performance on the training data, then it is a “high variance” model. 
# 
# To check for low or high variance, we will compare the MSE from the models fit 
# on the  training data in Q9 to the MSE computed from the predictions made using 
# the test data.  We already computed the latter at the end of Q9.  
# 
# It would be great just to compare something like the R2 on both training and test models.  
# However, we did not “fit” a model on the test data, we simply estimated the Salary in 
# the test data using the “predict” command.  So, there is no R2 for the test data models.  
# 
# We have already computed MSEs for the test data using the 3 models from Q3, Q4, and Q6 in Q9.  
# Let’s obtain the MSEs from the models fit on the training data frames here.  
# Then we can answer the ultimate question. 

# Completed in previous question?
# RSS83train <- sum(reg83$residuals^2)
# RSS84train <- sum(reg84$residuals^2)
# RSS8alltrain <- sum(reg8all$residuals^2)
# RSS83train;RSS84train;RSS8alltrain
# RSE83train <- sqrt(RSS83train/(nrow(dat1.train)-1-1))
# RSE84train <- sqrt(RSS84train/(nrow(dat1.train)-3-1))
# RSE8alltrain <- sqrt(RSS8alltrain/(nrow(dat1.train)-14-1))

#                   Training      Test
# 1 variable model  14037.705 16904.185
# 3 variable model   4517.161  8979.041
# 14 variable model  4480.369  8678.964

# How do these three models perform in “out of sample” prediction?  
# In consideration for resiuals , the training data set performed better 
# than the out of sample test set which is in nature based on where the model was derived from.
# Is there evidence of high variance in any of the three cases?  
# Yes, evidence of high variance lays in the difference between training and the test data set 
# We would like those numbers to be closer to eachother.
# Why is this important?
# The prediction model minimizing the gap of residuals between trainin and testing will enable us to 
# deploy in real life scenarios where we be working from a results accuracy perspective with ambiguity
#   
#   Hint:  One can obtain the MSE (and store in “mse”) for a regression “reg” from the “anova” command which is shown below for the “all-in” regression of Q6 which was rerun in Q9:  
#   anov8all <- anova(reg8all)
# anov8all
# anov8all$'Mean Sq'[nrow(anov8all)] 
# mse8all <- anov8all$'Mean Sq'[nrow(anov8all)]
# mse8all
# mse8all.tst


