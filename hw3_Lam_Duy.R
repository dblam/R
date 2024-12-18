# Duy Bobby Lam
# dblam@uci.edu
# FE288 - Predictive Analytics - Ken Murphy

# Homework 3 - 10/27/2024

#1.	Read the hourly bike sharing data into R/RStudio.  Use the script provided in the Appendix below to preprocess the data for the assignment.  After doing this answer the following questions:
# a.	What is the size of the data set?  That is how many rows and how many columns? [rows , columns] ==> [17379 , 17] 
# b.	What is the response (dependent or Y) variable in the data? cnt = # of rentals per hour
# c.	Give an example of one quantitative variable  in the data set?  What is the variable and what is its scale? temp is an example of a quantitative variable , its scale (given : min = 0.20 and max = 1.00) is Normalized temperature in Celsius. The values are divided to 41 (max).
# d.	Give an example of one qualitative variable in the data set?   What is the variable, what values does it take on, and what do these values represent?
#    workingday : if day is neither weekend nor holiday is 1, otherwise is 0.
# Hint:  To read in the data use:
library(fpp3)
dat <- read.csv("hw3_288_bike_hour.csv")
dim(dat)
dat1 = dat[0:500,]
str(dat1)
# str(dat1)
# dat1$dteday = as.Date(dat1$dteday,format="%m/%d/%Y")
# dat1$mnth = as.factor(dat1$mnth)
# dat1$season = as.factor(dat1$season)
# dat1$yr = as.factor(dat1$yr)
# dat1$yr = as.numeric(dat1$yr) -1
# names(dat1)[4] = "is2012"
# dat1$workingday = as.factor(dat1$workingday)
# dat1$workingday = as.numeric(dat1$workingday) -1
# names(dat1)[9] = "isholiday"
# dat1$weathersit = as.factor(dat1$weathersit)
# dat1$weekday = as.factor(dat1$weekday)
#dat3 = readr::read_csv("hw3_288_bike_hour.csv")
#names(dat3)
#dat1$mnth = as.numeric(dat1$mnth)
#dat1 = as_tsibble(dat1, index = instant)

names(dat1)
dat1$dteday = as.Date(dat1$dteday,format="%m/%d/%Y")
dat1$mnth = as.factor(dat1$mnth)


#2.	Using the data frame, “dat1”, built in Q1, plot the data in time across the two years in the data set.
#Hint:  To plot the data use:
plot(dat1$instant, dat1$cnt)
datseason <- ts(dat1[,17], start = 1, frequency = 12)
#datseason1 <- ts(dat1[,17], start = 1, frequency = 4)
datseason[1:10]
str(datseason)
ASts = as_tsibble(datseason)
str(ASts)
names(ASts) = c("Month", "Count")
autoplot(ASts, Count)
gg_season(ASts, Count)
gg_season(ASts, polar = TRUE)

#a.	What pattern(s) is(are) observed over the entire 2 years? Average bikes rented per day increases over 2 years,  
#b.	What pattern(s) is(are) observed over 1 month worth of data?  One week?  One day? season highs seem to be between may-aug (summer),
#c.	What do you conclude from this in terms of variable that might be useful for predicting the number of bike rentals? there is a seasonal theme to increased rentals 
#d.	Name a second variable that might be useful, different from the conclusion drawn in part c above with an argument of why you feel it might be useful in the predictive model. weathersit qualitative variable might be useful making an inference on our rentals data

#You can add axis labels and headings if desired.  

#To plot less observations, for example 100 data points, one could use:
  plot(dat1$obs[1:100], dat1$cnt[1:100])
  
  # 3.	Create a training data set with the first year of data and make a test data set from the remainder (second year) of the data set.  
dat1 = dat
dat1$yr = as.factor(dat1$yr)
dat1$yr = as.numeric(dat1$yr) -1
dat1$season=as.factor(dat1$season)
dat1$weekday=as.factor(dat1$weekday)
dat1$weathersit=as.factor(dat1$weathersit)
dat1$mnth=as.factor(dat1$mnth)
dat1$registered=as.factor(dat1$registered)
dat1$holiday=as.factor(dat1$holiday)
names(dat1)[4] = "is2012"
dat2011 = filter(dat1, dat1$is2012 == "0")
dat2012 = filter(dat1, dat1$is2012 == "1")

  # a.	Run the “all-variables-in” regression model on the training data.  What is the overall fit for this model?  
#str(dat2011)
#reg2011 = lm(cnt~.-instant-dteday-hr-is2012-registered-workingday, data = dat2011);summary(reg2011)

dat.test = cbind(dat2012[,3],dat2012[,5],dat2012[,7:8],dat2012[,10:15],dat2012[,17])
names(dat.test)[1] = "season";names(dat.test)[2] = "mnth";names(dat.test)[11] = "cnt"
dat.train = cbind(dat2011[,3],dat2011[,5],dat2011[,7:8],dat2011[,10:15],dat2011[,17])
names(dat.train)[1] = "season";names(dat.train)[2] = "mnth";names(dat.train)[11] = "cnt"
str(dat.test);dim(dat.test)
str(dat.train);dim(dat.train)

reg.train = lm(cnt~., data = dat.train);summary(reg.train)
reg.train1 = lm(cnt~., data = dat.train2);summary(reg.train1)

#calculation ref : https://stackoverflow.com/questions/43123462/how-to-obtain-rmse-out-of-lm-result
#calculating RSS 
reg.trainRSS = c(crossprod(reg.train$residuals));reg.trainRSS
#calculating MSE or RSE
reg.trainRSE = reg.trainRSS / (length(reg.train$residuals)-10-1);reg.trainRSE
#calculating RMSE or Root MSE
reg.trainRMSE = sqrt(reg.trainRSE);reg.trainRMSE
  #   b.	What is the RMSE (standard error of regression/residual standard error)?  Interpret this number in a sentence. RMSE = 83.8673 ... the number is further away from 0 and R-Squared is .607 , this means our model is not that good . only 60% of predictions is explained by all the x variables' coefficients
  # c.	Interpret the estimated slope coefficient of “holiday” in a sentence.
# holiday coeffficent = -68.40129 ... during a holiday, our estimated rental count decreases by about 68 units
  # d.	Interpret the estimated slope coefficient of “weathersit3” in a sentence.
# weathersit = -11.65980 ... during a light snow or light rain day, our estimated rental count decreases by roughly 11 units 
  # Hint:  The data set is two years of hourly data.  The first year ends on the 8645th observation.  To create training and test data sets, use:

#  4.	How well does this model estimated on question 3 perform on the test data set as compared to its performance on the training data?  
ytest.hat = predict(reg.train,dat.test)
#calculating RSS 
ytest.hatRSS = sum((dat.test$cnt-ytest.hat)^2);ytest.hatRSS
#reg.trainRSS = c(crossprod(reg.train$residuals));reg.trainRSS
#calculating MSE or RSE
ytest.hatRSE = ytest.hatRSS/nrow(dat.test);ytest.hatRSE
#reg.trainMSE = reg2011RSS / length(reg.train$residuals);reg.trainMSE
#calculating RMSE or Root MSE
ytest.hatRMSE = sqrt(ytest.hatRSE);ytest.hatRMSE
#reg.trainRMSE = sqrt(reg.trainMSE);reg.trainRMSE

# a.	Compute the RMSE on the test data set, when the trained model from Q3 is used to make the predictions.
# b.	Obtain the RMSE from the training data set
# c.	Compare the results and explain your insight.
#Training Set : [RSS,RSE,RMSE]  = [60806550, 7042.686, 83.92071]
#Test Set : [RSS,RSE,RMSE]      = [203885943, 23343.94, 152.7872]
#when training set is used to predict and compared to the test set, we observe higher residuals .. 
# d.	What does this say about the training-test split of the data used in Q3?  Could it be problematic?  Why?
#   this means that our prediction model is not really accurate against test or 2012 data observed
#   Hint:  This code will compute the RMSE on the test data set, “dat1.tst”
# yhat.tst <- predict(regall.tr,dat1.tst)
# resid.all <- dat1.tst$cnt - yhat.tst 
# MSE.all <- mean((resid.all)^2)
# RMSE.all <- sqrt(MSE.all)
# RMSE.all
# 
# Hint 2:  This code will obtain the RMSE from the model that was trained in Q3:
#   sum.all <- summary(regall.tr)
# sum.all$sigma
# 

# 5.	Use 6-digits from your own student identification number as a seed.  Randomly select 50% of the rows for a training data set and include the rest of the data in a test data set using the data frame created in Q1.  Run the “all-variables-in” regression model on the training data.  Don’t forget to drop the “obs” variable from the regression, however, the “yr” variable can remain in the model this time.  
set.seed(17289088)
tr2 <- sample(17379,17379/2)
dat2.train <- dat1[tr2,]
dat2.test <- dat1[-tr2,]
str(dat2.train)
dat.train2 = cbind(dat2.train[,3:5],dat2.train[,7:8],dat2.train[,10:15],dat2.train[,17]);names(dat.train2)[12] = "cnt"
dat.test2 = cbind(dat2.test[,3:5],dat2.test[,7:8],dat2.test[,10:15],dat2.test[,17]);names(dat.test2)[12] = "cnt"
str(dat.test2)
# a.	What is the fit for this model on the training data? R-Squared is lower at 0.5824
dim(dat.train2)
reg.train2 = lm(cnt~., data=dat.train2);summary(reg.train2)

#   b.	What is the RMSE for this model fit on the training data?  
#calculating RSS 
reg.train2RSS = c(crossprod(reg.train2$residuals));reg.train2RSS
#calculating MSE or RSE
reg.train2RSE = reg.train2RSS / (length(reg.train2$residuals)-11-1);reg.train2RSE
#calculating RMSE or Root MSE
reg.train2RMSE = sqrt(reg.train2RSE);reg.train2RMSE
#   c.	How does the current “training” RMSE compare to the training RMSE from Q3?
reg.trainRMSE;reg.train2RMSE
# > reg.trainRMSE;reg.train2RMSE
# [1] 83.92071
# [1] 118.7794
#   d.	Is it better or worse?  Provide insight
# it seems worst after introducing another variable which translate to higher residuals in variations of our predictions for training set
# Hint:  Recall the code used in the Week 2 Saturday session to create training and test data with randomly selected rows:
#   set.seed(number)
# tr2 <- sample(17379,17379/2)
# dat1.tr2 <- dat1[tr2,]
# dat1.tst2 <- dat1[-tr2,]
# Hint 2:  To run the appropriate “all-in” regression model here, it makes sense to drop two variables, 
# regall.tr2 <- lm(cnt ~ . -obs, data = dat1.tr2)
# summary(regall.tr2)
# 
# Hint 3:  See Q3 and Q4 and modify that script to answer part b. 


# 6.	How well does this model estimated on Q5 perform on the test data set as compared to its performance on the training data discuss in Q5?  
ytest.hat2 = predict(reg.train2,dat.test2)
#calculating RSS 
ytest.hat2RSS = sum((dat.test2$cnt-ytest.hat2)^2);ytest.hat2RSS
#reg.trainRSS = c(crossprod(reg.train$residuals));reg.trainRSS
#calculating MSE or RSE
ytest.hat2RSE = ytest.hat2RSS/nrow(dat.test);ytest.hat2RSE
#reg.trainMSE = reg2011RSS / length(reg.train$residuals);reg.trainMSE
#calculating RMSE or Root MSE
ytest.hat2RMSE = sqrt(ytest.hat2RSE);ytest.hat2RMSE
#reg.trainRMSE = sqrt(reg.trainMSE);reg.trainRMSE
#   a.	Compute the RMSE on the test data set, when the trained model from Q5 is used to make the predictions.
# > ytest.hat2RMSE = sqrt(ytest.hat2RSE);ytest.hat2RMSE
# [1] 114.5784
# b.	Obtain the RMSE from the training data set
reg.train2RMSE
# > reg.train2RMSE
# [1] 118.7794
# c.	Compare the results and explain your insight.
# our 2nd prediction model using new training set including yr provides a closer approximate to test set which is good . even with sacrificing accuracy in the training set , our new model explains closer variations to the 2nd test set being considered
# d.	What does this say about the bias-variance tradeoff in this setting?
# smaller residual on training set can be an issue when introduced to the test observation environment .. in making sound predictions, we want our model to be a closer approximate when considering a test set outside of the training environment


# 7.	Using the training and test data sets created in Q5 and build a “preferred” model that best addresses the bias/variance tradeoff for the bike rentals on the training data.  The model chosen would likely have less variables than the “all-in” model of Q5.  

# a.	Which variables were selected for your chosen model? season , is2012 , weekday , casual
#   b.	How is the fit of the chosen model as compared to the fit of the “all-in” model from Q5?  
str(dat.train2)
reg.train2.1 = lm(cnt~season+is2012+weekday+casual,data=dat.train2);summary(reg.train2.1)
#   c.	How does this model compare to the model of Q5 using the nested models F-test?  (See the hint below) the residual is a little bit higher but we simplified our estimated model by half the number of x variables ... 
anova(reg.train2,reg.train2.1)
# d.	Check the quality of the model trained in this question on the test data (like we did in Q4 and Q6).    How does the RMSE of the model fit on the training data compare to the RMSE of the model fit on the test data?
anova(reg.train1,reg.train2.1)
#   e.	Argue for using this model in place of the “All-In” model of Q5.
# we would prefer the simpler model while the residual RSS is very close between the complicated models from 4 to 6 to 7
# Hint:  To run the nested models F-test with the “all-in” model of Q5 and the “preferred” model here, we would use the “anova” command.   Suppose we called the regression model built in this question “reg.pref.tr”, the command to compare models would be
# anova(reg.pref.tr, regall.tr2)
# 
# This hypothesis test works like this:
#   Ho:  The models perform the same in predicting bike rental count
# Ha:  The model with more variables (regall.tr2) performs better at predicting bike variable count.
# Test Statistic:  F-test
# P-value:  Is found in the last column of the anova table.  Small P  Reject Ho

# 8.	(Automated model selection)  Using the training data set created in Q5, perform best subsets regression to pick a “good” model. 
library(leaps)
regfit.full <- regsubsets(cnt~., dat.train2)
summary(regfit.full)
summary(regfit.full)$rsq

out.bs <- regsubsets(cnt ~ ., dat.train2, nvmax = 5, really.big = T)
sum.bs <- summary(out.bs)
sum.bs
sum.bs$rsq

# a.	What is the highest R-squared model with 5 variables using this procedure?  What is the value of R-squared for this model?  (See Hint below)
# R-Squared = 0.5373194
# b.	What is the highest R-squared model with 10 variables using this procedure?
# R-Squared = 0.5733750
out.bs2 <- regsubsets(cnt ~ ., dat.train2, nvmax = 10, really.big = T)
sum.bs2 <- summary(out.bs2)
sum.bs2
sum.bs2$rsq
#   c.	What happens when the parameter “nvmax” is increased beyond 10?
# R-Square tops out aroung 0.58 averages
out.bs3 <- regsubsets(cnt ~ ., dat.train2, nvmax = 20, really.big = T)
sum.bs3 <- summary(out.bs3)
sum.bs3
sum.bs3$rsq
#   d.	When all the individual predictor variables in this data set are considered, there are 51 variables.  How many unique models are there when 10 variables are chosen from the 51?  When 20 are chosen?  Hint:   See if you can figure out how to use the “choose” command in R.
# 51 choose 10 vs 51 choose 20
# > choose(51,10)
# [1] 12777711870
# > choose(51,20)
# [1] 7.753516e+13
# e.	Do you know why it is hard to find the highest R-squared model (“best model”) with a certain number of variables when there are many predictors?
#there are so many combinations for choosing x variables..  at bias and variance trade off becomes an issue between complex and simplicity
# Hint:  The R package “leaps” is needed for this question, so you must run the “install.packages” and “library” commands.  Also, the Week 2 Saturday provided some detail on using this package.  Install the “leaps” package and then run the library command
# install.packages("leaps")
# library(leaps)
# 
# Do not forget to comment out the “install.packages” command with an “#” sign after running it.  This helps the teaching assistant in grading.  
# 
# Subset selection procedures are discussed in James et al, Section 6.1 with scripts at the end of the chapter.   To run best subsets to see all models with up to 5 variables, one would use:
#   out.bs <- regsubsets(cnt ~ . - obs, dat1.tr2, nvmax = 5, really.big = T)
# sum.bs <- summary(out.bs)
# sum.bs
# 
# 9 (Stepwise Regression)  Using the training data set from Q5, perform forward stepwise regression to pick a “good” model.  
# a.	What model did you select and why?  (See Hint 1)
# of the 3 models from Q4,Q6,Q7 (reg.train1,reg.train2,reg.train2.1) I selected Q7 reg.train2.1 after readjusting to RMSE from each model using anova and wanting to simplify the model for interpretation.
# b.	How does this model compare to the selected models from Q5 and Q7 in terms of fit? it is about the same in having close numbers between test and training data .. also with similar R-Squared
#   c.	Is there a difference in the set of variables selected?    
#   at a certain point our R-Square will tend to increase with more variables, therefore, the sweet spot is having a high R-squared and lower # of x variables to prevent overfitting
#   Hint :  To modify “regsubsets” to do forward (or backward) stepwise regression run:  
out.forwd <- regsubsets(cnt ~ . , dat.train2, nvmax = 51, method = "forward", really.big = T)		
sum.forwd <- summary(out.forwd)
sum.forwd

out.backwd <- regsubsets(cnt ~ . , dat.train2, nvmax = 51, method = "backward", really.big = T)		
sum.backwd <- summary(out.backwd)
sum.backwd
# 
# For backward stepwise regression, replace the method to “backward”.  
# 
# Hint 1:
#   To see how the R-squared increases in this stepwise procedure, use:
sum.forwd$rsq	
plot(sum.forwd$rsq)
# 
sum.backwd$rsq	
plot(sum.backwd$rsq)
# 
# To see the values of adjusted R-squared or Mallows Cp, try
 sum.forwd$adjr2
 plot(sum.forwd$adjr2)
 sum.forwd$cp
 plot(sum.forwd$cp)
# 
 sum.backwd$adjr2
 plot(sum.backwd$adjr2)
 sum.backwd$cp
 plot(sum.backwd$cp)
# Rhetorical question, how are these two quantities useful? yes
 
 
  
 # 10.	Using the model from Q7, create an forecast for the number of bike rentals in year 3 on Tuesday, June 26 (not a holiday) at 8pm when weathersit = 1, temp = 0.80, atemp = 0.85, hum = 0.50, and windspeed = 0.200.  
 # a.	What is the estimate of the number of rentals?
 #95.15325 
 #   b.	What is the error associated with the forecast?  Hint:  Remember the RMSE.
reg.train2RMSE
# > reg.train2RMSE
# [1] 118.7794
 # c.	Provide a 95% prediction interval for your forecast on rentals. 
# To obtain the interval:
 #   >    predict(reg.train2.1, newobs, interval = "prediction",
 #                +            level = 0.95)
 # fit       lwr      upr
 # 1 95.15325 -143.1433 333.4498
 # d.	How confident are you regarding the forecast made in this problem? 
 # I am more confident with the 3rd model from Q7 than i was from prior models   
 #   Hint:  The “predict” command is useful here.  First you need to set up the data to plug in to the predict command.  Here we will enter the data manually (a pain!):
names(dat.train2)
 newobs = data.frame(2,2,6,19,0,2,1,0.80,0.85,0.50,0.20,0)
#newobs <- data.frame(0,2,2,6,19,0,2,1,0.80,0.85,0.50,0.20,0)
 names(newobs) <- names(dat.train2)
 newobs$season <- as.factor(newobs$season)
 newobs$mnth <- as.factor(newobs$mnth)
# newobs$hr <- as.factor(newobs$hr)
 str(newobs)
 newobs$weekday <- as.factor(newobs$weekday)
 newobs$weathersit <- as.factor(newobs$weathersit)
 str(newobs)
 str(dat.train2)
 # 
 # Then apply the “predict” command:
   predict(reg.train2.1, newobs)

# To obtain the interval:
   predict(reg.train2.1, newobs, interval = "prediction",
           level = 0.95)
 # 
 
 