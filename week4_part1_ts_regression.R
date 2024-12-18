#
#  Predictive Analytics (FE288) 
#  Super Saturday Week 4 In-Class R Script: 
#  Part 1:  Time Series Regression
#  Last updated:  10/25/24 
#
#
#  This script covers:
#  1.  Basic components of times series regression
#  2.  Trend regression
#  3.  Trend and seasonal regression
#
#
#  Use the monthly Alcohol Sales data from 2001 to 2024
#  Source:  https://fred.stlouisfed.org/graph/?id=S4248SM144NCEN  
#
#  The fpp3 package written by Rob Hyndman is key to much of the
#    analysis this week - please install this package 
#
#  We will also use ggplot which is part of package fpp3
#
#  
#install.packages("ggplot2")
library(ggplot2)
#


#
#  Read in the "Alcohol Sales" data set
#
AlcSal <- read.csv("week04_alcohol_sales_for_Saturday_for_Mac.csv")
#
#  Convert a couple variables
#
names(AlcSal)
str(AlcSal)
AlcSal$Date <- as.Date(AlcSal$Date, format = "%m/%d/%Y")
AlcSal$Month <- as.factor(AlcSal$Month)
str(AlcSal)
head(AlcSal)
#



#
#  ***Time Series Regression***
#
#  Modeling Trend with Regression
#  
#  Trend is one of the three signals.
#
#  Start out with a "simple linear trend" regression
#   A "trend" variable is simply a column of 1, 2, 3 etc.
#  "Index" is such a column
#
head(AlcSal)
#
#  Run a simple linear regression
#
reg1 <- lm(Sales ~ Index, data = AlcSal)
summary(reg1)
sum1 <- summary(reg1)
#
#  Interpret the regression results
#
#  Let's plot the line to see
#
plot(AlcSal$Index, AlcSal$Sales)
lines(AlcSal$Index, AlcSal$Sales, type = "l")
abline(reg1, col = "purple")
#
yhat1 <- predict(reg1, AlcSal)
plot(AlcSal$Date, AlcSal$Sales)
lines(AlcSal$Date, AlcSal$Sales, type = "l")
points(AlcSal$Date, yhat1, col = "red")
lines(AlcSal$Date, yhat1, col = "red")
#


#
#  Now plot actuals and trend forecasts using ggplot
#
AlcSal1 <- cbind(AlcSal, yhat1)
str(AlcSal1)
ggplot(AlcSal1) +
  geom_line(aes(x = Date, y = Sales, color = "Sales")) +
  geom_line(aes(x = Date, y = yhat1, color = "Forecast")) +
  ggtitle("Monthly Alcohol Sales 2001 - 2024") +
  ylab("Sales in $1Ms") +
  xlab("Year") +
  scale_color_manual(values = c("purple","black"))
#
#  What is observed in the plot?
#





#
#  Compute error metrics "by hand"
#  First compute the residuals (errors)
#  Then the RSS, MSE, MAE and MAPE
#
#  MSE, MAE and MAPE are commonly used as performance 
#    measures in forecast comparison settings
#    MSE = mean square error
#    MAE = mean absolute error
#    MAPE = mean absolute percent error
#
err_reg1 <- AlcSal$Sales - yhat1
err_reg1[1:10]
#
RSS_reg1 <- sum(err_reg1^2)
RSS_reg1
#
MSE_reg1 <- RSS_reg1/(nrow(AlcSal)-1-1)
MSE_reg1
#
RMSE_reg1 <- MSE_reg1^0.5
RMSE_reg1
sum1$sigma
#
#  Two other errors of interest at times in 
#    Time Series Analysis are the MAE and MAPE
#
MAE_reg1 <- mean(abs(err_reg1))
MAE_reg1
#
MAPE_reg1 <- mean(abs(err_reg1)/AlcSal$Sales)
MAPE_reg1
#


#
#  Time Series Regression with a squared time variable
#    We will do this two ways....
#
reg12a <- lm(Sales ~ Index + I(Index^2), data = AlcSal)
summary(reg12a)
sum12a <- summary(reg12a)
#
#  Can you interpert the "Time" variable?
#
#  An alternative way to do this is:
#
AlcSal$IndexSQ <- AlcSal$Index^2
reg12b <- lm(Sales ~ Index + IndexSQ, data = AlcSal)
summary(reg12b)
sum12b <- summary(reg12b)
#
#  Compare the fit of these 3 models so far
#
sum1$r.squared
sum12a$r.squared
sum12b$r.squared
#
#  What do you think?
#





#
#  Make predictions and plot
#
yhat12a <- predict(reg12a, AlcSal)
AlcSal12a <- cbind(AlcSal, yhat12a)
str(AlcSal12a)
ggplot(AlcSal12a) +
  geom_line(aes(x = Date, y = Sales, color = "Sales")) +
  geom_line(aes(x = Date, y = yhat12a, color = "Forecast")) +
  ggtitle("Monthly Alcohol Sales 2001 - 2024") +
  ylab("Sales in $1Ms") +
  xlab("Year") +
  scale_color_manual(values = c("purple","black"))
#
#  What is observed in this plot?
#




#
#  Compute error metrics
#
MSE_reg12a <- sum(sum12a$residuals^2)/sum12a$df[2]
MSE_reg12a
MSE_reg12a^0.5
sum12a$sigma
#
MAE_reg12a <- mean(abs(sum12a$residuals))
MAE_reg12a
#
MAPE_reg12a <- mean(abs(sum12a$residuals)/AlcSal$Sales)
MAPE_reg12a
#






#
#  Time Series Regression with Trend and Seasons  
#
#    Add seasonal variables to the regression model
#  
str(AlcSal)
#
#  The "seasons" here are the months.  Recall,
#    a "seasonal" variable is any periodic variable
#    that is, one where the Y-variable might be 
#    affected by the periodicity of the "season"
#    whether it be day of the week, month, actual 
#    seasons or something else.
#
#  The script below show how to create individual 
#    binary month variables.  we won't use these
#    variables here it is commented out.  We'll just
#    use the "factor" variable "month" in our seasonal
#    regression
#
#  Run the regression with Seasonal variables included
#    Again, the "seasons" here are the months
#    
reg2 <- lm(Sales ~ . - Date, data = AlcSal)
summary(reg2)
sum2 <- summary(reg2)
#
#  How is the fit?
#  Interpret the seasonal variables.
#
#  Use the result to obtain a yhat (forecasted y values).
#    Compare the yhat values to actual values in a graph.
#
yhat2 <- predict(reg2, AlcSal)
AlcSal2 <- cbind(AlcSal, yhat2)
ggplot(AlcSal2) +
  geom_line(aes(x = Date, y = Sales, color = "Sales")) +
  geom_line(aes(x = Date, y = yhat2, color = "Forecast")) +
  ggtitle("Monthly Alcohol Sales 2001 - 2024") +
  ylab("Sales in $1Ms") +
  xlab("Year") +
  labs(colour="") +
  scale_color_manual(values = c("blue","red"))
#
#  What do you see in the plot?
#




#
#  Let's compute those error metrics "by hand" again
#
#  Compute the residuals (errors), and then the RSS, 
#    MSE, MAE and MAPE
#
MSE_reg2 <- sum(sum2$residuals^2)/sum2$df[2]
MSE_reg2
sum2$sigma
#
MAE_reg2 <- mean(abs(sum2$residuals))
MAE_reg2
#
MAPE_reg2 <- mean(abs(sum2$residuals)/AlcSal$Sales)
MAPE_reg2
#
#  Let's make some comparisons of these regression models
#    First look at the MSEs
#
MSE_reg2
MSE_reg1
MSE_reg2/MSE_reg1
#
#  How do these two models compare?
#
MAPE_reg2/MAPE_reg1
#
#  How do these two models compare?
#    

#
#  Let's create some forecasts using Regression 2
#
#  The hardest part is setting up the new data frame
#
str(AlcSal2)
AlcSal2.new <- AlcSal2[9:44,]
str(AlcSal2.new)
AlcSal2.new$Index <- 285:320
AlcSal2.new$IndexSQ <- AlcSal2.new$Index^2
AlcSal2.new$Sales <- "NA"
AlcSal2.new
str(AlcSal2.new)
#
#  Now make 3 years worth of predictions
#
yhat2.new <- predict(reg2, AlcSal2.new)
AlcSal2.new$yhat2 <- yhat2.new
#
#  Bind together the original data set and the
#    predictions
#
AlcSal2.extend <- rbind(AlcSal2, AlcSal2.new)
dim(AlcSal2.extend)
#
#  Plot it
#
plot(AlcSal2.extend$Index, AlcSal2.extend$yhat2)
lines(AlcSal2.extend$Index, AlcSal2.extend$yhat2)
points(AlcSal2.extend$Index, AlcSal2.extend$Sales, col = "red")
lines(AlcSal2.extend$Index, AlcSal2.extend$Sales, col = "red")
#




#  
#  End Super Saturday - Week 4 Part 1 
#


