#
#  Predictive Analytics (BANA288)  
#  Introduction to Time Series
#  Part 4:  Time Series Regression (using base R)
#  
#  Last updated:  5/31/22 (Murphy)
#
#
#  This script covers:
#  1.  Simple Linear Trend Regression 
#  2.  Quadratic Trend regression
#  3.  Trend and seasonal regression
#
#  In each case errors are found and these errors can
#    be used to compare models
#
#  This content is found in Chapter 3 of Introduction to
#    Statistical Learning by James et al.
#
#  Use the monthly Alcohol Sales data from 2001 to 2018
#  Source:  https://fred.stlouisfed.org/graph/?id=S4248SM144NCEN,
#
#  We will also use ggplot which is part of package fpp3
#
#  
#install.packages("fpp3")
library(fpp3)
#


#
#  Part 4A:  Read in data and perform simple
#    linear trend regression.  Compute error 
#    metrics.
#
#
#  Read in the "Alcohol Sales" data set
#
AlcSal <- read.csv("alcohol_sales.csv")
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
#  Plot the data
#
ggplot(AlcSal) +
  geom_line(aes(x = Date, y = Sales)) +
  ggtitle("Monthly Alcohol Sales 2001 - 2018") +
  ylab("Sales in $1Ms") +
  xlab("Year")
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
str(AlcSal)
reg1 <- lm(Sales ~ Index, data = AlcSal)
summary(reg1)
sum1 <- summary(reg1)
#
#  Strong regression equation - R-squared = 76%
#  Let's plot the line to see
#
plot(AlcSal$Index, AlcSal$Sales)
lines(AlcSal$Index, AlcSal$Sales, type = "l")
abline(reg1, col = "purple")


#
#  Use the result to obtain a yhat (that is, the forecasts 
#    for the y values)
#  Compare them to actuals
#
yhat <- predict(reg1, AlcSal)
yhat
cbind(AlcSal$Sales, yhat)[1:10,]


#
#  Now lets look at performance on a graph.  Create a data frame
#    with the forecasted values
#
pred_reg1 <- data.frame(AlcSal$Date, AlcSal$Sales, yhat)
names(pred_reg1) <- c("Date", "Sales", "SalesHat")
head(pred_reg1)
str(pred_reg1)


#
#  Now plot actuals and trend forecasts
#
ggplot(pred_reg1) +
  geom_line(aes(x = Date, y = Sales, color = "Sales")) +
  geom_line(aes(x = Date, y = SalesHat, color = "Forecast")) +
  ggtitle("Monthly Alcohol Sales 2001 - 2018") +
  ylab("Sales in $1Ms") +
  xlab("Year") +
  labs(colour="") +
  scale_color_manual(values = c("red","black"))
#
#  What is observed in the plot?
#
#  Obviously, doing well in capturing the trend
#    But, doing poorly at picking up the seasonality!
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
err_reg1 <- AlcSal$Sales - yhat
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
sd(AlcSal$Sales)
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
#  The errors are also inside of the regression model
#    
err_reg1_alt <- reg1$residuals
err_reg1_alt[1:20]
err_reg1[1:20]
#
#  
#


#
#  Part 4B:  Create time series regression model with a 
#    squared time variable.  Quadratic regression (form of
#    polynomial regression).  Then compute errors.
#
#
reg1a <- lm(Sales ~ Index + I(Index^2), data = AlcSal)
summary(reg1a)
sum1a <- summary(reg1a)
#
#  Strong regression equation - R-squared = 77%
#    Only slightly better than the first

#
#  Make predictions and plot
#
yhat <- predict(reg1a, AlcSal)
pred_reg1a <- data.frame(AlcSal$Date, AlcSal$Sales, yhat)
names(pred_reg1a) <- c("Date", "Sales", "SalesHat")
#
#  Now plot actuals and trend forecasts
#
ggplot(pred_reg1a) +
  geom_line(aes(x = Date, y = Sales, color = "Sales")) +
  geom_line(aes(x = Date, y = SalesHat, color = "Forecast")) +
  ggtitle("Monthly Alcohol Sales 2001 - 2018") +
  ylab("Sales in $1Ms") +
  xlab("Year") +
  labs(colour="") +
  scale_color_manual(values = c("purple","black")) 
#
#
#  What is observed in this plot?
#

#
#  Compute error metrics for quadratic regression
#
MSE_reg1a <- sum(sum1a$residuals^2)/sum1a$df[2]
MSE_reg1a
MSE_reg1a^0.5
sum1a$sigma
RMSE_reg1
#
MAE_reg1a <- mean(abs(sum1a$residuals))
MAE_reg1a
MAE_reg1
#
MAPE_reg1a <- mean(abs(sum1a$residuals)/AlcSal$Sales)
MAPE_reg1a
MAPE_reg1

sum1$residuals[1:20]
sum1a$residuals[1:20]





#
#  Part 4C:  Build a time series regression with both 
#    Trend and Seasonal variables.  Here the months will
#    act as the seasons.  We will also compute performance
#    metrics for fit.
#
#
#    Add seasonal variables to the regression model
#  
str(AlcSal)
#
#  The "seasons" here are the months.  Recall,
#    a "seasonal" variable is any periodic variable
#    that is, one where the Y-varible might be 
#    affected by the periodicity of the "season"
#    whether it be day of the week, month, actual 
#    seasons or something else.
#

#
#  Run a regression with the Seasonal variables along with
#    the Index (linear time trend) variable
#  The "seasons" here are the months.  Since 
#    the "months" variable is a factor, R takes care of 
#    any issues of the seasons.
#    
names(AlcSal)
str(AlcSal)
reg2 <- lm(Sales ~ . - Date, data = AlcSal)
summary(reg2)
sum2 <- summary(reg2)
#
#  Fit is strong now.  95%.  
#    The seasonal variables do matter.
#
#  Use the result to obtain a yhat (forecasted y values).
#    Compare them to actuals
#
yhat2 <- predict(reg2, AlcSal)
cbind(AlcSal$Sales, yhat2)[1:10,]
#
#
#  Now lets look at performance on a graph.  Create a data frame
#    with the forecasted values
#
pred_reg2 <- data.frame(AlcSal$Date, AlcSal$Sales, yhat2)
names(pred_reg2) <- c("Date", "Sales", "SalesHat")
str(pred_reg2)
#
#  Now plot actuals and trend forecasts
#
ggplot(pred_reg2) +
  geom_line(aes(x = Date, y = Sales, color = "Sales")) +
  geom_line(aes(x = Date, y = SalesHat, color = "Forecast")) +
  ggtitle("Monthly Alcohol Sales 2001 - 2018") +
  ylab("Sales in $1Ms") +
  xlab("Year") +
  labs(colour="") +
  scale_color_manual(values = c("green","black"))
#
#  What do you see in the plot?
#    It appears that seasonality is at least somewhat 
#    accounted for now.
#
#
#  Let's compute those error metrics "by hand" again
#
#  Compute the residuals (errors), and then the RSS, 
#    MSE, MAE and MAPE
#
#  Compute the errors
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
MSE_reg2
MSE_reg1a
MSE_reg2/MSE_reg1a
#
MSE_reg1a
MSE_reg1
MSE_reg1a/MSE_reg1
#
#  The model with seasonal indicators is clearly an 
#    improvement over the just using the trend variable
#    The MSE model 2 is just 21.7% of that in model 1, that's
#    a 80% decrease
#
MAPE_reg2/MAPE_reg1
#
#  The percentage error in the model with seasonal indicators
#    model 2 is 47.6% of that in model 1.  This is a big 
#    improvement.  Clearly, there is a large benefit for 
#    including these variables in the model.
#    


#  
#  End - Introduction to Time Series Part 4
#


#  The script below show how to create individual 
#    binary month variables.  These variables are 
#    not used here so it is commented out.  It could,
#    in some cases, be useful to just include particular
#    month variables, not just all of them or none
#
#M_ <- AlcSal$Month
#tempdat <- data.frame(model.matrix(~M_-1))
#head(tempdat)
#tempdat <- tempdat[,c(5,4,8,1,9,7,6,2,12,11,10,3)]
#head(tempdat)
#AlcSal2 <- cbind(AlcSal[,c(4,1,2)], tempdat)
#names(AlcSal2)
#rm(tempdat,M_)


