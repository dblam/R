#
#  Predictive Analytics (FE288) 
#  Super Saturday Week 4 In-Class R Script: 
#  Part 3:  Simple Forecasting Methods in fpp3
#  Last updated:  10/25/24 
#
#
#  This script covers:
#     1.  So called Naive Forecasting methods
#     2.  Discusses "out-of-sample" forecasts
#     3.  Evaluates residuals from forecasts
#
#
#  Use the monthly Alcohol Sales data from 2001 to 2018
#  Source:  https://fred.stlouisfed.org/graph/?id=S4248SM144NCEN,
#
#  The analysis also relies on the package fpp3
#    which was written and supported by Rob Hyndman
#
#
#install.packages("fpp3")
library(fpp3)
#

#
#  Read in and set up the data set
#
AlcSal <- read.csv("week04_alcohol_sales_for_Saturday_for_Mac.csv")
str(AlcSal)
AlcSal$Date <- as.Date(AlcSal$Date, format = "%m/%d/%Y")
AlcSal$Month <- as.factor(AlcSal$Month)
AlcSalts <- ts(AlcSal[,4], start = 2001, frequency = 12)
ASts <- as_tsibble(AlcSalts)
names(ASts) <- c("Month", "Sales")
names(ASts)
dim(ASts)
#



#
#  Visualize the alcohol sales data
#
autoplot(ASts, Sales) +
  labs(title = "Monthly Alcohol Sales 2001-2024")
#
#  What patterns does Alcohol Sales show?
#



#
#  Specify model - how about a trend model?
#
#  Estimate the simple trend model
#
mod_trd <- model(ASts, TSLM(Sales ~ trend()))
report(mod_trd)
#
#  Evaluate performance (already done before)
#
#  Forecast with the estimated model
#
forc_trd <- forecast(mod_trd, h = 36)
forc_trd
autoplot(forc_trd, ASts) + 
  labs(title = "Alcohol Sales with 3 Year Forecast",
       x = "Month", y = "Sales Volume")
#
#  Add the season indicators (months)
#
mod_trd2 <- model(ASts, TSLM(Sales ~ trend() + season()))
report(mod_trd2)
forc_trd2 <- forecast(mod_trd2, h = 36)
autoplot(forc_trd2, ASts, level = NULL) + 
  labs(title = "Alcohol Sales with 3 Year Forecast",
       x = "Month", y = "Sales Volume")
#



#
#  Basic Forecasting methods for time series
#
#    Forecasting Methods:
#    Mean - uses the mean of the data to forecast into the 
#      future-if the data is white noise, this is the best you
#      can do
#    Naive - uses the last observation as the best forecast 
#      in the future - better than meanf if the data is trending
#    Seasonal naive - uses the last value in the same season as the 
#      forecast for the next time it is that season 
#    Naive with drift = True - uses the last observation, but 
#      allows for a trend component
#    
#    The "h" parameter is how many periods forward you wish to
#      forecast
#
#
#  Mean Method
#
mod_mean <- model(ASts, MEAN(Sales))
report(mod_mean)
accuracy(mod_mean)
sd(ASts$Sales)^2
#
forc_mean <- forecast(mod_mean, h = "3 years")
autoplot(forc_mean, ASts)
#
#
#



#
#  Naive Method
#
mod_naive <- model(ASts, NAIVE(Sales))
report(mod_naive)
accuracy(mod_naive)
forc_naive <- forecast(mod_naive, h = "3 years")
autoplot(forc_naive, ASts)
#

#
#  Seasonal Naive Method
#
mod_snaive <- model(ASts, SNAIVE(Sales))
report(mod_snaive)
accuracy(mod_snaive)
forc_snaive <- forecast(mod_snaive, h = "3 years")
autoplot(forc_snaive, ASts)
#

#
#  Drift Method ("random walk with drift")
#
mod_drift <- model(ASts, RW(Sales ~ drift()))
report(mod_drift)
accuracy(mod_drift)
forc_drift <- forecast(mod_drift, h = 36)
autoplot(forc_drift, ASts)
#

#
#  Validating Forecasts
#
#  This process uses a training and test set.  We build
#    the forecast on the training set and evaluate it
#    on the test set.
#
#  First build a training data set (240 obs/20 years)
#
train <- filter_index(ASts, "2001 01" ~ "2020 12")
#
#  Now build all 4 models on training set
#
mod_t <- model(train, 
              Mean = MEAN(Sales),
              Naive = NAIVE(Sales),
              Seasonal_Naive = SNAIVE(Sales),
              Drift = RW(Sales ~ drift()))
#
#  Create forecasts for 2021 01 through 2024 08
#    This is 44 monthly observations
#
forc_t <- forecast(mod_t, h = 44)
#
#  Now plot the forecasts and actual
#    The autoplot command plots the training data
#    and the 4 forecasts.  The autolayer command
#    plots the "test" data (last 44 observations
#    of the actual Alcohol Sales)
#
autoplot(forc_t, train, level = NULL) +
  autolayer(filter_index(ASts, "2021 01" ~ .), color = "brown") + 
  labs(title = "Alcohol Sales with 3 Year Forecast",
       x = "Month", y = "Sales Volume")
#
#  Check accuracy
#
accuracy(mod_t)
#
#  Which model is best?
#
mod_trd2train <- model(train, TSLM(Sales ~ trend() + season()))
report(mod_trd2train)



#
#  Residual Analysis
#
#  First done manually
#
augment(mod_t)

?augment
aug_mean <- augment(mod_mean)
autoplot(aug_mean, .resid)
#
aug_naive <- augment(mod_naive)
autoplot(aug_naive, .resid)
#
mod_snaive1 <- model(train, SNAIVE(Sales))
report(mod_snaive1)
accuracy(mod_snaive1)
forc_snaive1 <- forecast(mod_snaive1, h = "3 years")
autoplot(forc_snaive1, train)

aug_snaive <- augment(mod_snaive1)
autoplot(aug_snaive, .resid)
#
aug_drift <- augment(mod_drift)
autoplot(aug_drift, .resid)
#


#
#  See a histogram of the residuals
#
ggplot(aug_mean, aes(x = .resid)) +
  geom_histogram()
ggplot(aug_mean, aes(x = .resid)) +
  geom_histogram(binwidth = 750)
#
ggplot(aug_snaive, aes(x = .resid)) +
  geom_histogram()
ggplot(aug_snaive, aes(x = .resid)) +
  geom_histogram(binwidth = 750)
#  What is this supposed to look like?
#





#
#  Look at the autocorrelation of the residuals
#
acf_mean <- ACF(aug_mean, .resid, lag_max = 20)
autoplot(acf_mean)

acf_snaive <- ACF(aug_snaive, .resid, lag_max = 20)
autoplot(acf_snaive)
#
#



#
#  A shortcut for these 3 plots is
#
gg_tsresiduals(mod_mean)
gg_tsresiduals(mod_naive)
gg_tsresiduals(mod_snaive1)
gg_tsresiduals(mod_drift)
#
#

#  
#  End Super Saturday - Week 4 Part 3 
#

