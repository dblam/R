#
#  Predictive Analytics (FE288): 
#  Forecasting with Time Series I:  Naive and ETS Models
#  Part 1:  Validation and Cross Validation of Time Series
#
#  Last updated:  6/2/22 (Murphy)
#
#   Data Valication & Cross Validation .. Naive Forecasting 
#  This script covers:
#     A. Time Series Model Validation using Naive Forecasting 
#        Methods
#     B. Alcohol Sales Forecasting using Naive
#        forecasting methods
#     C. Time Series Cross-Validation
#   
#
#  Use the monthly Alcohol Sales data from 2001 to 2018
#  Source:  https://fred.stlouisfed.org/graph/?id=S4248SM144NCEN,
#
#  The script also uses some built in Google Stock price data
#    from the gafa_stock series
#
#
#  The analysis also relies on the package fpp3
#
#install.packages("fpp3")
library(fpp3)




#
#  Part 1A:  Time Series Model Validation using 
#    Naive Forecasting Methods
#
#  Set up the Google data (See H&A Chapter 2)
#
goog_st <- gafa_stock
#filter only google and stock year after 2015
goog_15pl <- filter(goog_st, Symbol == "GOOG", year(Date) >= 2015)
#mutate and add another column called 'day'
goog_15pl <- mutate(goog_15pl, day = row_number())
goog_15pl <- update_tsibble(goog_15pl, index = day, regular = TRUE)
str(goog_15pl)
#
autoplot(goog_15pl,Close)
#
str(goog_15pl)
#further filter to just 2015
goog_15 <- filter(goog_15pl, year(Date) == 2015)
autoplot(goog_15, Close)
#


#
#   Build some naive forecasting models with
#     the Google Stock
#   (See H&A Chapter 5)
#
#Building a NAIVE Forecast using model() command specifying 3 models on Close .. using : 
#MEAN() , use the mean of data set to forecast forward
#NAIVE() , use the last observation to predict next , use yesterday's to predict tomorrow
#NAIVE( ~drift()) , starts from last observation , subtract to first to consider positive or negative 'trend' , then extrapolate trend by one period at a fixed rate
fit_goog_15 <- model(goog_15, Mean = MEAN(Close),
                     Naive = NAIVE(Close),
                     Drift = NAIVE(Close ~ drift()))
#
#
#
#
#  Let's examine when we have created
# augment() == translate fit_goog_15 into useful summary
aug_fit_goog_15 <- augment(fit_goog_15)
# fitted , residual , innovative residual
str(aug_fit_goog_15)
# 4 layer plot 'missing actuals' 
#bottom = residual = actual minus the line ; 
#both naive and drift method seem fairly effective
autoplot(aug_fit_goog_15, Close) +
  autolayer(aug_fit_goog_15, .fitted) + 
  autolayer(aug_fit_goog_15, .resid)
#
#
#
#  This command creates a h = 30 period forecast
# forwarding 30 periods via forcast()
forc_goog_15 <- forecast(fit_goog_15, h = 30)
autoplot(forc_goog_15, goog_15)
# include confidence bands .. turning off bands below
# mean is not very good , 
# how good are these forecast vs actuals? model validation
autoplot(forc_goog_15, goog_15, level = NULL)
#
#
#
#  Now select another sample of Google data
#    January of 2016 will play the role of the test data
#
goog_16jan <- filter(goog_15pl, 
                     yearmonth(Date) == yearmonth("2016 Jan"))
autoplot(goog_16jan, Close)





#
#  Now, as we often do, use the forecasting models created
#    on the 2015 Google Stock closing prices to predict
#    performance in January 2016
# Creating forecast for 2016 using the data that we fit from 2015
# forecast() similar to predict()
forc_goog_16jan <- forecast(fit_goog_15, new_data = goog_16jan)
str(goog_16jan)
#
#  Let's take a look at the results
# Plotting , forecast for 2016, actual for 2016 = black
autoplot(forc_goog_16jan, goog_15, level = NULL) +
  autolayer(goog_16jan, Close, colour = "black") +
  labs(y = "USD", title = "Google Closing Price", 
       subtitle = "Jan 2015-Jan 2016") +
  guides(colour = guide_legend(title = "Forecast"))
#
#
#
#
#  Now let's compute how effective these forecasts were  
# Commands to evaluate model() augment() forecast() ==> accuracy()
accuracy(forc_goog_16jan, goog_16jan) #consider RMSE
sd(goog_15$Close) 
#looking at forecast error 'goog_15' vs 'super' naive model
sd(filter(goog_15pl,yearmonth(Date) == yearmonth("2015 Dec"))$Close)
# better in reducing errors but still wildly off
# 'in linear regression , we used ramdom .. here we use current & future observaton
#


#
#  Part 1B:  Alcohol Sales Forecasting using Naive
#    forecasting methods
#
#
#
#  Read in and set up the data set
#
AlcSal <- read.csv("alcohol_sales.csv")
# to time series object
AlcSal$Date <- as.Date(AlcSal$Date, format = "%m/%d/%Y")
# remove data not needed
AlcSalts <- ts(AlcSal[,4], start = 2001, frequency = 12)
# to tsibble object
ASts <- as_tsibble(AlcSalts)
names(ASts)
str(ASts)
names(ASts)[2] <- "Sales"
rm(AlcSalts)
#
#
#  Visualize the alcohol sales data
#
autoplot(ASts, Sales)
#
#  Alcohol sales shows trend and monthly seaonality
#
#
#  Create training and test data for the Alcohol Sales
#
str(ASts)
names(ASts)
#
#  Split data into training and test sets
# Training Set = 2001-2016
ASts_tr <- filter(ASts, year(index) <= 2016)
# Test Set = after 2016
ASts_tst <- filter(ASts, year(index) > 2016)
#
#
#  Fit all "naive" forecasting models on training data
# model() , MEAN() , NAIVE() , NAIVE(~drift()) 
# SNAIVE() , use last season to predict next season .. Season must be included in tsibble table to do so
fit_ASts_tr <- model(ASts_tr, 
                     Mean = MEAN(Sales),
                     Naive = NAIVE(Sales),
                     Drift = NAIVE(Sales ~ drift()),
                     SNaive = SNAIVE(Sales)
                     )
#
#  Check and set what we estimated
#
augment(fit_ASts_tr)
#
#
#  Create forecasts on the test data using the model built
#    on the training data
# Using Training Model , Training Data , Test Data
forc_ASts_tst <- forecast(fit_ASts_tr, new_data = ASts_tst)
#
#
#  Plot the series and the forecasts
#
autoplot(forc_ASts_tst, ASts_tr, level = NULL) +
  autolayer(ASts_tst, Sales, colour = "black") + 
  #adding autolayer will show test set
  labs(y = "USD ($M)", title = "Alcohol Sales",
       x = "Year/Month") +
  guides(colour = guide_legend(title = "Forecast"))
#
#
#  Now let's compute how effective these forecasts were  
#
accuracy(forc_ASts_tst, ASts_tst) #accuracy on training data; NOT test data
sd(ASts_tr$Sales)
names(forc_ASts_tst)
forc_ASts_SN = filter(forc_ASts_tst, .model == "SNaive")
#Calculate Test Error .. verifying test error 966 is the difference between the line covering SNaive
MSE_test = mean((ASts_tst$Sales - forc_ASts_SN$.mean)^2); sqrt(MSE_test)
#
#  None of the naive models are too good compared to
#    the Seasonal Naive (sNaive) model which just uses
#    last year 'worth of data' to predict this year
#


#
#  Part 1C:  Time Series Cross-Validation with the 
#    Alcohol Sales data
#       (See H&A Section 5.10)
#
#
#  Set up an object to use for cross-validation
# Validation & Cross-Validation .. out of sample validation , how well does the model perform outside the data set? cross-valiation repeatedly test and fit dataset along with an adjustment average
#CROSSVALIDATION
ASts
#takes data set , start at the 12'th , 13'th , 14'th.. observation (for each..), creates another set object with multiple rows in it
ASts_tr_cv <- stretch_tsibble(ASts, .init = 12, .step = 1)
# using 12 period data set to check the 13th , then 13 period data set to check 14th , then 14 period data set to predict 15th... and so forth
ASts_tr_cv[1:20,]
#
#
#  Use the newly created cross-validation "training"
#    part of the data, that was set up above
#    to estimate the Seasonal Naive model
#
fit_ASts_tr_cv <- model(ASts_tr_cv, 
                        SNaive = SNAIVE(Sales))
forc_ASts_tr_cv <- forecast(fit_ASts_tr_cv, h = 1)
forc_ASts_tr_cv[1:20,]
str(forc_ASts_tr_cv)
accuracy(forc_ASts_tr_cv, ASts)
#
#  Compare the accuracy here to the accuracy on the 
#    training data overall
#
fit_ASts_tr1 <- model(ASts, SNaive = SNAIVE(Sales))
accuracy(fit_ASts_tr1)
#
#  Exactly the same....something to do with SNAIVE forecast
#



#
#  Try the example a second time, except this time
#    use the "Drift" forecast
#
fit_ASts_tr_cv <- model(ASts_tr_cv, 
                        Drift = RW(Sales ~ drift()))
forc_ASts_tr_cv <- forecast(fit_ASts_tr_cv, h = 1)
forc_ASts_tr_cv
accuracy(forc_ASts_tr_cv, ASts)
#
#  Again compare with training error
#
fit_ASts_tr1 <- model(ASts, Drift = RW(Sales ~ drift()))
accuracy(fit_ASts_tr1)
#
#  Interesting the cross-validated forecast is worse....
#    This is good to know, but also counter-intuitive
#
#
#
#  End of Forecasting with Time Series I Part 1
#
#

