#
#  Predictive Analytics (FE288):
#  Forecasting with Time Series II:  
#  Part 1:  ARIMA Modeling - Identifying and Creating 
#    Stationary Data
#  
#  Last updated:  6/19/22
#
#
#  This script covers:   
#  A.  Testing for autocorrelation (Box-Pierce/Ljung-Box tests)
#  B.  Transforming data to address non-constant variance
#  C.  Testing for stationary data (Unit root tests) using 
#      psuedo-data (randomly generated data) and discussing random
#      walks (stationary data)
#  D.  Evaluating Stationarity in the Google and Alcohol Sales
#      data sets
#     
#
#  Use the monthly Alcohol Sales data from 2001 to 2018
#  Source:  
#    https://fred.stlouisfed.org/graph/?id=S4248SM144NCEN,
#  
#  This script uses other data sets as well
#    Google Stock Prices (goog)
#

#
#  In Arima Modeling we following the following steps
#  1.  Check if series needs to be transformed
#  2.  Identify in the series is stationary - if not
#        compute difference series
#  3.  Figure out the best AR and MA models to 
#        incorporate
#  4.  Make forecasts using ARIMA models
#  5.  Compare ARIMA models to ETS models
#
#  This script covers items 1 and 2 above
#
#
#
#install.packages("fpp3")
library(fpp3)


#
#  Part 1A:  Testing for Auto-Correlation 
#
#  If a data set has non-zero autocorrelation this is a 
#    mechanism to identify seasonality and whether or not 
#    the data is stationary
#
#
#  Read in and set up the Alcohol Sales data set
#
AlcSal <- read.csv("alcohol_sales.csv")
AlcSal$Date <- as.Date(AlcSal$Date, format = "%m/%d/%Y")
AlcSalts <- ts(AlcSal[,4], start = 2001, frequency = 12)
ASts <- as_tsibble(AlcSalts)
names(ASts)[2] <- "Sales"
names(ASts)
#
#
#  Examine plots of the data for Autocorrelation
#
#  The Autocorrelation and Partial Autocorrelation
#    plots show if the Y value is related to past 
#    values-if there is no relationship, to the past,
#    that is, if there is no significant auto-
#    correlation in the data, then there is no
#    reason to employ ARIMA methods
#
autoplot(ASts, Sales)
ACF(ASts, lag_max = 20)
autoplot(ACF(ASts, lag_max = 20))
PACF(ASts, lag_max = 20)
autoplot(PACF(ASts, lag_max = 20))
#
#  H&A Sections 2.8 and 9.5 describe the autocorrelation 
#    and partial autocorrelation functions, resp.
#
#  There is a hypothesis test for autocorrelation!
#  See H&A Section 5.4
#  
#  The Box-Pierce or Ljung-Box test for autocorrelation
#  Ho:  Data are uncorrelated in time
#  Ha:  Data are correlated in time
#  The two tests are very similar, the Ljung-Box is 
#    preferred.
#
#  These tests are known as "portmanteau" tests
#
?box_pierce
?features
features(ASts, Sales, box_pierce, lag = 24, dof = 0)
features(ASts, Sales, ljung_box, lag = 24, dof = 0)
#
#  Ultimately, the Alcohol Sales series does have 
#    significant correlation in time
#
#
#  To repeat the earlier statement, if there is no
#    autocorrelation at all, then ARIMA models won't 
#    provide much information, this will become apparent
#    at the end of this week's material
#
#  Note also that:
#    If the data is seasonal or is not stationary
#    then it will definitely be autocorrelated
#
#




#
#  Part 1B:  Transforming Alcohol Sales d ata to 
#    stablize variance
#
#  Before getting into the discussion of stationary series,
#    we consider an important class of transformations
#    the Box-Cox transformation.  Among other purposes
#    Box-Cox will help to stabilize the variance 
#    across the series, which is a fundamental assumption
#    in the errors being iid normally distributed with mean 
#    zero and standard deviation equal to sigma (fixed in t)
#    (iid = independent and identically distributed)
#
#  See H&A Section 5.6 for a discussion of this
#
#  Consider again the plot of the Alcohol data
#
autoplot(ASts)
#
#  In looking at the time series plot, it does 
#    appear that the variance in the series is 
#    growing in time
#
#  There are hypothesis tests for checking for 
#    non-constant variance or so-called 
#    heteroskadasticity, including White's test
#    and the Bruegel-Pagan test.  These are not 
#    illustrated here.
#
#  In any data set, one might consider taking logs to 
#    stabilize the variance.
#  More generally, there is a set of data transformations
#    known as the Box-Cox transformations: See H&A Section 
#    5.6
#
#  To compute a value of lambda for the Box-Cox 
#    transformation of the Alcohol Sales time 
#    series use the following function
# Hyndman section 5.1 == guerrero lambda feature
features(ASts, Sales, features = guerrero)
lambda <- features(ASts, Sales, features = guerrero)
lambda
autoplot(ASts, box_cox(Sales, lambda))
autoplot(ASts)
#
#  A small value of lambda points to using lambda = 0 
#    which means taking logs of the original Y variable
#    Here we don't do that, however, the computed lambda 
#    is used in the Box-Cox transformation (See H&A Section
#    5.6, equation 5.2)
#
#transforming data using mutate() salesl = log version of sales
AStsl <- mutate(ASts, Salesl = box_cox(Sales, lambda))
autoplot(AStsl, Salesl) +
  ylab("Log of Alcohol Sales") +
  xlab("Year")
str(AStsl)
#  The "logged" Alcohol Sales series appears to be 
#    more stable with respect to variance
#
#  The analysis of the Alcohol Sales series will
#    continue with both regular and logged series
#
#  FPP3 takes care of alot of these details for us
#
#
#
#   Take a quick look at the Google stock price data
#
google <- filter(gafa_stock, Symbol == "GOOG", 
                       year(Date) >= 2015)
#add column called day
google <- mutate(google, day = row_number())
#specify day is the index of tsibble
google <- update_tsibble(google, index = day, regular = TRUE)
#
#  Like the Alcohol Sales series the Google stock
#    price data clearly shows a trend in time
#  In this analysis, this is an example of non-stationary
#    data - something that must be addressed
#
str(google)
autoplot(google, Open)
autoplot(google, Close)
#
# see if data needs to be transformed .. 
features(google, Open, features = guerrero)
lambdag <- features(google, Open, features = guerrero)
lambdag
#
#


#
# 
#  Part 1C:  Addressing Non-Stationary Time Series using 
#    Random Walks as an example
#    
#  See H&A Section 9.1 for additional information
#
#  The goal of the ARIMA analysis process is to obtain
#    the implicitly generated signal in the series under
#    study and arrive at residuals with no autocorrelation
#
#  Any series that is not stationary, has no-zero 
#    autocorrelation, so this effect is analyzed first 
#    in the ARIMA analysis flow chart (see H&A Section 9.7)
#  
#  First consider a series that is stationary and has no
#    autocorrelation at all: "white noise"
#  Indeed white noise is a series of random generated
#    values from a normal distribution
#
#  This command generates 10 draws from a random
#    normal distribution with mean 10 and standard
#    deviation equal to 5
#
rnorm(10, mean = 10, sd = 5)
#
#  Now generate 300 observaitons from normal mean
#    zero and standard deviation one, i.e., N(0,1)
#
set.seed(112358)
wn_time <- ts(data.frame(rnorm(300)))
wnts <- as_tsibble(wn_time)
names(wnts)
#
#  Plot it.. white noise , no trend , no correlation in time , no level changes 
#
autoplot(wnts, value)
#
#  Consider the autocorrelation and partial 
#    autocorrelation
#
ACF(wnts, lag_max = 20)
PACF(wnts, lag_max = 20)
# all the ACF and PACF are within the dotted lines.. meaning there is no correlation
autoplot(ACF(wnts, lag_max = 20))
autoplot(PACF(wnts, lag_max = 20))
#
#  Unsurprisingly, there is no autocorrelation 
#    -that is a result of how this series 
#    was created 
#
# taking white noise series and turning it into a random walk
#  A random walk, yt, for t = 1, 2, etc., is generated
#    by the following equation yt = yt-1 + et where 
#    et is N(0,1) and uncorrelated with other et
#
#  Create a random walk, y_rw, with 300 observations
#
y_start = 0
#set up a blank time series object matrix of 0s , 300 rows , 1 column
y_rw_t <- ts(data.frame(matrix(0,300,1)))
y_rw_t[1] <- y_start
#running loop to iterate for each row, assign the row before + a random value from white noise series
for (i in 2:300){
  y_rw_t[i,1] <- y_rw_t[i-1,1] + wn_time[i]
}
y_rw <- as_tsibble(y_rw_t)
#plotting random walk values .. taking last data point and adding a random variable to it .. making it less white noisey
autoplot(y_rw)
# AUTO CORRELATION heavy auto correlation ACF
autoplot(ACF(y_rw, lag_max = 20))
# PARTIAL CORRELATION strong one bar 
autoplot(PACF(y_rw, lag_max = 20))
#
#GOAL : analyze the changes in the series.. 
#
#  Note that even though we just add a random draw from
#    a normal distribution to the previous observation to 
#    compute the next data point, the random walk tends to 
#     "drift"
# 
#  The idea of a random walk is used as a motivation to
#    compute differences of series, or, to "difference" 
#    a series-->by "difference" we mean subtract the 
#    most recent past point from the current point.
#
#  The differences, instead of the original data, are then 
#    analyzed for further autocorrelation (in time)
#
#  Note:  yt - yt-1 = et, so differences are 
#    random
#
#  fpp3 provides a function that computes differences
#    "diff"
#
#  To address this difference the data ("diff" function)
#  That is, subtract a value of the data from the one before
#
#ADDING difference values
str(y_rw)
# mutate and adding extra column 'differences'
y_rw <- mutate(y_rw, value_d = difference(value)) #difference(value)
str(y_rw) #first one is missing value_d[1] = NA
head(y_rw)
#
#  Above analysis shows how the differences calculated
#    by the diff function are the same as the differences
#    calculated by taking yt - yt-1
#  Note the differences series, y_diff, loses one 
#    observaition
#  And, of course, the differences are going to be
#    white noise
# if the series was up or down , with drift .. then, we can add differences to make it stationary
autoplot(y_rw, value_d)
#display showing the changes after differences to make data set statrionary (required by ARIMA models) 
#first differencing of random walk
autoplot(y_rw, value) + 
  autolayer(y_rw,value_d)

# FIRST DIFFERENCING COMPLETES HERE
# NEXT, TESTING STATIONARITY


#  Indeed, this series is random and stationary 
#    because of how we established the random walk 
#    series in the first place
#  
#  There is a hypothesis test to check to see if 
#    series is stationary, known as the KPSS test
#    See section 8.1 in Hyndman for information 
#    on this
#
#  The null and alternative hypothesis for the 
#    KPSS test are
#  Ho:  Data is stationary
#  Ha:  Data is non-stationary
#
#
#KPSS - testing if new data is stationary
#install.packages("urca")
library(urca)
features(y_rw, value, unitroot_kpss)
features(y_rw, value_d, unitroot_kpss)
#
#  The result shows a value of the test 
#    statistic.  If this value is not 
#    greater than the critical values 
#    shown in the test, then the result
#    is fail to reject Ho.  
#
#  In one case we reject Ho (orginal random walk) 
#    and in the other case we fail to reject Ho if we 
#    use alpha = 0.05, becuase the differenced 
#    series is (much closer to) stationary
#


#
#  Part 1D:  Examining stationarity in Google and 
#    Alcohol Sales data
#
#  The Google data is examined first
#
autoplot(google, Open)
#
#  Clearly the data is non-stationary as the stock price 
#    is moving up, so the data will require differencing
#
#  Check the KPSS (unit-root) test first on the opening
#    prices
#
str(google)
features(google, Open, unitroot_kpss)
#when pvalue is low , we reject the data being stationary .. means our data is = nonstationary
#
#  A large value of the test statistic, beyond all
#    values given as critical, indicates we would
#    reject the null hypothesis, the Google price
#    series is not stationary
#
#  The "ndiffs" function provides insight into 
#    how many differences are required
#
features(google, Open, unitroot_ndiffs)
# ndiffs column output , means we only need to diff one time
#
#  The Google price series needs to be differenced
#    once.  Go ahead and do this
#
str(google)
google <- mutate(google, Open_d = difference(Open,1))
str(google) #checking again to check our new column , opend should be our stationary series to work with to fit the model
#
#  The differenced Google series, representing the 
#    change in prices looks pretty stationary
#
features(google, Open_d, unitroot_ndiffs) # open_d is now stationary ==> ndiff = 0
features(google, Open_d, unitroot_kpss)
#
#  Fail to reject the null hypothesis!
#  There is no evidence that the differenced
#    Google Stock Price data is non-stationary
#  
#
autoplot(google, Open) #nonstationary
autoplot(google, Open_d) #stationary
#
#
#
#
#
#  Consider now the Alcohol Sales Data and the 
#    transformed version of this data 
#
autoplot(AStsl, Sales)
autoplot(AStsl, Salesl)
#
#  Both series appear non-stationary, so to address this, 
#   we difference the data, that is we will focus the 
#   analysis on the change in Y versus the actual Y itself
#
str(AStsl)
# Unit Root Test on ndiffs
features(AStsl, Sales, unitroot_ndiffs)
features(AStsl, Salesl, unitroot_ndiffs)
str(AStsl)
#
#  The results of the ndiffs function indicate
#    that both of these series need to be 
#    "differenced" one time
#
#  Now add differenced series
# USING difference() to difference Sales where Yt - Yt-1
AStsld <- mutate(AStsl, Sales_d = difference(Sales))
AStsld <- mutate(AStsld, Salesl_d = difference(Salesl))
str(AStsld)
#
#  Recheck the "ndiffs" function
# making sure our data is stationary
features(AStsld, Sales_d, unitroot_ndiffs)
features(AStsld, Salesl_d, unitroot_ndiffs)
autoplot(AStsld, Sales_d)
autoplot(AStsld, Salesl_d)
# CONSIDER SEASONAL differencing .. when data has seasonality .. do the season has some kind of non stationary behavior , every Jan is lower or higher than Jan before

#  Both of the series appear stationary  
#  Now test for stationarity with KPSS
#
features(AStsld, Sales_d, unitroot_kpss)
features(AStsld, Salesl_d, unitroot_kpss)
#
#  No evidence that either series is non-stationary
#    as the P-value is not small
#
#
#  Series that are seasonal, may be experiencing 
#    non-stationary behavior with respect to 
#    seasonality.  This means that the season to 
#    season affect has a systematic movement.
#
#  This can be checked with the "unitroot_nsdiffs" 
#    command (number of "seasonal" differences)

# nsdiff vs ndiffs
# SEASONAL nonstationary consideration
features(AStsld, Sales_d, unitroot_nsdiffs)
features(AStsld, Salesl_d, unitroot_nsdiffs)
#
#  Even in the already differenced series, the data
#    may (does) require seasonal differencing
#
#
#  Thus, taking this advice, create the series with 
#    seasonal differences 
#  Take the seasonal differences of the already
#    differenced data
# 12 period lagg = considers seasonal data differences
AStsld <- mutate(AStsld, Sales_dd = difference(Sales_d, 12))
AStsld <- mutate(AStsld, Salesl_dd = difference(Salesl_d, 12))
str(AStsld)
# notice first 13 is missing
AStsld$Sales_dd
#  [1]    NA    NA    NA    NA    NA    NA    NA    NA    NA
# [10]    NA    NA    NA    NA   191   218   187  -214  -323
# [19

#  Here we look at the orginal data long with the differenced
#    and doubly differenced data
#  
autoplot(AStsld, Sales) +
  autolayer(AStsld, Sales_d, colour = "Red") +
  autolayer(AStsld, Sales_dd, colour = "Blue")
#
#  Then repeat that for the transformed data set
# log data
autoplot(AStsld, Salesl) +
  autolayer(AStsld, Salesl_d, colour = "Red") +
  autolayer(AStsld, Salesl_dd, colour = "Blue")
#
#  That's a strange picture, let's do it again with just
#    two differenced series, leaving out the transformed
#    data
#
autoplot(AStsld, Salesl_d, colour = "Red") +
  autolayer(AStsld, Salesl_dd, colour = "Blue")
#
#  Check the autocorrelation and partial autocorrelation
#    for these two doubly differenced series
#  
#  First the orginal Sales data differenced twice
#
ACF(AStsld, Sales_dd, lag_max = 20)
PACF(AStsld, Sales_dd, lag_max = 20)
autoplot(ACF(AStsld, Sales_dd, lag_max = 20))
autoplot(PACF(AStsld, Sales_dd, lag_max = 20))
#  
#  Now the transformed Sales data differenced twice
#
ACF(AStsld, Salesl_dd, lag_max = 20)
PACF(AStsld, Salesl_dd, lag_max = 20)
autoplot(ACF(AStsld, Salesl_dd, lag_max = 20))
autoplot(PACF(AStsld, Salesl_dd, lag_max = 20))
#
#  Both series are still showing autocorrelation
#  This is addressed in the next part, part 2 of 
#    this week's content.
#
#
features(AStsld, Sales_dd, unitroot_kpss)
features(AStsld, Salesl_dd, unitroot_kpss)
#
#
#  End of Forecasting with Time Series II Part 1 
#


