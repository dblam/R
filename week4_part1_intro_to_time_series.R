#
#  Predictive Analytics (FE288): 
#  Introduction to Time Series
#  Part 1:  Basics of Data, Plots and Signals
#
#  Last updated:  5/18/22 (Murphy)
#
#  This script covers:
#  1.  Introduction to the FPP3 Package
#  2.  Time series effects:  Trend, Seasonality, Cycles
#  3.  tsibble objects
#  4.  Some basic time series graphics
#  5.  An introduction to autocorrelation
#
#  This content is found in Chapter 2 of 
#    Forecasting Principles and Practice (FPP) 3rd ed
#
#  The script Uses data from the "fpp3" package  
#    and
#  Use the monthly Alcohol Sales data from 2001 to 2018
#  Source:  https://fred.stlouisfed.org/graph/?id=S4248SM144NCEN,
#
#
#  Part 1A:  FPP3 Library and Data
#
#  The book Forecasting Priniciples and Practice (FPP)
#    in its 3rd edition has an associated library for 
#    R known as "fpp3".  This will be heavily utilized
#    during the time series analysis portion of the course
#
#install.packages("fpp3")
library(fpp3)
#
#
#
#  The fpp3 library contains a number of data sets
#  To see them, use:
#
data()
#
#  Stock prices of GAFA companies
#
?gafa_stock 
gstock <- gafa_stock
str(gstock)
#
#  Admissions at UC Berkeley
#
?UCBAdmissions
dat_UCB <- UCBAdmissions
str(dat_UCB)
#
#  US Economic monthly data from 1967 to 2015
#
?economics
dat_econ <- economics
dat_econ[1:10,]
dat_econ[565:574,]
str(dat_econ)
plot(dat_econ)
plot(dat_econ$date,dat_econ$pce)
cor(dat_econ[,2:6])
#
#  (Old) Air Passenger data
#     Evidently a classic data set
#
?AirPassengers
AP <- AirPassengers
str(AP)
#
#  Set up this data as a time series "tibble"
#  What is a "tibble"?
#  See:  https://tibble.tidyverse.org/
#
#
AP_ts <- as_tsibble(AP, index = Month)
str(AP_ts)
names(AP_ts)
names(AP_ts) <- c("Year-Mnth", "Passengers")
#AP_ts <- mutate(AP_ts, Passengers = value)
autoplot(AP_ts, Passengers)
autoplot(AP_ts, Passengers) +
  labs(title = "Air Passengers over 1949-1960",
       y = "Passengers (in 1000s)")





#
#  Part 1B:  The Alcohol Sales time Series
#
AlcSal <- read.csv("week4_alcohol_sales.csv")
#
#
names(AlcSal)
str(AlcSal)
head(AlcSal)
#
#  Change the data types
#
AlcSal$Date <- as.Date(AlcSal$Date, format = "%m/%d/%Y")
str(AlcSal)
AlcSal$Month <- as.factor(AlcSal$Month)
str(AlcSal)
head(AlcSal)
#
#  Plot the sales in time (using base R)
#
plot(AlcSal$Date, AlcSal$Sales, xlab = "Month/Year", 
     ylab = "Sales")
lines(AlcSal$Date, AlcSal$Sales, type = "l")
# see : trend , seasonality , december&june=high sales , jan = low sales

#  As an alternative plot the Sales data using
#    ggplot
#
ggplot(AlcSal) +
  aes(x = Date, y = Sales) +
  geom_point() +
  geom_line()
#
#  What "signals" are observed?
# trend , pattern of seasonality , predictable pattern in high and low sales

#
#  Convert Alcohol Sales series to a Time Series or
#    tsibble object.  This is done in two steps
#
AlcSalts <- ts(AlcSal[,4], start = 2001, frequency = 12) #convert 4th column to a tsibble .. 
AlcSalts[1:10]
str(AlcSalts)
str(AlcSal)


#
#  Convert the Alcohol Sales time series object 
#    to a "tsibble" object
#
ASts <- as_tsibble(AlcSalts)
str(ASts) #index = month , value = sales
names(ASts)
names(ASts) <- c("Yr-Month", "Sales") #changing index and value 'names'


#
#  Plot the sales in time using autoplot, a command 
#    built into package FFP3
#
autoplot(ASts,Sales) +
  ggtitle("Monthly Alcohol Sales 2001 - 2018") +
  ylab("Sales in $1Ms") +
  xlab("Year")
#

#
#  A couple of other interesting plots that show the 
#    signals in a Time Series
#
#  This one plots by season, here the months.
#
gg_season(ASts, Sales) +
  ggtitle("Alcohol Sales by Month 2001 - 2018") +
  ylab("Sales in $1Ms") +
  xlab("Year")
#  
#  The plot shows each year with a different color
#    and the months are on the x-axis.
#  The annual trend is really apparent as well as the
#    month to month correlation (seaonality)
#
#
#  This plot is another one that shows seasonality,
#    but it displays it in a different way.  It is 
#    called a polar plot
# #polar option = TRUE
gg_season(ASts, polar = TRUE) +
  ggtitle("Alcohol Sales by Month 2001 - 2018") +
  ylab("Sales in $1Ms") +
  xlab("Year")
#
#  Similar result to the other seasonal plot, just in
#    circular display
#

#
#  Part 1C:  Lags Plots and Autocorrelation
#
#
#  Lag plots are useful for checking autocorrelation
#  
#  Autocorrelation is correlation of a series with itself
#    in time, that is, with past observations.
# looking back to the last time , it was this month to see behaviors
# autocorrelation , in time, relationship between last time and this time

# lag of 1 .. relationship between this and 1, 2 , 3 .. 12 periods ago
#consider data that are trending with time
gg_lag(ASts, Sales) 
gg_lag(ASts, Sales, lags = 1:12)
#
#  The more closely the colored line hug the 45 degree
#    line, the more correlated the data is with itself
#    L periods in the past.  
#
#  For example, consider the lag 12 plot.  There is a 
#    strong relationship of the data with the observation
#    12 periods ago.  Not surprising as this his monthly
#    data showing 12 period seasonality
#


#
#   Take a look at the autocorrelation function 
#     This function looks at the correlation of Yt with
#     Yt-j for j = 1, 2, 3, etc.
#
#   If Yt is correlated with Yt-j for j > 0, we say that 
#     Y is autocorrelated
#
#   There are some dotted lines to show when the correlation
#     is significantly different from 0
#
#computing values for autocorrelation .. correlation of the series with it self when you lag series by 1 time period .. how are they correlated
ACF(ASts, Sales, lag_max = 20)
# 12'th period correlation is high 12M 0.827
autoplot(ACF(ASts, Sales, lag_max = 20))
#nesting output of autocorrelation and plotting them
#blue dotted lines = above = statistically significant
#
#  There is pretty significant autocorrelation in the 
#    alcohol sales data.
#
#  This chart is not surprising for data that is trending
#    and has seasonal components. 
#
#  See Hyndman 3rd, Chapter 2 Section 8 for a discussion 
#    on this.
#
#  Autocorrelation violates basic assumptions in regression
#
#
#  Let's compare those results to some white noise!
#  What is "white noise"? series that is not correlated with time .. no relation between next observation and observations from the past
#  See:  Section 2.9 in H&A 3rd Ed.
#
rnorm(100, 100, 20) # generate 100 random normal values , mean 100 , standard deviation 20
set.seed(123654) #random seed
y <- tsibble(sample_num = 1:100, wn = rnorm(100), index = sample_num)
names(y)
str(y)
autoplot(y, wn) + ggtitle("White Noise")
ACF(y, wn, lag_max = 20) #correlation values = small = close to 0
autoplot(ACF(y, wn, lag_max = 20)) #bars are inside blue dotted line , no proveable autocorrelation in data series
#
#  White noise shows no (very very little) autocorrelation
#. clean data for model while pulling out white noise that has no correlation significance.

#  
#  End - Introduction to Time Series Part 1
#
