#
#  Predictive Analytics (FE288) 
#  Super Saturday Week 4 In-Class R Script: 
#  Part 2:  Introduction to FPP3 and Time Series Charts
#  Last updated:  10/25/24 
#
#
#  This script covers:
#  1.  Introduction to the FPP3 Package
#  2.  tsibble objects
#  3.  Time series effects:  Trend, Seasonality, Cycles
#  4.  Some basic time series graphics
#  5.  An introduction to autocorrelation
#
#
#  Use the monthly Alcohol Sales data from 2001 to 2024
#  Source:  https://fred.stlouisfed.org/graph/?id=S4248SM144NCEN  
#
#  Also, we'll use data from the "fpp3" package  
#
#
#install.packages("fpp3")
library(fpp3)
#





#
#  In fpp3 there are many data sets
#
data()
#
#  For example gafa_stock
#
gstock <- gafa_stock
?gafa_stock  
#
#  UC Berkeley Admissions Data
# 
?UCBAdmissions
dat_UCB <- UCBAdmissions
str(dat_UCB)
#




#
#  Look at some Economic data
#
?economics
dat_econ <- economics
str(dat_econ)
plot(dat_econ)
cor(dat_econ[,2:6])
#

#
#  (Old) Air Passenger data
#     Evidently a classic data set
#
?AirPassengers
dat_AP <- AirPassengers
str(dat_AP)
dat_AP1 <- as.data.frame(dat_AP)
names(dat_AP1)
str(dat_AP1)
#
#  Plot the data
#
plot(dat_AP1)
dat_AP2 <- as_tsibble(dat_AP, index = Month)
autoplot(dat_AP2)
#



#
#  Now read the Alcohol Sales data set
#
AlcSal <- read.csv("week04_alcohol_sales_for_Saturday_for_Mac.csv")
#
names(AlcSal)
str(AlcSal)
head(AlcSal)
#

#
#  Change the data types
#
AlcSal$Date <- as.Date(AlcSal$Date, format = "%m/%d/%Y")
AlcSal$Month <- as.factor(AlcSal$Month)
str(AlcSal)
head(AlcSal)
#
#  Plot the sales in time
#
plot(AlcSal$Date, AlcSal$Sales, xlab = "Month/Year", 
     ylab = "Sales")
lines(AlcSal$Date, AlcSal$Sales, type = "l")
#
#  As an alternative plot the Sales data using
#    ggplot
#
ggplot(AlcSal) +
  aes(x = Date, y = Sales) +
  geom_point() +
  geom_line()




#
#  Convert Alcohol Sales series to a Time Series object
#    using the "ts" command
#
AlcSalts <- ts(AlcSal[,4], start = 2001, frequency = 12)
AlcSalts[1:10]
str(AlcSalts)
str(AlcSal)
#
#  Convert the Alcohol Sales time series object 
#    to a "tsibble" ojbect
#
ASts <- as_tsibble(AlcSalts)
str(ASts)
names(ASts)
names(ASts) <- c("Yr-Month", "Sales")
names(ASts)
#
#

#
#  Plot the sales in time using autoplot, a command 
#    built into package FFP3
#
autoplot(ASts,Sales) +
  ggtitle("Monthly Alcohol Sales 2001 - 2024") +
  ylab("Sales in $1Ms") +
  xlab("Year")
#

#
#  A couple of other interesting plots that show the 
#    signals in a Time Series
#
#  This one plots by season, here the months.
#
gg_season(ASts) +
  ggtitle("Alcohol Sales by Month 2001 - 2024") +
  ylab("Sales in $1Ms") +
  xlab("Year")
#  
#  The plot shows each year with a different color
#    and the months are on the x-axis.
#  The annual trend is really apparent as well as the
#    month to month correlation (seaonality)
#
#  This plot is another way to display seasonality,
#    but it displays it in a different way.  It is 
#    called a polar plot
#
gg_season(ASts, polar = TRUE) +
  ggtitle("Alcohol Sales by Month 2001 - 2018") +
  ylab("Sales in $1Ms") +
  xlab("Year")
#
#  Similar result to the other seasonal plot, just in
#    circular display
#
#  What do you observe?
#





#
#  Lag plots are useful for checking autocorrelation
#  
#  Autocorrelation is correlation of a series with itself
#    in time, that is, with past observations.
#
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
#  Autocorrelation --> Important in Time Series  
#
#  Take a look at the autocorrelation function 
#     This function looks at the correlation of Yt with
#     Yt-j for j = 1, 2, 3, etc.
#
#   If Yt is correlated with Yt-j for j > 0, we say that 
#     Y is autocorrelated
#
#   There are some dotted lines to show when the correlation
#     is significantly different from 0
#
ACF(ASts, Sales, lag_max = 20)
autoplot(ACF(ASts, Sales, lag_max = 20))
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
#  What is "white noise"?
#
rnorm(10,100,20)
#random normalized number  10 observation , 100=mean , 20=std deviation
set.seed(999999)
y <- tsibble(sample = 1:500, wn = rnorm(500), index = sample)
names(y)
str(y)
autoplot(y, wn) + ggtitle("White Noise")
ACF(y, wn, lag_max = 20)
autoplot(ACF(y, wn, lag_max = 20))
#
#  White noise shows very little (no!) autocorrelation
#



#  
#  End Super Saturday - Week 4 Part 2 
#
