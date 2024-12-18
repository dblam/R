#
#  Predictive Analytics (FE288): 
#  Introduction to Time Series
#  Part 2:  Manual Time Series Decomposition
#
#  Last updated:  5/18/22 (Murphy)
#
#  This script covers:
#  1.  A model for decomposing time series:  Trend-Cycle, Seasonal, 
#      and Remainder
#  2.  Decomposition by hand:  
#        a.  Identifying the Trend-Cycle component
#        b.  Identifying the Seasonal component
#  3.  Review the purpose of decomposition
#
#  This content is found in Chapter 3 of 
#    Forecasting Principles and Practice (FPP) 3rd ed.
#
#  Use the monthly Alcohol Sales data from 2001 to 2018
#  Source:  https://fred.stlouisfed.org/graph/?id=S4248SM144NCEN,
#
#  This part of the presentation is on modeling Time Series
#    using ideas from decomposition
#
#  The analysis also relies on the package fpp3
#
#install.packages("fpp3")
library(fpp3)



#
#  Part 2A:  Read in the Alcohol Sales data
#    and set up the data set as a tsibble
#
#
#  Recall the time series decomposition is:
#    Yt = Tt + St + Rt
#      Yt = Sales at time t
#      Tt = Trend-Cycle at time t
#      St = Seasonal Effect at time t
#      Rt = Remainder at time t  
#
AlcSal <- read.csv("week4_alcohol_sales.csv")
AlcSal$Date <- as.Date(AlcSal$Date, format = "%m/%d/%Y")
AlcSal$Month <- as.factor(AlcSal$Month)
AlcSalts <- ts(AlcSal[,4], start = 2001, frequency = 12)
ASts <- as_tsibble(AlcSalts)
names(ASts) <- c("Yr-Month", "Sales")
names(ASts)
str(ASts)




#
#  Start out by decomposing the Alcohol Sales data 
#    using an automated (STL) procedure to see what 
#    that does
#
# using decomposition tool
dcmp <- ASts 
mod1 <- model(dcmp, stl = STL(Sales)) # model() builds a model while passing data = dcmp , stl = STL() model 
components(mod1)[1:13,] #components of model , set of columns created
comp1 <- components(mod1) #comp1 = 7 variables represented by previous line
autoplot(comp1) 
#graphing components of comp1 STL decomp : trend (flatspot in middle) + season_year (seasonal component adjust to seasonal effects) + Remainder (what is left over) .. bottom 3 graphs add up to top graph
names(comp1)
# proving 3 components add up to sales
comp1$Sales[1:15]
comp1$trend[1:15]
comp1$season_year[1:15]
comp1$remainder[1:15]
comp1$trend[1] + comp1$season_year[1]+comp1$remainder[1]
comp1$Sales[1]
#
#
#
#






#
#  Part 2B:  Decompose the Alcohol Sales manually 
#     by first addressing the trend component
#
#  MOVING AVERAGES
# The technique used is via moving averages.  A moving
#    average is simply an average over some time window
#    in the data set. slow => over time
#
#  Since moving average forecasts move when the data 
#    moves, it makes sense to use to measure the upward
#    or downward trend of the data.  It is also more 
#    flexible as it will adjust.
#
#  Often, moving averages (or order k) use the current 
#    period and k-1 periods back in time, to forecast 
#    next period.  
#    
#    Here, we use "centered" moving averages
#    as they are better suited for knowing where we are 
#    today in the decomposition process
#
#  Recall the time series decomposition is:
#    Yt = Tt + St + Rt
#      Yt = Sales at time t
#      Tt = Trend-Cycle at time t
#      St = Seasonal Effect at time t
#      Rt = Remainder at time t  
#
#  We will use a moving average to estimate the trend
#    at time t, Tt
#
#
#  In this command a centered moving average of order 5 
#    is computed and added to the data set
#
ASts1 <- mutate(ASts, 
                    MA5 = slider::slide_dbl(Sales,mean,.before = 2, 
                         .after = 2,complete = TRUE))
#slider slides a window open around data .. 5 periods to comput the average
head(ASts1)
mean(ASts1$Sales[1:5])
mean(ASts1$Sales[2:6])
#
#  The autoplot command can be used here
#
autoplot(ASts1a, Sales) + 
  geom_line(aes(y = MA5), color = "Red")
#
#  Lets try another value of k for this moving average
#    How about k = 13?
#
ASts1b <- mutate(ASts1a, 
                MA13 = slider::slide_dbl(Sales,mean,.before = 6, 
                                        .after = 6,complete = TRUE))
autoplot(ASts1b, Sales) + 
  geom_line(aes(y = MA13), color = "Violet")
#
#  How about k = 21?
#
ASts1c <- mutate(ASts1b, 
                 MA21 = slider::slide_dbl(Sales,mean,.before = 10, 
                                         .after = 10,complete = TRUE))
autoplot(ASts1c, Sales) + 
  geom_line(aes(y = MA21), color = "Green")
#
#
autoplot(ASts1c, Sales) + 
  geom_line(aes(y = MA21), color = "Green") +
  geom_line(aes(y = MA13), color = "Violet") +
  geom_line(aes(y = MA5), color = "Red")




#
#  Now let's create a column where we remove the 
#    trend-cycle effect in orginal data set
#
#  The new variable "Detrend" is the alcohol 
#    sales series without the trend-cycle component
#
#  Add it to the data set and plot
#
ASts2 <- mutate(ASts1c, Detrend = Sales - MA13)
autoplot(ASts2, Detrend)




#
#  Part 2C:  Further decompose the Alcohol Sales manually 
#     by first addressing the seasonal component
#
#  To address the seasonal component, we'll do this 
#    in a naive way by just calculating the average
#    for all values in a given season using this
#    new "detrended" data series
#
#
#  Set up to compute the seasonal averages
#    The "rep" command just means "repeat"
#
seastot <- rep(0, 12)
j <- 1
#
#  Now run a loop over all the alcohol sales 
#    data that computes the totals for each
#    of the 12 "seasons", that is, months
#
for (i in 1:216) {
  seastot[j] <- seastot[j] + ASts2$Detrend[i]
  j <- j + 1
  if (j == 13) j <- 1  
}
#
#  Use the season totals to compute seasonal averages
#    Set up the array for the data set
#    Again first three months and last three months
#      have one less year of data
#
seasavg <- seastot/18
seasavg
#
#  Now use the 12 seasonal averages to build an 
#    array of 216 observations where the seasonal average
#    just repeats 18 times.
#
seasonalvals <- rep(seasavg, 18)
seasonalvals[1:20]
#
#  Add the new variable to the data set and plot
#
ASts3 <- mutate(ASts2, Seasonal = seasonalvals)
str(ASts3)
autoplot(ASts3, Seasonal)
# 
#  Really exciting!  j/k
#
#
#  Now we will add a second new variable called
#    DeTrndSeas.  This additional new variable is the 
#    de-trended and de-seasonalized data
#
ASts4 <- mutate(ASts3, DeTrndSeas = Detrend - seasonalvals)
#
#  Let's plot that
#
autoplot(ASts4, DeTrndSeas)
#
#  There still appears to be autocorrelation in this series
#
#  Why is that important?
#
#  Let's take a look
#
ACF(ASts4, DeTrndSeas, lag_max = 20)
ACF_DTS <- ACF(ASts4, DeTrndSeas, lag_max = 20)
autoplot(ACF_DTS)
#
#  Actually, there is still a ton of autocorrelation
#
#
#  Let's finish by graphing everything
#
names(ASts4)
autoplot(ASts4, Sales) + 
  geom_line(aes(y = MA5), color = "Red") +
  geom_line(aes(y = Detrend), color = "Blue") +
  geom_line(aes(y = Seasonal), color = "Green") +
  geom_line(aes(y = DeTrndSeas), color = "Brown")


#  
#  End - Introduction to Time Series Part 2
#