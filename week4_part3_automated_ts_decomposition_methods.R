#
#  Predictive Analytics (FE288): 
#  Introduction to Time Series
#  Part 3:  Automated Time Series Decomposition Methods 
#
#  Last updated:  5/18/22 (Murphy)
#
#  This script covers an introduction to decomposition procedures:  
#      a.  Classical Decomposition 
#             Additive and 
#             Multiplicative)
#      b.  X11, SEATS decomponsitions
#      c.  STL 
#
#
#  This content is found in Chapter 3 of 
#    Forecasting Principles and Practice (FPP) 3rd ed.
#
#
#  Use the monthly Alcohol Sales data from 2001 to 2018
#  Source:  https://fred.stlouisfed.org/graph/?id=S4248SM144NCEN,
#
#  This part of the presentation is on modeling Time Series
#    using ideas from decomposition
#
#  The analysis also relies on the package fpp3
#
#
#install.packages("fpp3")
library(fpp3)

#DECOMPOSITION METHODS : ADDITIVE AND APPLICATIVE
#
#  Read in and set up the data set
#
AlcSal <- read.csv("week4_alcohol_sales.csv")
AlcSal$Date <- as.Date(AlcSal$Date, format = "%m/%d/%Y")
AlcSal$Month <- as.factor(AlcSal$Month)
AlcSalts <- ts(AlcSal[,4], start = 2001, frequency = 12)
ASts <- as_tsibble(AlcSalts)

str(ASts)
names(ASts) <- c("Yr-Month", "Sales")
names(ASts)
rm(AlcSalts)


#
#  Before beginning decomposition it is useful to check
#    on the SD of Sales in the series.  Like in the case
#    of regression, one can think of this as the "total"
#    variation in the Alcohol Sales series
#
autoplot(ASts, Sales)
sd(ASts$Sales)
# SD MEASURE OF AVERAGE POINT FROM THE MEAN
#


#
#  Part 3A:  Automated Time Series Decomposition in R
#    Classical Decomposition
#
#  Now will use the built-in tools to be more sophisticated
#    with respect to time series decomposition.
#  
#  Classical Decomposition - DESCRIPTIVE METHOD TO UNDERSTAND WHAT IS GOING WITH DATA SET SERIES
#
#  This is additive classical decomposition
#
mod_cda <- model(ASts, classical_decomposition(Sales, 
                                               type = "additive"))
comp_cda <- components(mod_cda) #STORING RESULTS AS COMPONENTS
autoplot(comp_cda)
#
#  Let's check what we get from "components"
#
names(comp_cda)
ACF(comp_cda, random, lag_max = 20)
autoplot(ACF(comp_cda, random, lag_max = 20))
#
#



#
#  Multiplicative version of the classical decomposition
#
mod_cdm <- model(ASts, classical_decomposition(Sales, 
                                               type = "multiplicative"))
comp_cdm <- components(mod_cdm)
autoplot(comp_cdm)
#
#
#

#
#  Remember, we can think of the "random" component like an
#    error.  It isn't actually a model error, but we 
#    could interpret the left over part as the part of the 
#    series that could not be explained by "trend-cycle"
#    and "seasonal" effects
#
#  Let's compute the RMSE for the random component 
#    for the two models above
#
MSE_cda <- mean(comp_cda$random^2, na.rm = TRUE)
MSE_cda
RMSE_cda <- MSE_cda^0.5
RMSE_cda
MSE_cdm <- mean(comp_cdm$random^2, na.rm = TRUE)
RMSE_cdm <- MSE_cdm^0.5
RMSE_cdm
#
#  RMSE of Multiplicative decomposition is very low
#    Do you know why?
#
names(comp_cdm)
comp_cdm$random
#
#  Try computing the multiplicative error this way:
#
MSE_cdm1 <- mean((ASts$Sales - (comp_cdm$trend*comp_cdm$seasonal))^2, 
                 na.rm = TRUE) # A MORE TRUE RESIDUAL TO COMPARE
RMSE_cdm1 <- MSE_cdm1^0.5
#COMPARING THE TWO ON TREND AND SEASONAL EFFECT ... 
RMSE_cdm1
RMSE_cda
sd(ASts$Sales)
#
#  Multiplicative Decomposition actually worked better than 
#    Additive.  Both caputured a great deal of the signal in the
#    Alcohol Sales series
#






#
#  Part 3b:  The x11 and SEATS decomposition techniques
#
#  Read about these decompositions in H&A 3rd ed 
#    Section 3.5
#  These methods are used widely by government to 
#    understand signals in the economy
#

#
#  x11 and SEATS use the same command
#  First it is the x11 Decomposition
#
mod_x11 <- model(ASts, X_13ARIMA_SEATS(Sales ~ x11()))
comp_x11 <- components(mod_x11)
autoplot(comp_x11)
#
#  Let's compute a fitted value and calculate error
#
names(comp_x11)
comp_x11$irregular[1:10]
#
#  X11 decomposition appears to be a multiplicative
#    decomposition
#
#  How does he know that?
#
#  Let's compute the RMSE of the irregular component
#
err_x11 <- comp_x11$Sales - (comp_x11$trend*comp_x11$seasonal)
err_x11[1:15]
MSE_x11 <- mean(err_x11^2)
RMSE_x11 <- MSE_x11^0.5
RMSE_x11
sd(ASts$Sales)
#
#  Whoa!  That's low!  RMSE = 110
#

#
#  Now for the SEATs = Seasonal Extraction in ARIMA Time Series
#    decomposition method
#
mod_seats <- model(ASts, X_13ARIMA_SEATS(Sales ~ seats()))
comp_seats <- components(mod_seats)
autoplot(comp_seats)
#
#  Again, this appears to be a multiplicative decomposition
#
err_seats <- comp_seats$Sales - (comp_seats$trend*comp_seats$seasonal)
err_seats[1:15]
MSE_seats <- mean(err_seats^2)
RMSE_seats <- MSE_seats^0.5
RMSE_seats
#
#  Whoa!  Even lower!  RMSE = 100
#
#  So overall, it appears that x11 and SEATs
#    captures a great deal of the "signal"
#    in the Alcohol Sales series
#

#
#  Part 3c:  STL Decomposition technique
#    STL = Seasonal and Trend Decomposition using 
#      LOESS
#
#  The trend window is the number of periods used to 
#    estimate the trend
#  The seasonal window is the number of years used
#    to compute the seasonal effects ("periodic" means
#    use all the years)
#  These can be controlled by the user
#
mod_STL <- model(ASts, STL(Sales ~ trend(window = 17) + 
                             season(window = 5),
                           robust = TRUE))
comp_STL <- components(mod_STL)
autoplot(comp_STL)
#
#  Once again compute performance
#
MSE_STL <- mean(comp_STL$remainder^2, na.rm = TRUE)
RMSE_STL <- MSE_STL^0.5
RMSE_STL
#
#
#

#  
#  End - Introduction to Time Series Part 3
#