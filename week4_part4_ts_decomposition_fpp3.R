#
#  Predictive Analytics (FE288) 
#  Super Saturday Week 4 In-Class R Script: 
#  Part 4:  Time Series Decomposition (automatic)
#  Last updated:  10/25/24 
#
#
#  This script covers:
#  1.  An introduction to time series decomposition procedures:  
#      a.  Classical
#      b.  X11
#      c.  SEATS
#      d.  STL
#  
#  Time Series decomposition is a descriptive method that is used
#    to understand the components of a time series
#
#
#  Use the monthly Alcohol Sales data from 2001 to 2024
#  Source:  https://fred.stlouisfed.org/graph/?id=S4248SM144NCEN,
#
#  The analysis also relies on the package fpp3
#
#
#install.packages("fpp3")
library(fpp3)
#


#
#  Read in and set up the data set
#
AlcSal <- read.csv("week4_alcohol_sales.csv")
AlcSal$Date <- as.Date(AlcSal$Date, format = "%m/%d/%Y")
AlcSal$Month <- as.factor(AlcSal$Month)
AlcSalts <- ts(AlcSal[,4], start = 2001, frequency = 12)
ASts <- as_tsibble(AlcSalts)
names(ASts) <- c("Yr-Month", "Sales")
names(ASts)
#




#
#  Time Series Decomposition in R
#
#  Now will use the built-in tools to be more sophisticated
#    about the decompositions.
#  
#
#  Classical Decomposition
#
#  This is additive classical decomposition
#
mod_cda <- model(ASts, classical_decomposition(Sales, 
                                               type = "additive"))
comp_cda <- components(mod_cda)
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
#  Note, we can think of the "random" component like an
#    error.  It isn't actually a model error, but we 
#    could interpret it that way....
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
#  Compute "residuals" which are equal to 
#    Yt - Trendt * Seasonalt
#
res_cdm <- ASts$Sales - (comp_cdm$trend*comp_cdm$seasonal)
#
MSE_cdm2 <- mean(res_cdm^2, na.rm = TRUE)
RMSE_cdm2 <- MSE_cdm2^0.5
RMSE_cdm2
#
#  Which "worked" better Additive or Multiplicative
#


#
#  Now the x11 and SEATS decompostions
#
#  Read about these decompositions in H&A 3rd ed 
#    Section 3.5
#  These methods are used widely by government to 
#    understand signals in the economy
#
#  An additional package is needed here: "seasonal"
#
#install.packages("seasonal")
library(seasonal)
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
#
#  Whoa!  That's low!  RMSE = 110
#  Remember seasonal regression was 515!
#



#
#  Seats Decomposition
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
#  Remember seasonal regression was 515!
#


#  
#  STL Decomposition
#
mod_stl <- model(ASts, STL(Sales ~ trend(window = 13) + 
                             season(window = "periodic"),
                           robust = TRUE))
comp_stl <- components(mod_stl)
autoplot(comp_stl)
#
#  Let's check what we get from "components"
#
names(comp_stl)
ACF(comp_stl, remainder, lag_max = 20)
autoplot(ACF(comp_stl, remainder, lag_max = 20))
#
#  This appears to be an additive decomposition
#  Let's compute error
#
MSE_stl <- mean(comp_stl$remainder^2, na.rm = TRUE)
MSE_stl
RMSE_stl <- MSE_stl^0.5
RMSE_stl
#
#
#


#  
#  End Super Saturday - Week 4 Part 4 
#