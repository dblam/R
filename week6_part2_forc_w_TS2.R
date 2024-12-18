#
#  Predictive Analytics (FE288):
#  Forecasting with Time Series II:  
#  Part 2:  ARIMA Modeling - Fitting and Forecasting 
#    ARIMA Models
#  
#  Last updated:  6/19/22
# 
#   BUILDING ARMA & ARIMA MODELS
#
#  This script covers:   
#  A.  Generate some AR and MA models (for fun!)
#  B.  Fitting non-seasonal ARMA models - computing 
#      errors and checking AICs
#  C.  Fitting seasonal ARMA models
#  D.  Putting it all together with automated ARIMA
#      modeling and forecasting with ARIMA models
#
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
#  This script covers items 3 and 4 above
#
#  The script here assumes that a stationary 
#    series has been obtained (See part 1 of this
#    content)
#
#  Commands to set up the data are included for 
#    completeness below
#
#install.packages("fpp3")
library(fpp3)
#
#  Set up the Alcohol Sales data as it was in Part 1
#
#  The Alcohol Sales is doubly differenced:
#    Single period differenced and 
#    12 period or seasonally differenced
#
#  We analyze the doubly differenced series, and
#    because the data was also transformed, using
#    the Box-Cox transforamtion, we analyze for the
#    "best" model using both the regular series
#    differences and the transformed series differences
#
#    The standard data is in series:  Sales_dd
#    The transformed data is in series:  Salesl_dd
#
AlcSal <- read.csv("alcohol_sales.csv")
AlcSal$Date <- as.Date(AlcSal$Date, format = "%m/%d/%Y")
AlcSalts <- ts(AlcSal[,4], start = 2001, frequency = 12)
ASts <- as_tsibble(AlcSalts)
names(ASts)[2] <- "Sales"
lambda <- features(ASts, Sales, features = guerrero)
AStsld <- mutate(ASts, Salesl = box_cox(Sales, lambda))
AStsld <- mutate(AStsld, Sales_d = difference(Sales))
AStsld <- mutate(AStsld, Salesl_d = difference(Salesl))
AStsld <- mutate(AStsld, Sales_dd = difference(Sales_d, 12))
AStsld <- mutate(AStsld, Salesl_dd = difference(Salesl_d, 12))
str(AStsld)
#
#
#
#  Google data set up
#
google <- filter(gafa_stock, Symbol == "GOOG", 
                 year(Date) >= 2015)
google <- mutate(google, day = row_number())
google <- update_tsibble(google, index = day, regular = TRUE)
#
#

#
#  Part 2A:  To gain insight into ARIMA models,
#    we will build a few first.
#
# Building ARIMA models from scratch
#
#  There are several reasons to do this
#  a. Demonstrate what ARMA models look like - they look
#    random, but do have structure
#  b. Demonstrate the ACF and PACF functions, and how 
#    they can act as diagnostics for determining the 
#    order of the ARMA model
#
#
#  First build a 200 observation white noise series
#    Much of the rest of the series building is based
#    on this sereis
#
# Creating a random white noise series .. 
set.seed(333222)
wn_time <- ts(data.frame(rnorm(200)))
autoplot(as_tsibble(wn_time), value)
features(as_tsibble(wn_time), value, box_pierce, lag = 10, dof = 0)
#
#  There is no autocorrelation in white noise!
#
#
#  Simulate some Autoregressive (AR) Models
#
#
#  AR1 Model with parameter phi1 = 0.2 , no moving average , no differencing
#
ysim1 <- ts(data.frame(matrix(rep(0),200,1)))
ysim1[1,1] <- wn_time[1]
#similar to random walk , instead of one , we use fraction of one
for (i in 2:200) {
  ysim1[i,1] <- 0.2*ysim1[i-1,1] + wn_time[i] #relationship with the past is built here
}
ysim1 <- as_tsibble(ysim1)
str(ysim1)
autoplot(ysim1, value) + 
  xlab("Observations in Time") +
  ylab("AR1 Model with Phi = 0.2")
#
#  Let's check auto-correlation
#
ACF(ysim1, value)
autoplot(ACF(ysim1, value))
PACF(ysim1, value)
autoplot(PACF(ysim1, value))
# thus PACF is showing the autocorrelation terms, a singl strong bar, indicates there is likely 1 AR term we can use to fit this data

#  Note the patterns present in the Acf and Pacf 
#    plots
#  Pacf shows 1 bar indicating AR1 model
#
#
#  Now try AR1 with phi1 = 0.8
#
ysim2 <- ts(data.frame(matrix(0,200,1)))
ysim2[1,1] <- wn_time[1]
for (i in 2:200) {
  ysim2[i,1] <- 0.8*ysim2[i-1,1] + wn_time[i] 
}
ysim2 <- as_tsibble(ysim2)
str(ysim2)
autoplot(ysim2, value) + 
  xlab("Observations in Time") +
  ylab("AR1 Model with Phi = 0.8") 
#auto correlation is stronger at 0.8 than 0.2
autoplot(PACF(ysim2, value)) #stronger bar in 0.8 is shown
autoplot(ACF(ysim2, value))
#
#
#  AR2 Model with parameters 
#    phi1 = 0.8 and phi2 = -0.3
#
ysim3 <- ts(data.frame(matrix(rep(0),200,1)))
ysim3[1,1] <- wn_time[1]
ysim3[2,1] <- wn_time[2]
# using 0.8 and 0.3
for (i in 3:200) {
  ysim3[i,1] <- 0.8*ysim3[i-1,1] + 
    - 0.3*ysim3[i-2,1] + wn_time[i] 
}
ysim3 <- as_tsibble(ysim3)
autoplot(ysim3, value) + 
  xlab("Observations in Time") +
  ylab("AR1 Model with Phi1 = 0.8, Phi2 = -0.3")
#
#
#  Lets check Acf and Pacf for this last one
#
ACF(ysim3, value)
autoplot(ACF(ysim3, value))
PACF(ysim3, value)
autoplot(PACF(ysim3, value))
#
#
#  Pacf shows 2 bars indicating AR1 and AR2 
#    models
#
#  Note how the Pacf function points to the AR
#    terms present in the series
#
#  Now try AR2 with phi1 = 0.8 and phi2 = 0.1
#
ysim4 <- ts(data.frame(matrix(rep(0),200,1)))
ysim4[1,1] <- wn_time[1]
ysim4[2,1] <- wn_time[2]
# note phi1 and phi2 need to add up to a number less than 1 .. meaning .. 0.8 and 0.4 would not work...!
for (i in 3:200) {
  ysim4[i,1] <- 0.8*ysim4[i-1,1] + 
    + 0.1*ysim4[i-2,1] + wn_time[i] 
}
ysim4 <- as_tsibble(ysim4)
autoplot(ysim4, value) + 
  xlab("Observations in Time") +
  ylab("AR1 Model with Phi1 = 0.8 and Phi2 = 0.1")
#
#
#  Simulate Moving Average (MA) models
#
#  MA1 Model with theta = 0.8
#
ysim5 <- ts(data.frame(matrix(rep(0),200,1)))
ysim5[1,1] <- wn_time[1]
#adding the last period error instead of adding last period Y
for (i in 2:200) {
  ysim5[i,1] <- wn_time[i] + 0.8*wn_time[i-1] 
}
ysim5 <- as_tsibble(ysim5)
autoplot(ysim5, value) + 
  xlab("Observations in Time") +
  ylab("AR1 Model with Theta = 0.8")
#
#  Check Acf and Pacf
# MA1 model .. one strong bar in the 
# ACF == predicts moving average models
ACF(ysim5, value)
autoplot(ACF(ysim5, value))
PACF(ysim5, value)
autoplot(PACF(ysim5, value))
#
#  Acf is showing 1 bar --> indicates MA1 model
#
#  
#   MA2 Model with parameters
#     theta1 = 0.6 and theta2 = -0.4
#
ysim6 <- ts(data.frame(matrix(rep(0),200,1)))
ysim6[1,1] <- wn_time[1]
ysim6[2,1] <- wn_time[2]
for (i in 3:200) {
  ysim6[i,1] <- wn_time[i] + 0.6*wn_time[i-1] - 0.4*wn_time[i-2] 
}
ysim6 <- as_tsibble(ysim6)
autoplot(ysim6, value) + 
  xlab("Observations in Time") +
  ylab("AR1 Model with Theta1 = 0.8 and Theta2 = -0.4")
#
#  Check Acf and Pacf
#
ACF(ysim6, value)
autoplot(ACF(ysim6, value))
PACF(ysim6, value)
autoplot(PACF(ysim6, value))
#
#  Acf now showing 2 bars --> MA2
#

# TAKE AWAYS..

# IF : the ACF is trailing off but significant , PACF has 1 or 2 strong bar 
#     ==> we have a AR1 or AR2 Model

# IF : the PACF is trailing off but significant , ACF has 1 or 2 strong bar 
#     ==> we have a MA1 or MA2 Model



#
#  Part 2B:  ARMA Modeling 
#
#  Here we begin Alcohol Sales series using the doubly
#    differenced data
#  
#  Start out by checking the autocorrelation and 
#    partial autocorrelation for these two doubly 
#    differenced series, Alcohol Sales and transformed
#    Alcohols sales by taking logs
#
#  The "ggtsdisplay" command is showing the ACF and PACF 
#    functions for our data.
#
str(AStsld)
gg_tsdisplay(AStsld, Sales_dd)
gg_tsdisplay(AStsld, Salesl_dd)
#
#  Both series indicate AR2 models
#  Although there are some other bars on the Pacf
#    diagram that are significant
#
#  Start out by running a general ARMA model on the 
#    non-transformed Alcohol Sales differenced data
#
?ARIMA
fit_1 <- model(AStsld, ARIMA(Sales_dd))
report(fit_1)
#
# performance indicator .. AICc=3010.85 
# Coefficients:
#         ar1      ar2
#       -1.0037  -0.6660
# s.e.   0.0519   0.0515

#  Let's check the statistics
#
#  Here are the fitted values:
#
augment(fit_1)
aug_1 <- augment(fit_1)
aug_1$.resid


#
#
#  Try a few other models, AR1 and AR2 and ARMA(1,1)
#    report - summarizes model results
#    glance - 
#
fit_2 <- model(AStsld, ASa1 = ARIMA(Sales_dd ~ pdq(1,0,0)),
               ASa2 = ARIMA(Sales_dd ~ pdq(2,0,0)),
               ASa1m1 = ARIMA(Sales_dd ~ pdq(1,0,1)),
               search = ARIMA(Sales_dd)
               )
report(fit_2)
report(select(fit_2,search))
glance(fit_2)
#
#  The AIC is lower for the first model, fit_ts1
#  Remember this trades off the fit versus the 
#    model complexity
#
#  The original, AR2 model fits the doubly differenced
#    Alcohol Sales data the best so far according the
#    Arima criteria
#
#  Let's check the residuals
#
gg_tsresiduals(select(fit_2,search))
# 
#Evaluate: assumptions residuals are not : trending , correlated in time , no pattern , no auto correlation
#
#  Now address the transformed Alcohol Sales data. 
#  Based on earlier info from the Pacf graph, begin
#    with AR2 model here also
#
fit_2l <- model(AStsld, ASa1 = ARIMA(Salesl_dd ~ pdq(1,0,0)),
                ASa2 = ARIMA(Salesl_dd ~ pdq(2,0,0)),
                ASa1m1 = ARIMA(Salesl_dd ~ pdq(1,0,1)),
                search = ARIMA(Salesl_dd)
               )
report(fit_2l)
glance(fit_2l)
report(select(fit_2l,ASa2))
#
#
#  This model is more complicated with the 
#    transformed data
#  More on this later
#




#
#  Part 2C:  Seasonal Arima Modeling
#
#  These models are described in H&A Section 9.9
#
#  The seasonal ARIMA model has 2 ARMA components, 
#    one as described previously and 
#    the second one describes the seasonal effects.  
#
#  The form of the model is ARIMA(p,d,q)(P,D,Q)[m]
#    where 
#    p = # AR terms, 
#    d = # of differences and 
#    q = # MA terms;
#CAPITAL TERMS = SEASONAL
#    P = # AR seasonal terms, 
#    D = # of seasonal differences and
#    Q = # MA seasonal terms.
#    m = period of the seasonality
#
#  Again start with both the regular data and then 
#    analyze the transformed data
#
#  Note that the "seasonal = FALSE" modifier has 
#    been removed before running auto.arima
#
fit_3 <- model(AStsld,
               ASA0 = ARIMA(Sales_dd ~ pdq(0,0,0) + PDQ(0,0,0)),
               ASA1 = ARIMA(Sales_dd ~ pdq(0,0,0) + PDQ(1,0,0)),
               ASa1A2 = ARIMA(Sales_dd ~ pdq(1,0,0) + PDQ(1,0,0)), 
               ASa2A2 = ARIMA(Sales_dd ~ pdq(2,0,0) + PDQ(2,0,0)),
               search = ARIMA(Sales_dd),
               auto = ARIMA(Sales_dd, stepwise = FALSE, approx = FALSE)
               )
glance(fit_3)
# WE want model with smallest AICc .. 
report(select(fit_3, auto))
# auto = Model: ARIMA(2,0,3)(0,0,1)[12] w/ mean  ... 2 AR , 4 MA , seasonal 1MA , 12 constants
#comparing model residual vs original data set
sqrt(112149)
sd(AStsld$Sales)
#looking at the model on graph , we have a little bit of auto correlation but everything else looks normal
gg_tsresiduals(select(fit_3, auto))
#
#
#
#  Now for the tranformed Alcohol sales data:
#
fit_3l <- model(AStsld,
               auto = ARIMA(Salesl_dd, stepwise = FALSE, approx = FALSE)
               )
report(fit_3l)
gg_tsresiduals(select(fit_3l, auto))
#
#  The model is:
#    ARIMA(2,0,0)(2,0,2)[12]
#
#  While there is some autocorrelation left, it
#    is not much
#
aug_3 <- augment(select(fit_3,auto))
str(aug_3)
features(aug_3, .resid, box_pierce, lag = 24, dof = 8)
#
#  While the residual plot was showing little autocorrlation, the 
#    Portmanteau test is showing autocorrelation
#
aug_3l <- augment(fit_3l)
features(aug_3l, .resid, ljung_box, lag = 24, dof = 7)
#
#  While the residual plot was showing little autocorrlation, the 
#    Portmanteau test is showing autocorrelation here as well
#



#
#  Part 2D:  Complete automatic Arima modeling on the 
#   Alcohol Sales data and Forecasting
#
#  First model on the standard data set.  Note in this 
#    section, we now use the orginal data set and not
#    the differenced data
#
fit_4 <- model(AStsld,
               search = ARIMA(Sales),
               auto = ARIMA(Sales, stepwise = FALSE, approx = FALSE)
               )
glance(fit_4)
report(select(fit_4,auto))
gg_tsresiduals(select(fit_4, auto))
aug_4 <- augment(select(fit_4, auto))
features(aug_4, .resid, ljung_box, lag = 24, dof = 9)
# testing residual small p value rejecting the null hypothesis of noautocorrelation ; auto correlation was found

#  While the autocorrelation is still present, let's 
#    check the reduction in variance as compared to the
#    naive "mean" model
#
rep_fit4 <- glance(fit_4) 
rep_fit4$sigma2[1]/var(AStsld$Sales)
#
#
#  And now build the model with the transformed data
#
fit_4l <- model(AStsld,
               search = ARIMA(Salesl),
               auto = ARIMA(Salesl, stepwise = FALSE, approx = FALSE)
               )
glance(fit_4l)
report(select(fit_4l,search))
gg_tsresiduals(select(fit_4l, search))
aug_4l <- augment(select(fit_4l, search))
# > report(fit_3l)
# Series: Salesl_dd 

# Model: ARIMA(4,0,0)(0,1,1)[12] w/ drift 
# 4 + 1 + 1 + 1Constat = 7
features(aug_4l, .resid, ljung_box, lag = 24, dof = 7)
#
#
#  Finally produce some forecasts for the next
#    three years using the latest models
#
#  First the orginal data
#
forc_4 <- forecast(fit_4, h = 36)
forc_4_a <- filter(forc_4, .model == "auto")
autoplot(forc_4_a, AStsld)
#
#  Now the transformed data
#
forc_4l <- forecast(fit_4l, h = 36)
autoplot(forc_4l, AStsld)
#
#
#
#
#  Revisit Google data
#
str(google)
autoplot(google, Open)
sd(google$Open)
#
#  Use first 900 days to build the model
#
google900 <- filter(google, day <= 900)
google106 <- filter(google, day > 900)
#
# Fit some ARIMA models
#
fit_g1 <- model(google900, ARIMA(Open))
report(fit_g1)
fit_g2 <- model(google900, ARIMA(Open, 
                                 stepwise = FALSE, approx = FALSE)
                )
report(fit_g2)
fit_g0 <- model(google900, ARIMA(Open ~ pdq(0,1,0)))
report(fit_g0)
#
#  Build some forecasts
#
forc_g2_n <- forecast(fit_g2, new_data = google106)
#
#  Plot the data, the forecasts and compare to actuals
#
autoplot(forc_g2_n, google900, level = NULL) +
  autolayer(google106, Open, colour = "Black")
#
#  Check accuracy
#
accuracy(forc_g2_n, google106)
#
#  Which model is the best?
#
fit_gN <- model(google900, NAIVE(Open))
forc_gN <- forecast(fit_gN, new_data = google106)
accuracy(forc_gN, google106)
#
#  Forecast is not too helpful!
#  Indicates very little (none?) information present 
#    in past prices to forecast future
#
#
#
#  End of Forecasting with Time Series II Part 2 
#