#
#  Predictive Analytics (FE288): 
#  Forecasting with Time Series I:  Naive and ETS Models
#  Part 2:  ETS Forecasting
#
#  Last updated:  6/2/22 (Murphy)
#
#  This script covers:
#     A.  Simple Exponential Smoothing (SES) --> E Term ; error term
#     B.  Holt's Model (Exponential Smoothing w Trend) --> T Term ; trend term 
#     C.  Holt-Winter's Model (Holt's Model w Seasonality) --> Complete ETS Framework
#     D.  Automated ETS modeling and forecasting
#   
#
#  Use the monthly Alcohol Sales data from 2001 to 2018
#  Source:  https://fred.stlouisfed.org/graph/?id=S4248SM144NCEN,
#
#  The script also uses some other data sets
#    Hardware Sales data - weekly sales from a store
#    Watercraft Sales data - quarterly sales of personal water
#      craft (jetskis?)
#
#  Content for this script is found in Chapter 8 of H&A
#
#
#  The analysis relies on the package fpp3
#
#install.packages("fpp3")
library(fpp3)
#
#  Read in and set up the data set
# from SIMPLE to FULL .. Exponential Time Smoothing Model
AlcSal <- read.csv("alcohol_sales.csv")
AlcSal$Date <- as.Date(AlcSal$Date, format = "%m/%d/%Y")
AlcSal$Month <- as.factor(AlcSal$Month)
AlcSalts <- ts(AlcSal[,4], start = 2001, frequency = 12)
ASts <- as_tsibble(AlcSalts)
names(ASts)[2] <- "Sales"
names(ASts)
#
#  Visualize the alcohol sales data
#
autoplot(ASts, Sales)
#
#  Alcohol sales shows trend and monthly seaonality
#




#
#  Part 2A:  Simple Exponential Smoothing Models
#
#
#   Read in the Hardware Sales data set to begin fitting
#     Simple Exponential Smoothing (SES) Models
#
list.files(path=".")
HWsale <- read.csv("hardware_sales.csv")
#
#  Plot the data
#
str(HWsale)
plot(HWsale$Week, HWsale$Sales)
lines(HWsale$Week, HWsale$Sales)
#
# no seasonality or trend .. up and down ==> idiosyncratic 
#  Set up the data as a tsibble
#
HWsalts <- as_tsibble(HWsale, index = Week)
str(HWsalts)
autoplot(HWsalts, Sales)
#
#
#  Fit a simple exponential smoothing (SES) model
# model() : ETS() , Sales , error()+trend()+season() .. 
# A N N : additive error term , no trend term , no seasonality
fit_HWsalts_SES <- model(HWsalts, 
                     ETS(Sales ~ error("A") + trend("N") + season("N"))
                     )
accuracy(fit_HWsalts_SES)
sd(HWsalts$Sales)
#
#
#  Take a look at the estimates from the SES model
#
#  Where do you start? what is the smoothing constant? 
# best alpha to reducing our error
report(fit_HWsalts_SES)
#
#
#  Plot the model components
#
aug_SES <- augment(fit_HWsalts_SES)
autoplot(aug_SES, Sales) +
  autolayer(aug_SES,.fitted, colour = "Red") + #exponential smoothing forecast
  autolayer(aug_SES,.resid, colour = "Blue") #errors , how far off red is from black
#
#
#  Use SES to forecast h = 8 periods into the future
#
forc_HWsalts_SES <- forecast(fit_HWsalts_SES, h = 8)
forc_HWsalts_SES
str(forc_HWsalts_SES)
autoplot(forc_HWsalts_SES, HWsalts) 
#turning off confidence band
autoplot(forc_HWsalts_SES, HWsalts, level=NULL) 
#
#
#


#
#  Part 2B:  Exponential Smoothing with Trend Modeling
#     (Holt's Model)
#
#
#  Read in the Water Craft data set
#
WCsale <- read.csv("water_craft.csv")
#
#  Plot it to see
#
str(WCsale)
plot(WCsale$Time, WCsale$Sales)
lines(WCsale$Time, WCsale$Sales)



#
#  Convert the Water Craft data to a tsibble
#
WCsaltser <- ts(WCsale[,4], start = c(2017, 1), frequency = 4)
WCsalts <- as_tsibble(WCsaltser)
names(WCsalts)[2] <- "Sales"
str(WCsalts)
autoplot(WCsalts, Sales)
#
#
#  Fit Holt's model to the Water Craft data
#    Why Holts model?
# A A N : additive error , additive trend , no seasonality
fit_WCsalts_Holt <- model(WCsalts, 
                          ETS(Sales ~ error("A") 
                              + trend("A") 
                              + season("N"))
                          )
#
#
#  Check model accuracy
#
accuracy(fit_WCsalts_Holt)
sd(WCsalts$Sales)
#
#  Examine the model itself
#
report(fit_WCsalts_Holt)
#smoothing constant : error = alpha , trend = beta
#initial states = initial values 
aug_Holt <- augment(fit_WCsalts_Holt)
#shows adjusted red line , something linear regression cannot do
#errors show seasonality in blue which will need to be addressed
autoplot(aug_Holt, Sales) +
  autolayer(aug_Holt,.fitted, colour = "Red") +
  autolayer(aug_Holt,.resid, colour = "Blue")


#
#  Create h = 8 periods of forecasts
#
forc_WCsalts_Holt <- forecast(fit_WCsalts_Holt, h = 8)
forc_WCsalts_Holt
autoplot(forc_WCsalts_Holt, WCsalts) 
#
#
#  "Damped" Trend Exponential Smoothing Models
#     An alternative version of Holt's
#
# A Ad N : Ad = damped trend , flating trend
fit_WCsalts_Holtd <- model(WCsalts, 
                          ETS(Sales ~ error("A") 
                              + trend("Ad") 
                              + season("N"))
                          )
#
#  Check accuracy and compare to the first Holt's model
#
accuracy(fit_WCsalts_Holtd)
accuracy(fit_WCsalts_Holt)
sd(WCsalts$Sales)
#
#
#  Let's take a look the Dampened trend model 
#
# dampening parameter = phi
report(fit_WCsalts_Holtd)
aug_Holtd <- augment(fit_WCsalts_Holtd)
#
#
#  Let's graph the model components
#
autoplot(aug_Holtd, Sales) +
  autolayer(aug_Holtd,.fitted, colour = "Red") +
  autolayer(aug_Holtd,.resid, colour = "Blue") +
  labs(y = "USD ($1000s)", title = "Water Craft Sales",
       x = "Year/Quarter") 
#
#
#  Now make forecasts, h = 8, periods into the future
#
forc_WCsalts_Holtd <- forecast(fit_WCsalts_Holtd, h = 8)
forc_WCsalts_Holtd
str(forc_WCsalts_Holtd)
#
#  Plot this : damped trend and additive trend
#
autoplot(forc_WCsalts_Holtd, WCsalts, 
         level = NULL, colour = "Blue") +
  autolayer(forc_WCsalts_Holt, WCsalts, 
            level = NULL, colour = "Red") +
  labs(y = "USD ($1000s)", title = "Water Craft Sales",
       x = "Year/Quarter")
#
#


#
#  Part 2C:  Exponential Smoothing models with Trend
#     and Seasonality (Holt-Winter's Model)
#     These are known as ETS models
#   
#  Continue to use the Water Craft data, after all
#    it is exhibiting seasonality
#
#  We begin with the Additive Seasonality (HWa) for the
#    Holt-Winter's model
#
# A A A : error , trend , seasonality
fit_WCsalts_HWa <- model(WCsalts, 
                           ETS(Sales ~ error("A") 
                               + trend("A") 
                               + season("A")
                        )
)
#
#  Check accuracy on the training data
#
accuracy(fit_WCsalts_HWa)
accuracy(fit_WCsalts_Holt)
sd(WCsalts$Sales)
#
#  Surprised it isn't better than Holt's by more...
#
#  Let's take a look at the model itself....
#
# alpha , beta , gamma
# initiall state for error , trend , (4) seasons
report(fit_WCsalts_HWa)
aug_HWa <- augment(fit_WCsalts_HWa)
aug_HWa
#
#  Plot the model components
#
autoplot(aug_HWa, Sales) +
  autolayer(aug_HWa,.fitted, colour = "Red") +
  autolayer(aug_HWa,.resid, colour = "Blue") +
  labs(y = "USD ($1000s)", title = "Water Craft Sales",
       x = "Year/Quarter") 
# erors line in blue is closer to 0 which is good
#
#  Again, let's do 2-year's worth of forecasts (h = 8)
#
forc_WCsalts_HWa <- forecast(fit_WCsalts_HWa, h = 8)
forc_WCsalts_HWa
str(forc_WCsalts_HWa)
#
#  Plot these
#
autoplot(forc_WCsalts_HWa, WCsalts, 
         level = NULL, colour = "Blue") +
  autolayer(forc_WCsalts_Holt, WCsalts, 
            level = NULL, colour = "Red") +
  labs(y = "USD ($1000s)", title = "Water Craft Sales",
       x = "Year/Quarter")
#
#
#  Time for the multiplicative version of the 
#    Holt-Winter's model (HWm)
#
#  Fit the model first
# A A M : multiplicative seasonality
fit_WCsalts_HWm <- model(WCsalts, 
                         ETS(Sales ~ error("A") 
                             + trend("A") 
                             + season("M"))
                         )
#
#  Check accuracy
#
accuracy(fit_WCsalts_HWm)
accuracy(fit_WCsalts_HWa)
accuracy(fit_WCsalts_Holt)
sd(WCsalts$Sales)
#
#  Look at the model
# comparing seasonality options : additive vs multiplicative 
# multiplicative : AIC AICc BIC lower , errors = sigma^2 is lower
report(fit_WCsalts_HWm)
report(fit_WCsalts_HWa)
aug_HWm <- augment(fit_WCsalts_HWm)
autoplot(aug_HWm, Sales) +
  autolayer(aug_HWm,.fitted, colour = "Red") +
  autolayer(aug_HWm,.resid, colour = "Green") +
  labs(y = "USD ($1000s)", title = "Water Craft Sales",
       x = "Year/Quarter") 
#fit is much tighter .. growing amplitude mades multiplicative option better
#
#
#  Make forecasts
# issues running
forc_WCsalts_HWm <- forecast(fit_WCsalts_HWm, h = 8)
forc_WCsalts_HWm
#
#  Plot both Holt-Winters' models together
#
autoplot(forc_WCsalts_HWm, WCsalts, 
         level = NULL, colour = "Blue") +
  autolayer(forc_WCsalts_HWa, WCsalts, 
            level = NULL, colour = "Red") +
  labs(y = "USD ($1000s)", title = "Water Craft Sales",
       x = "Year/Quarter")
#
#  H-W model does an excellent job at forecasting the 
#    water craft data
#


#
#
#
#  Part 2D:  General ETS Modeling and Forecasting
#    using FPP3
#
?ETS
#
#
#
#
#  Automated Fiting of the ETS models 
#    
#
#  Back to the Alcohol Sales data
#
#  Visualize the alcohol sales data
#
autoplot(ASts, Sales)
#
#  Alcohol sales shows trend and monthly seaonality
#
#
#  Split data into training and test sets
#
ASts_tr <- filter(ASts, year(index) <= 2016)
ASts_tst <- filter(ASts, year(index) > 2016)
#
#  Fit the model first
#
fit_ASts_tr <- model(ASts_tr, ETS(Sales))
#
#
#  Check model accuracy
#
accuracy(fit_ASts_tr)
sd(ASts$Sales)
#
#
#  Look at the model that was fit
#
report(fit_ASts_tr)
#
#
#  Note it was an ETS 
#             M = multiplicative error
#             A = additive trend
#             M = multiplicative seasonality
#   model!
#
aug_ASts_tr <- augment(fit_ASts_tr)
autoplot(aug_ASts_tr, Sales) +
  autolayer(aug_ASts_tr,.fitted, colour = "Red") +
  autolayer(aug_ASts_tr,.resid, colour = "Green") +
  labs(y = "USD ($1000s)", title = "Alcohol Sales",
       x = "Year/Month") 
#
#
#  Make forecasts on the test data
#
forc_ASts_tst <- forecast(fit_ASts_tr, new_data = ASts_tst)
forc_ASts_tst
#
#
#  Plot the series and the forecasts
#
autoplot(forc_ASts_tst, ASts_tr, level = NULL, colour = "red") +
  autolayer(ASts_tst, Sales, colour = "black") +
  labs(y = "USD ($M)", title = "Alcohol Sales",
       x = "Year/Month")
#
#
#  Now let's compute how effective these forecasts were  
#
accuracy(forc_ASts_tst, ASts_tst)
acc_ets <- accuracy(forc_ASts_tst, ASts_tst)
acc_ets$RMSE
sd(ASts$Sales[193:216])
#
#
#  Compare what we've done with ETS models to 
#    linear regression
#
AlcSaltr <- AlcSal[1:192,]
AlcSaltst <- AlcSal[193:216,]
reg1 <- lm(Sales ~ Index + Month, data = AlcSaltr)
summary(reg1)
pred1 <- predict(reg1, AlcSaltst)
MSE <- mean((AlcSaltst$Sales - pred1)^2)
MSE^0.5
acc_ets$RMSE
sd(ASts$Sales[193:216])
#
#
#  Looks like ETS model is much better at out of 
#    sample forecasting than time series regression
#    was
#
#
#
#
#  End of Forecasting with Time Series I Part 2
#
#

