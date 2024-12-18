
dat4 <- read.csv("hw6_288_home_sales_mac.csv")
str(dat4)
Homet <- ts(dat4[,2], frequency = 12, start = c(2010,1))
Homets <- as_tsibble(Homet)
names(Homets)[2] <- "HSales"
features(Homets, HSales, unitroot_kpss)
features(Homets, HSales, unitroot_ndiffs)
Homets <- mutate(Homets, HSales_1 = difference(HSales))
features(Homets, HSales_1, unitroot_ndiffs)
fit5 <- model(Homets, 
              m100 = ARIMA(HSales_1 ~ pdq(1,0,0)),
              m200 = ARIMA(HSales_1 ~ pdq(2,0,0)))
glance(fit5)
accuracy(fit5)
fit6 <- model(Homets, ARIMA(HSales))
report(fit6)
accuracy(fit6)
aug_fit6 <- augment(fit6)
autoplot(Homets, HSales) +
  autolayer(aug_fit6, .fitted, colour = "red")
gg_tsresiduals(fit6) 
features(aug_fit6, .resid, ljung_box, lag = 20, dof = 0)
#8. Find the best ETS model for HomeSales , call the model fit8 .. compare ETS to ARIMA 'fit6'
fit8 <- model(Homets, ETS(HSales))
aug_fit8 = augment(fit8)
report(fit8); report(fit6)
glance(fit8); glance(fit6)
accuracy(fit8); accuracy(fit6)
# a. what ETS model was chosen? .. ETS(M,N,N)
# b. create forecast for the next 3 years .. for ETS(fit8) & ARIMA(fit6)
forc6 = forecast(fit6, h=36)
forc8 = forecast(fit8, h=36)
# c. plot the actual data, fitted values, & forecast
autoplot(Homets, HSales) +
  autolayer(aug_fit6, .fitted, colour = "red") + 
  autolayer(aug_fit8, .fitted, colour = "green") + 
  autolayer(forc6, HSales, level = NULL, colour = "red") +
  autolayer(forc8, HSales, level = NULL, colour = "green")
# d. which model would you pick here? is there a numerical measure that we could use to answer the question?
#  I would choose the ETS model .. in evaluating against the data's mean and standard deviation with the varying RMSE, sigma^2, and AICc of each model that was estimed during the process.

#9. create an ARIMA model for the US GDP from 1980 til 2019
# a. load data, plot, describe the graph
dat9 <- read.csv("hw6_288_USGDP_mac.csv")
GDPt <- ts(dat9[,2], frequency = 4, start = 1947)
GDPts <- as_tsibble(GDPt)
names(GDPts)[2] <- "GDP"
GDPts <- filter(GDPts, index >= yearquarter("1980"))
autoplot(GDPts, GDP)
#   our graph is trending upwards , there seems to be a seasonal theme , and correlation with time
# b. transform data via Box-Cox procedure with Guerrero.. 
features(GDPts, GDP, features = guerrero)
lambda <- pull(features(GDPts, GDP, features = guerrero), 
               lambda_guerrero)
lambda
GDPts <- mutate(GDPts, GDPT = box_cox(GDP, lambda))
str(GDPts)
#  why does transformation make sense in this case? 
features(GDPts, GDP, unitroot_ndiffs)
# running the unitroot_ndiffs tells us that we will need to difference our data 2 times in order to get to stationary state which would avoid actual trends and seasonality in data for ARIMA modeling .. using Box-Cox and Guerrero allows us to automate the differencing process for best outcome for the purpose of data transformation
# why do we need to transform the data before ARIMA modeling ? nonstationary data enables the nature of model estimation to latch on to trends and seasonality year over year which is a downside of regression analysis .. similar to having separate training and test set, we want to account for the 'non-predictable' uncertainty element
# c. build ARIMA model on transformed data. what model was chosen, what is the RMSE? ... 
fit9 = model(GDPts, ARIMA(GDPT))
aug_fit9 = augment(fit9); glance(fit9); accuracy(fit9); report(fit9)
#2nd way to fit ARIMA with better results?
fit9A2 <- model(GDPts, ARIMA(box_cox(GDP, lambda)))
report(fit9A2)
accuracy(fit9A2); glance(fit9A2)
aug_fit9A2 <- augment(fit9A2)
aug_fit9A2
# RMSE manually calculated ; provided in accuracy(fit9A2)
rmse_fit9A2 <- (mean(aug_fit9A2$.resid^2, na.rm = TRUE))^0.5
rmse_fit9A2
# Model: ARIMA(3,0,0)(2,1,1)[4] w/ drift 
# ARIMA(GDPT) RMSE = 0.297
# d. build ETS model on transformed data. what is the model & RMSE?
fit9e = model(GDPts, ETS(GDPT))
aug_fit9e = augment(fit9e); glance(fit9e); accuracy(fit9e); report(fit9e)
# Model: ETS(A,A,A) ; Smoothing parameters: alpha = 0.8106211 , beta  = 0.3032349 , gamma = 0.1893788 
# ETS(GDPT) = 0.330
# e. pick a preferred model between ARIMA and ETS.. state why.. 
# ARIMA model provides lower AICc , RMSE , and sigma^2 .. 
# f. make forecast for the next 5 years and plot them
forc9A2 = forecast(fit9A2, h = 60)
forc9e  = forecast(fit9e, h=60) 
autoplot(GDPts, GDPT) +
  autolayer(aug_fit9A2, .fitted, colour = "red") + 
  autolayer(aug_fit9e, .fitted, colour = "green") + 
  autolayer(forc9A2, GDPT, level = NULL, colour = "red") +
  autolayer(forc9e, GDPT, level = NULL, colour = "green")

