# FE288 - Business Analytics 
# Duy Bobby Lam | dblam@uci.edu
# Professor Ken Murphy
# Nov. 7, 2024
# HW 6 ARIMA Models

library(fpp3)

#1 run script to create 200 obs. of a series, stored as a tsibble, called 'wnts'
set.seed(234567)
wn = ts(data.frame(rnorm(200)))
wnts = as_tsibble(wn)
names(wnts)[2] = "WN"
str(wnts)
# a. describe the script .. 
#   sets a seed for 'recallable' random start point.. then, generate time series data frame of 200 normally distributed observations called 'wn'.. convert wn time series data frame object into a tsibble table while renaming the index reference-able column elements into 'WN'
# b. plot the data & describe what is observed
autoplot(wnts)
#   seems like our data is randomized with no observed trend or seasonality , we can make an inference that the set is stationary
# c. plot the autocorrelation & partial auto correlation values (20lags?) & describe what is observed
autoplot(wnts, WN)
wntsacf = ACF(wnts, lag_max=20)
autoplot(wntsacf)
wntspacf = PACF(wnts, lag_max=20)
autoplot(wntspacf)
#   we notice that the black bars are within bluelines for both ACF and PACF at 20lags .. showing no significance in autocorrelation in the data set 'wnts'
# d. run the portmanteau test (either box-pierce or box-ljung) to test for the presence of autocorrelation
features(wnts, WN, box_pierce, lag = 20, dof = 0)
features(wnts, WN, ljung_box, lag = 20, dof = 0)
#   Ho (null) : data are uncorrelation , Ha (alternate) : data are correlated .. our pvalue is high 0.9 > 0.01 .. we fail to reject the hypothesis that our data is uncorrelated 
# e. summarize your findings .. is there autocorrelation? should there be? explain ..  just as we expected form using the rnorm() distribution of random seed generated data set .. our wnts tsibble data set show that we have no autocorrelation. additionally, there should not be since this data is randomized

#2. run script to create 200 obs. of 'synthetic' time series which is stored as a tsibble
y2 = wn
for (i in 2:200){
  y2[i] = 0.6*y2[i-1] + wn[i] + 0.3*wn[i-1]
}
y2ts = as_tsibble(y2)
names(y2ts)[2] = "Y2"
str(y2ts)
# a. describe the series in terms of ARIMA model classification .. i.e. this model is ARIMA(?,?,?) ..
#  ARIMA(2,0,0) -> p=1,q=0,d=0 .. we are looking at (2) Phi variables phi1=0.6 & phi2=0.3 where the sequence start with 'in in 2:200' loop .. no errors or constant is being introduced
# b. plot the data & describe what is observed
autoplot(y2ts, Y2)
#  we are observing a light stationary data set where the data looks randomized with no trend , seasonality or correlation
# c. plot the autocorrelation & partial auto correlation values & describe what is observed
y2tsacf = ACF(y2ts, Y2, lag_max=20)
autoplot(y2tsacf)
y2tspacf = PACF(y2ts, Y2, lag_max=20)
autoplot(y2tspacf)
#  ACF and PACFF does show partial correlation and especially in the first observation strong one bar, we can look more into the first couple strong bars from ACF & PACF to get more details on a potentially effective ARIMA model 
# d. runteh portmanteau test (box-peirce or box-ljung) to test for presence of autocorrelation
features(y2ts, Y2, box_pierce, lag = 20, dof = 0)
features(y2ts, Y2, ljung_box, lag = 20, dof = 0)
# since pvalue is low = 0 , we reject the null hypothesis that our data is uncorrelated
# e. we have found that there is correlation in our y2ts data set via box-pierce and box-ljung tests .. from this set we can further develop a working model to could potentially predict Y2 more accurately

#3. Fit ARIMA models to the data series created in Q1 & Q2 .. call these arima1 & arima2 .. for each:
arima1=model(wnts,ARIMA(WN))
arima2=model(y2ts,ARIMA(Y2))
# a. describe what the model found when fitting using ARIMA
report(arima1) # ARIMA(0,0,0) constant=-0.1095 , se=0.0665 , sigma^2=0.8898 , AICc = 547.28
report(arima2) # ARIMA(1,1,2) , [constant,s.e.] : ar1=[0.5316, 0.0834], ma1=[-0.5933, 0.0906], ma2=[-0.3806, 0.0855] , sigma^2=0.9039, AICc=551.85
#  arima1 model found ARIMA(0,0,0) meaning the errors are uncorellated across time containing only white noise which was concluded when we tested for correlation via ACF and PACF ... 
#  arima2 found ARIMA(1,1,2) there is correlation in historic obs. of Y with initial spike and trailing drops in ACF & PACF .. ACF trailing off but significant PACF 1 strong bar leads to p=AR1=1 , PACF also trailing off with 2 strong bar leads to q=MA1,MA2=2, the model included difference-ing at level1 so d = 1; ARIMA(P,D,Q) = (1,1,2)
# b. compare the models sigma^2 to the standard deviation of series
#   arima1 sigma^2=0.8898 
#   arima2 sigma^2=0.9039
sd(wnts$WN)#[1] 0.9432822
sd(y2ts$Y2)#[1] 1.409413
# it looks like y2ts having significance was more effective when modeling using ARIMA models .. on the other hand , the wnts data set being non-significant did not really improve over standard deviation when modeled
# c. was there a disparity between the model found & the model that should have been determined? yes for the arima2 model, there is. .. if you figured out which model each of the above series were, you can try running these models using the information provided below
accuracy(arima1)
accuracy(arima2)
# the RMSE is 0.941 for both arima1 and arima2 which is lower thand both model's standard deviation of 0.9432822 & 1.409413 .. this means that our model was slightly beter at predicting outcomes than relying on observation mean x standard deviations

#4. Read in US monthly single family homes data from 2010 forward & conduct analysis to see if the series is stationary; that is not trending up or down 
dat4=read.csv("hw6_288_home_sales.csv")
str(dat4)
Homet = ts(dat4[,2], frequency = 12, start=c(2010,1))
Homets = as_tsibble(Homet)
names(Homets)[2] = "HSales"
str(Homets)
# a. set up data for analysis . plot the data & describe patterns observed .. there seems to be an increasing trend , our data is not stationary 
autoplot(Homets)
# b. is the data trending up or down ? or both? .. both . if so, then the day may need to be differenced before an AR & MA terms in the model can be fitted .. we call this nonstationary . calculate the # of differencing needed I(d)=1 .. difference the data appropriately & add the differenced data to the tsibble for later via 'mutate'
features(Homets, HSales, unitroot_kpss)
features(Homets, HSales, unitroot_ndiffs) # I(d) = 1
Homets = mutate(Homets,HSales_1=difference(HSales))
features(Homets, HSales_1, unitroot_ndiffs)#checking again I(d) = 0
# c. plot the differenced sales data. describe what you see. does the data appear stationary at this point? # yes .. we see random spikes with no trend .. our data seems to be stationary
autoplot(Homets, HSales_1)

#5. using the differenced Home Sales data try fitting ARIMA(1,0,0) and ARIMA(2,0,0) models .. call these by their (P,D,Q) m100 & m200
m100 = model(Homets, ARIMA(HSales_1~pdq(1,0,0)))
m200 = model(Homets, ARIMA(HSales_1~pdq(2,0,0)))
# a. which of these models fits best in terms of the sigma (or sigma squared?) ... seems like m100 fits better with lower AICc and sigma^2 .. lower sigma indicates a smaller spread of errors around the fitted line, model fits data more closely with less variance in the residuals
report(m100) #sigma^2 = 2315
report(m200) #sigma^2 = 2320
# b. which of these fits best in term so of AICc .. we want the model with lowest AICc
# m100 AICc = 1867.08
# m200 AICc = 1868.54

#6. use the original Home Sales tsibble (not differenced) & fit ARIMA model using the automated model fitting procedure. this means using the variable 'HSales_d1' in tsibble "Homets' to create the model fit6
fit6 = model(Homets, ARIMA(HSales))
report(fit6) # sigma^2 = 2065 , AICc = 1856.18
accuracy(fit6)
# a. describe the ARIMA model .. ARIMA(1,1,1)(0,0,2) with drift.. we can see that there were 5 coefficients estimated with AR=1, I(d)=1, MA=1 .. as well as sma1 & sma2 for seasonal drift effect which is the constant.. 
# b. plot the data & fitted values of the model
aug_fit6 = augment(fit6)
autoplot(Homets, HSales)+autolayer(aug_fit6, .fitted, colour="red")
# c. is the model, fit6 better fitting as compared to the model estimated in Q5?  YES, the model is more complicated by we have a lower sigma^2 and AICc
# d. how does the automated procedure settle upon the model it chose? the automated version searches locally around a start point of 'ie. differncing the data' from the basic approach and expanding parameters to minimize AICc.. however, in attempts to do so our model became more complicated and harder to interpret
accuracy(fit6)
accuracy(m100)
accuracy(m200)

#7. Analyze the residuals form the model estimated in Q6. Did the fitted model remove all of the signal from the home sales data? 
# a. plot the residuals. do the residuals appear random or are there patterns? yes except for a couple spikes towards the 2nd half.. do they appear to be normally distributed? they do appear to be normally distributed. is there significant autocorrelation? there is positive autocorrelation but not really significant
gg_tsresiduals(fit6)
# b. run a Box-Ljung test on the residuals to check for autocorrelation . what is the result & the implications 
features(aug_fit6, .resid, ljung_box, lag = 20, dof=0)
# low pvalue so we reject the null hypothesis that our data is non stationary
# c. how did the model do for explaining the “signal” in the home sales series?
# our model is normalized and stationary .. the model can be better but can be more accurate with automated ARIMA
accuracy(fit6);glance(fit6)

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

