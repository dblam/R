# FE288 - Business Analytics 
# Duy Bobby Lam | dblam@uci.edu
# Professor Ken Murphy
# Nov. 7, 2024
# HW 5 - Exponential Smoothing Models

library(fpp3)

#1 Create a time series object from the bike sharing data with frequency 7 (daily) & create a simple exponential smoothing model

# a. read in the data , & create the tsibble object for bike count and plot the series. What do you see in time? 2 variable columns references : index=Date & count=number, the data looks clustered with no obvious seasonality or trend 'idiosyncratic'
dat = read.csv("hw5_288_bike_share_day.csv")
dat$dteday = as.Date(dat$dteday, format="%m/%d/%Y")
cnts = ts(dat[,14], frequency=7)
cntts=as_tsibble(cnts)
names(cntts)[2]="count"
str(cntts); autoplot(cntts, count)
# b. fit the simple exponential smoothing (SES) model with error to the bike sharing data a with a 0.75.
fit_cntts_SES075 = model(cntts, ETS(count~error("A")+trend("N", alpha=0.75)+season("N")))
aSES075 = accuracy(fit_cntts_SES075);aSES075
sdcntts = sd(cntts$count);sdcntts
rSES075 = report(fit_cntts_SES075); rSES075
augSES075 = augment(fit_cntts_SES075); str(augSES075)
# c. fit the mean & naive models .. then compare these results to the simple exponential smoothing model to part b.. which is best so far?
# the SES model seem to have the smallest RMSE=1006 so the exponential smoothing seems to be doing a better job fitting our data and the other
fit_cntts_MNNd=model(cntts, Mean=MEAN(count),Naive=NAIVE(count),Drift=NAIVE(count~drift()))
aMNNd = accuracy(fit_cntts_MNNd);aMNNd
augMNNd=augment(fit_cntts_MNNd);str(augMNNd)
rMNNd = report(fit_cntts_MNNd);rMNNd
# comparing models accuracy & report
aSES075;aMNNd
rSES075;rMNNd
# plotting the models SES & MEANxNAIVExDRIFT
str(augSES075);str(augMNNd)
autoplot(augSES075,count)+autolayer(augSES075,.fitted,colour="red")+autolayer(augSES075,.resid,colour="blue")
autoplot(augMNNd,count)+autolayer(augMNNd,.fitted)+autolayer(augMNNd,.resid)

#2 fit the optimized SES model & Holt's Exponentially Smoothing model that incorporates trend. Evaluate these forecasting methods.. 
# a. refit the simple exponential SES model to the bike rental counts without specifying the smoothing alpha. How do the results differ from Q1?.. our RMSE went down further to 965 with auto-optimize alpha from 0.75 to 0.2834
fit_cntts_SES = model(cntts, ETS(count~error("A")+trend("N")+season("N")))
aSES = accuracy(fit_cntts_SES);aSES
rSES = report(fit_cntts_SES); rSES
augSES = augment(fit_cntts_SES); str(augSES)
aSES075;aMNNd;aSES
report(rSES);report(rSES075);rMNNd
# b. fit holt's model with addtitive trend to the bike sharing data
fit_cntts_SESa = model(cntts, ETS(count~error("A")+trend("A")+season("N")))
aSESa = accuracy(fit_cntts_SESa);aSESa
rSESa = report(fit_cntts_SESa); rSESa
augSESa = augment(fit_cntts_SESa); str(augSESa)
# c. compare the accuracy of these models with the best model from Q1.. which is a bettter fit? why? seems like adding addaptive trend did not help, our fitted model slightly worst. the most effecttivve model seems to be aSES from 2a (without additive trend) from a RMSE and sigma^2 prespective
aSES075;aSES;aSESa
aSES075$RMSE;aSES$RMSE;aSESa$RMSE
report(rSES075)#sigma^2:1014459
report(rSES)#sigma^2:932845.1
report(rSESa)#sigma^2:935936.9

#3 fit holt-winters' seasonal smoothing models to the bike count data using combinations of the additive & multiplicative seasonality, & the additive & damped additive trend methods, but stick only with additive errors
# a. fit the holt-winters model with error, trend, and seasonality arguments set to "A". what are the parameters of this model? parameters = alpha=0.2889 , beta=0.0001 , gamma=0.0001 
fit_cntts_AAA = model(cntts, ETS(count~error("A")+trend("A")+season("A")))
aAAA = accuracy(fit_cntts_AAA)
rAAA = report(fit_cntts_AAA)
augAAA = augment(fit_cntts_AAA);str(augAAA)
report(rAAA)
# b. run the other 3 Holt-Winters models as described above. which of the (4) models is the preferred one? Note: to obtain the 4 models, use both "A" and "Ad" in trend & both "A" & "M" in the season argument but leave the error argument set to A
# "A" "Ad" "A"
fit_cntts_AAdA = model(cntts, ETS(count~error("A")+trend("Ad")+season("A")))
aAAdA = accuracy(fit_cntts_AAdA)
rAAdA = report(fit_cntts_AAdA)
augAAdA = augment(fit_cntts_AAdA);str(augAAdA)
# "A" "A" "M"
fit_cntts_AAM = model(cntts, ETS(count~error("A")+trend("A")+season("M")))
aAAM = accuracy(fit_cntts_AAM)
rAAM = report(fit_cntts_AAM)
augAAM = augment(fit_cntts_AAM);str(augAAM)
# "A" "Ad" "M"
fit_cntts_AAdM = model(cntts, ETS(count~error("A")+trend("Ad")+season("M")))
aAAdM = accuracy(fit_cntts_AAdM)
rAAdM = report(fit_cntts_AAA)
augAAdM = augment(fit_cntts_AAdM);str(augAAdM)
# c. how do these models compare to the models from Q1 & Q2? are the results surprising? explain. not surprising that they are in very similar ranges for estimated parameters, sigma^2, AICs, and RMSE .. not really surprising since our data periods are the same
report(rAAA);report(rAAdA);report(rAAdM);report(rAAM)
#COMPARING ETS using .. sigma^2: AAA=922322.5 , AAdA=923094.1 , AAdM=922322.5 , AAM=930403.6
aAAA;aAAdA;aAAdM;aAAM
#accuracy$RMSE: AAA=953, AAdA=953, AAdM=945, AAM=957
# d. between all the models .. which would be the preferred? how did you select the best answer? ... I would choose the ETS(AAdA) because it would include drift addaptive trend and addaptive seasonality .. with 2nd to best RMSE .. the reason I did not select multiplicative is it may be of issue in our forecast going off path exponentially from our observations
report(rSES);report(rAAA);report(rAAdA);report(rAAdM);report(rAAM)
aSES;aAAA;aAAdA;aAAdM;aAAM

#4 create forecasts for the preferred model from Q1-3, for the next 4 weeks .. display the values of the forecasts to the console & plot the forecasts on a time oriented graph
forc_mod = forecast(rAAdA, h=28)
autoplot(forc_mod, cntts)
forc_mod

#5 built into r package fpp3 are many data sets. Load the quarterly earnings at Johnson & Johnson data set "JohnsonJohnson" 
datJJ = JohnsonJohnson
str(datJJ)
# a. desribe the data. what is the data range? how many observations? what is the periodicity of this data set? plot the data & describe in a sentence what is observed.
# datJJ is a timeseries data that range from 0.440 to 16.200. There are 84 observations from 1961 to 1981 which makes this a seasonal time series set. 
JJts=as_tsibble(datJJ, index=yearquarter());names(JJts)=c("Quarter","QE");autoplot(JJts)
# b. run the best ETS model  on the earnings for JJ. report the chosen model, the estimated coefficients, and associated model error... Chosen Model : ETS(M,A,A) , Estimated Coefficients : alpha = 0.2776 , beta = 0.0636 , gamma = 0.5867 , associated model error =  sigma^2 = 0.0085 , RMSE = 0.471
JJets5 = model(JJts,ETS(QE))
report(JJets5)
accuracy(JJets5)
# benchmark comparison
JJmean5 = model(JJts, MEAN(QE))
accuracy(JJmean5)
# c. plot the model components. what do these 5 plots tell us? there is a seasonality the in our data as the season graph show the variation between 0 , our QE trends upwards overtime
comp_JJets5 = components(JJets5)
autoplot(comp_JJets5, QE)
# d. using the model from part b, forecast the next four years for quarterly earnings. how confident would you be with respect to the forecasts? 
forc_JJets5 = forecast(JJets5, h=12)
autoplot(forc_JJets5,JJts)
#in comparison to our mean model , the ets function performs much better with lower RMSE and seasonality factor being evaluated in our prediction .. i am confident that this model would be good to make predictions 

#6 on the ETS function & choosing hte best model ..
# a. how is the best model selected by the ETS function in question 6? that is what is the criteria function use to select the best model? ... AIC the function tends to minimizes AIC = a function of likelihood plus a penalty term .. the correct AIC , and Bayesian Information Criteria (BIC) 
#** Minimizing the AIC assuming Gaussian residuals is asymptotically equivalent to minizming one-step time series cross validation MSE
# b. is the criteria function minimizing the sum of squares or something else? .. the criteria function minimizes the entire AIC equation rather than partally minimizing the sum of squares
# c. doe the criteria function account for the number of parameters in the function? explain .. on the surface, no. but the general function takes recognition of the 'recursion' effect across equations of differnt parameters

#7 On validating ETS models .. 
# a. does it make sense to perform the validation set process applied to HW2 and HW3 fitting in the context of time serires model fitting ? why or why not? YES it would make sense to apply ETS modeling into HW 2 and 3 .. where we were considering salary expectations and bike rentals.. There may be good experience and seasonality trend of significance that translate to higher salary and rentals during peak seasons; respectively
# b. how would this approach be applied in this context? what does time series corss validation mean? explain.. we would create a tsibbble data object for each of the data set and set the index to be either quarterly or monthly.. our predicited Y is unchanged but the predictor variables can be better utilized in their categorical nature .. cross validation between random observations and actual test predictions may provide higher accuracy in predictions

#8 compute the best ETS model on the US employment data for total private organization employment .. similar to JJ data, fpp3 provides us_employment data set for us
# a. set up the data set & plot it .. describe in a sentence what is observed .. increasing trend of montly employment count form the time series from 1940 to 2020; 
usemp8 = us_employment
usemp = filter(usemp8,Title=="Total Private")
usemp = usemp[, c(1,4)]
autoplot(usemp,Employed)
str(usemp)
# b. split the data into training and test set
use.tr = filter_index(usemp, "1990 01"~"2015 12")
use.tst = filter_index(usemp, "2016 01"~.)
# c. fit the best ETS model using hte automated routine, what is the model chosen? ETS(A,Ad,A) compare the RMSE of the fit with the standard deviation of employment data overall .. recall , RMSE=(distance between predicted & actual) and SD=(spread of data around mean) .. RMSE of ETS = 169 and our SD of training data set = 8849.105 .. this is a good indicator that our model is going a very good job since it is not much far away than actual values as compared to the larger number of actuals from the mean .. our data seems to have a trend and seasonality theme that was accounted for in estimating the model
use8 = model(use.tr, ETS(Employed))
report(use8)
accuracy(use8)
sd(use.tr$Employed)
# d. make forecasts for the next 45 months using the chosen model. plot the data and the forecasts
forc_use8 = forecast(use8, h=45)
autoplot(forc_use8, use.tr, level=NULL)+autolayer(filter_index(use.tst,"2016 01"~.),color="red")
# e. finally, compare the accuracy of the model on the test set (RMSE) to the accuracy on the out of sample data (test set) our training set performed better in nature having been evaluated in our model estimation .. test set however was worst and higher than sd 
accuracy(use8) #RMSE = 169
accuracy(forc_use8, use.tst) #RMSE = 1179

#9 compute the best ETS model on the oogle closing ("Close") price data form 2015 only
# a. what ETS model is best for predicting Google closing prices? what is the associated RMSE? ETS(M,N,N) RMSE=11.2
dat_goog15 = filter(gafa_stock, Symbol=="GOOG", year(Date)==2015)
dat_goog15 = mutate(dat_goog15, day=row_number())
dat_goog15 = update_tsibble(dat_goog15, index=day, regular=TRUE)
str(dat_goog15)
goog9 = model(dat_goog15, ETS(Close))
report(goog9)
accuracy(goog9)
# b. how does the RMSE compare to the RMSE of the NAIVE model? they are about the same
goog9n = model(dat_goog15, NAIVE(Close))
accuracy(goog9n)
# c. what do the results of parts a & b tell you about this modeling context (forecasting asset prices)? forecasting asset prices can be hard because future data does not have to rely on historic prices .. there is almost no significant in increasing or decreasing trend so smoothing may be the best approach
#d. create the components of this ETS model for forecasting Google closing prices "Close" & plot them .. comment on what is observed. based on 'multiplicity' our increasing trend seem to exponentialy growing with closing price in the prection
goog9.comp = components(goog9)
autoplot(goog9.comp)

#10 preview of HW6 , autoregressive, integrated, moving average (ARIMA) models.. compare the results for predicting Google stock price in 2015 found in Q9 with a 'best' ARIMA model
# a. fit the best ARIMA model . what is the chosen model? ARIMA(0,1,1)
#install.packages("urca")
library(urca)
goog10=model(dat_goog15, ARIMA(Close))
report(goog10)
accuracy(goog10)
# b. how does the RMSE compare to the RMSE of the NAIVE model?
# ARIMA RMSE = 11.1 , EST & NAIVE RMSE 11.2 ... arima performed better with lower RMSE
# c. how confident would you be with respect to forecasting stock prices in general.. not very confident but theoretically it should be better interms of minimizing RMSE. forecasting asset prices can be difficult due to the related variables and moving prameters that might be of significant .. the model becomes complicated very quickly while historical data trends only tell partial of the story
