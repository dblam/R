# Duy Bobby Lam
# FE288 - Predictive Analytics
# Prof: Ken Murphy
# Nov 2, 2024

library(fpp3)

#1 Import and Clean Data .. 
# Qualitative Variables to Factors. 
# Add time trend variable & quadratic time trend (trend squared) variable to the data set. 
# Convert dat variable from a character data type to 'date' data type
dat = read.csv("hw4_288_home_starts.csv")
head(dat);str(dat)
dat$Quarter = as.factor(dat$Quarter); dat$Date = as.Date(dat$Date, format = "%m/%d/%Y"); dat$Month = as.factor(dat$Month)
dat[789,]
dat$timetr = 1:789
dat$timetrsq = dat$timetr^2
str(dat)
names(dat)[5:6] = c("Trend", "TrendSQ")
names(dat)
# a. what factors affect, Starts, the response variable in this data set (open ended - no analysis required)? .. definition of time period could be weekly , monthly , quarterly , seasonal , etc. also we can loop from newest to oldest or oldest to newest .. 
# b. which variables are candidates for seasonal variables in this data set? explain.. Quarter or Monthly could be used as seasonal variable indicators based on how we define our per season cycle
# c. Subset the data frame dat into january 2009 and onward.. We will look at the last 189 rows (15yrs & months) for the remainder of the assingment.
str(dat)
dat0 = dat
dat = subset(dat0, Date > "2008-12-01")
str(dat)

#2 Plot the Starts data now in a data frame 'dat' in time
# Run a regression model that predict 'Starts' with the time variable 'Trend' , as the predictor variable, call this model reg2
plot(dat$Starts, dat$Trend)
reg2 = lm(Starts~Trend,data = dat);summary(reg2)
# a. what do you observe in the plot? .. Starts increases with Trend variable, also there is a increasing seasonal trend in every 3 or 4 occurences
# b. what is the estimated regression model , reg2? .. Y-dat$Starts = -243.5229 + 0.48348*(dat$Trend)
# c. how well does it fit ? Mulltiple R-Squared = 0.7733 means that 77.33% of our prediction from model is explained by variable Trend with  residual standard error of 14.36 on 187 degrees of freedom .. our prediction seems to be off by ~14 units based on observed and predicted values .. this is rather large so we could do better..
# d. Interpret the coefficient associated with the 'Trend' variable in a sentence.. for each unit increase in dat$Trend, our prediction for dat$Starts increases by 0.48348
# e. Is 'Trend' a significant variable? Our Trend variable is significant to predicting Strats with alpha < 0.001 and very low standard error of 0.01914 .. That is, is there a 'significant' trend in Strats data? YES
# f. Visualize the model on the original data plot - does the model do a good job? Yes
plot(dat$Date,dat$Starts,xlab="Month",ylab="# of Starts",main="Housing Starts by Month");lines(dat$Datedat$Starts)
# To visualize the model forecasts on the plot of the data , try:
yhat2 = predict(reg2,dat) #creates the estimates yhat2
plot(dat$Date,dat$Starts,xlab="month",ylab="# of starts") # plots the original daata in time 
points(dat$Date,yhat2,col="red") # plot points yhat2 'fitted values' to graph
lines(dat$Date,yhat2,col="red") # connect lines from points 'fitted values'

#3 Try: adding the square time trend variable dat$TrendSQ to capture more of the signal in the series
# a. Fit the regression model, reg31, that predicts dat$Starts with predictors dat$Trend & dat$TrendSQ .. write out the equation
reg31 = lm(dat$Starts~dat$Trend+dat$TrendSQ);summary(reg31)
# dat$Starts = -1287 + 3.505*dat$Trend + 0.002174*dat$TrendSQ
# b. how will the model fit? .. the model fits better than previous 'reg2' with a higher Adjusted R-Squared : 0.8085
# c. Does the model fit better than that of Q2? 
anova(reg2,reg31)
# YES , reg31 provides a higher R-Squared as well as lower sum of squared errors and F statistic value
# d. Can you interpret the dat$Trend variable like in Q2? Why or Why not? No, because our data is squared, that means we are gradually and exponentially increases our dat$Trend variable to better predict dat$Starts
# e. try adding cubic term (time trend cubed) to the model as reg 32.. These models, reg31 and reg32, are known as 'polynominal regression' models in time. Normally, we generally leave in the lower ordered terms when we run the higher ordered terms even if these are not significant, so each model just has one more variable than the one before it. Does adding 'time cubed' help? Somewhat, but not very significant .. Rquared has gotten worst while total sum of squares and F statistics decreased a bit.
dat$timetrcubed=1:189;dat$timetrcubed = dat$timetr^3; str(dat)
names(dat)[7] = 'TrendCubed';str(dat)
reg32=lm(Starts~Trend+TrendSQ+TrendCubed, data=dat); summary(reg32)
anova(reg2,reg31,reg32)
# Comment on the quality of these models .. Is there a significant trend in the data set? 
# Yes there is signifinant trend , our RSE is relatively small which points to a more accurate regression of data predicted
#Compute RMSE for the quadratic trend model. Plot the actual housing starts and the fitted values on the same graph. 
a2=anova(reg2);a31=anova(reg31);a32=anova(reg32)
#a2$'Sum Sq' = TSS = SST = sum(dat$Strats-mean(dat$Strats))^2 = measures total variability of a data set
#deviance(reg2) = sum(resid(reg2)^2) = RSS = sum(reg2$residuals^2) = the lower the value, the better a model fits a dataset
#RSE = residual standard error 'shown in summary(model)' = sqrt(deviance(reg2)/df.residual(reg2)) = sqrt(SSE/(n-1(1+k))) = the lower the value, the more closely a mobel is able to fit the data before overfitting becomes a problem
#RMSE = MSE = sqrt(mean((data$actual - data$predicted)^2)) = library(metrics);rmse(data$actual,data$predicted) = larger the number means poor regression
yhat32 = predict(reg32,dat) 
RSSyhat32 = sum((dat$Starts-yhat32)^2)
RSEyhat32 = sqrt(RSSyhat32/nrow(dat))
RSSyhat32;RSEyhat32
# SPLITTING DATA SET TO TRAIN & TEST , Not needed
# set.seed(714988);train=sample(1:189,95);str(train)
# dat.trainQ = dat[train,]; dat.testQ = dat[-train,]
# reg.trainQ = lm(Starts~Trend+TrendSQ+TrendCubed, data=dat.trainQ); summary(reg.trainQ)
# yhatQ = predict(reg.trainQ,dat.trainQ)
#Plotting
plot(dat$Date,dat$Starts,xlab="month",ylab="# of starts") # plots the original daata in time 
points(dat$Date,yhat32,col="purple") # plot points yhat2 'fitted values' to graph
lines(dat$Date,yhat32,col="purple") # connect lines from points 'fitted values'

#4 Add the seasonal variable 'Quarter' to the quadratic regression model reg31 of Q3 to predict housing starts , 'Starts' .. call this reg41 , evaluate results
reg41 = lm(Starts~Trend+TrendSQ+TrendCubed+Quarter, data=dat);summary(reg41)
# a. fit the regression model reg41 that predicts Starts with predictors Trend & Season , write the equation for the estimated model
#Equation : Y_Starts = 1.491979e+03+Trend*(-5.242496e+00)+TrendSQ*(4.684172e-03)+TrendCubed*(-2.432348e-05)+Quarter2*(1.767269e+01)+Quarter3*(1.504625e+01)+Quarter4*(3.040034e+00)
# b. how does the model fit? .. low RSE , higher R-Squared 'adjusted' with most of the variables being significant in predicting Y
# c. interpret coefficient Quarter3 in a sentence, what does this mean? .. In Quarter 3, Starts increases by 15.05 on average.
# d. does this model fit better than Q2? doe sit provide significantly better fit?
# yes it is significantly better than reg2 , our reg41 is more accurate with higher R-Squared and smaller R-Squared
reg0 = lm(Starts~1,data=dat)
anova(reg0,reg2,reg41)
#RMSE of reg41
yhat41 = predict(reg41,dat) 
RSSyhat41 = sum((dat$Starts-yhat41)^2)
RSEyhat41 = sqrt(RSSyhat41/nrow(dat))
RSSyhat41;RSEyhat41

#5 now add the other seasonal variable 'Month' to the model of the previous question to predict 'Starts' .. call this reg51
reg51 = lm(Starts~.-Date, data = dat);summary(reg51)
anova(reg0,reg2,reg41,reg51)
#RMSE of reg51
yhat51 = predict(reg51,dat) 
RSSyhat51 = sum((dat$Starts-yhat51)^2)
RSEyhat51 = sqrt(RSSyhat51/nrow(dat))
RSSyhat51;RSEyhat51
# Analysis of Variance Table
# 
# Model 1: Starts ~ 1
# Model 2: Starts ~ Trend
# Model 3: Starts ~ Trend + TrendSQ + TrendCubed + Quarter
# Model 4: Starts ~ (Date + Month + Quarter + Trend + TrendSQ + TrendCubed) - 
#   Date
# Res.Df    RSS Df Sum of Sq         F    Pr(>F)    
# 1    188 170049                                     
# 2    187  38544  1    131505 1444.2144 < 2.2e-16 ***
#   3    182  19961  5     18583   40.8166 < 2.2e-16 ***
#   4    174  15844  8      4117    5.6515 2.207e-06 ***
# a. fit the regression model reg51 that predicts Starts with Trend Quarter and Month. what happens when this model fit? can you explain why it happens?
# it looks like our F statistic increase due to over fitting of model with 11 additional variables; even though our R-Squared is increased.. also Quarter variable becomes non-significant or NA
# b. now remove the Quarter variable from reg51, call it reg52. how does this model look compared to the model look compared to the model from part a? compare the coefficient of determination in these 2 models
reg52 = lm(Starts~.-Date-Quarter, data = dat);summary(reg52)
anova(reg0,reg51,reg52)
# removing Quarter form reg51 did not make much diffence in coefficents .. our model however, has gotten simpler
# c. how well does the model52 fit? .. our R-squared is 0.8993 , which seems to be the ceiling for our model optimization , also we have been able to remove Quarter which simplified the model and reduced overfitting of variables
# d. does this model fit better than that of Q4? does it provide significantly better fit? Yes and YES

# AT THIS POINT .. we will switch to using the content from the Hyndman & Athanasopoulous Forecasting Principles & Practices textbook. we will use 'tsibble' time series tibble in place of data frame . we will continue with this through out weeks 5& 6

#6 create a time series 'tsibble' object, 'HSdat' from housing starts data frame that was used for regression above.
# a. ensure the package fpp is installed .. use install.packages('fpp3') to install the package & 'library(fpp3)' to turn it on
library(fpp3)
# b. create the tsibble 'HStarts' series
# dat is the original data frame with housing starts in it. the mutate command overwrites the 'Month' variable in 'dat' & prepares the series for conversion to a 'tsibble'.. the new object is called 'dat1' , 'as_tsibble()' command converts the data in dat1 to a tsibble data type containing 2 columns 2(month) & 4(starts).. only 'month' and 'starts' variables are needed here
str(dat)
dat1 = mutate(dat,Month=yearmonth(Date))
HSdat = as_tsibble(dat1[,c(2,4)], index = Month)
str(HSdat)
# c. plot the housing starts over the entire time frame of the data set using 'autoplot' . describe any signals in this "Starts" series ... there is a seasonal trend in our data where Starts is low in January to March and at its peak around summer time from May til August, we can see the economy impact during Covid with higher averages possibly from lower interest rates, it almost seem like our data are not correlated and filled with white noise
autoplot(HSdat,Starts)
# d. create a seasonal plot
#SEASONAL MAP
gg_season(HSdat, Starts) + ggtitle("seasonal starts of HSdat")+ylab("Starts")+xlab("Month")
#POLAR MAP
gg_season(HSdat, Starts, polar=TRUE) + ggtitle("seasonal starts of HSdat")+ylab("Starts")+xlab("Month")
# e. plot the autocorrelation fuction for the time series. is there significant autocorrelation? what does this mean? our autoplot(ACF()) show plot points above the blue dotted lines which means that our seasonality lag parameter and varables are significant to estimating Starts .. 1-3M and 10-12M seems to have higher significance
ACF(HSdat, lag_max=20)
autoplot(ACF(HSdat,lag_max = 20))

#7 Generate naive time-series forecasts. evaluate the quality of these forecasts.
# a. create a 60-period (5yr) forecast using random walk RW forecast with drift
#to compute the sesonal naive forecast .. 
mod7.SN = model(HSdat, SNAIVE(Starts))
mod7.RW = model(HSdat, RW(Starts~drift()))
# b. create a 60-period (5yr) forecast using the seasonal naive 'SNAIVE' method on the Starts series
#to create a forecast using seasonal naive for 5yr 60months in the future
forc7.SN = forecast(mod7.SN, h=60)
forc7.RW = forecast(mod7.RW, h=60)
# c. plot these forecasts on the same graph
#to plot multiple forecasts, use 'autoplot' then 'layer' the second series on top of the first via '+' and 'autolayer()'
autoplot(forc7.SN, HSdat, level=NULL, colour='Black')+autolayer(forc7.RW, HSdat, level = NULL, colour='Red')
# d. obtain the RMSE for each forecast & compare the linear regression model of Q5
#to compute the RMSE of the model
accuracy(mod7.RW);accuracy(mod7.SN)
#rmse of reg51 = 9.1558 
#rmse 0f random wal with drift = mod7.RW = 10.2
#rmse of seasonal naive forecast = mod7.SN = 10.8 
#seems like our linear regression reg51 is a little better than this model mod7.SN and mod7.RW

#8 execute the classical additive & multiplicative decompositions for the Starts series
# a. run the additive decomposition on housing starts, Starts
#to perform additive classical decomposition use:
dc8_cladd = model(HSdat, classical_decomposition(Starts, type='additive'))
#to obtain the component of the decomposition :
comp_cladd = components(dc8_cladd)
# b. plot the decomposition. what is observed regarding trend & seasonal effect? .. increasing trend and season cycle seems to be consistent accross the months plotting, our random component show a spike around June 2020
autoplot(comp_cladd)
# c. compute the RMSE from the 'random' series for both these decompositions
#to compute the error from the 'random' component of the decomposition , use:
dc8_cladd.mse=mean(comp_cladd$random^2, na.rm=TRUE)
dc8_cladd.rmse = dc8_cladd.mse^0.5; dc8_cladd.rmse #sqrt(dc8_cladd)

# && multiplicative
#9. repeat Q8 with 'multiplicative' , what is the difference between the two?
# main difference is how the components are combined to form the original time series. # additive adds the components together.. best when seasonal variation is relatively constant overtime
# multiplicative assumes components are multipled together.. best when the seasonal variation increases overtime
dc8_clmul = model(HSdat, classical_decomposition(Starts,type='multiplicative'))
comp_clmul = components(dc8_clmul)
autoplot(comp_clmul)
dc8_clmul.mse=mean(comp_clmul$random^2, na.rm=TRUE)
dc8_clmul.rmse = dc8_clmul.mse^0.5; dc8_clmul.rmse

#10 compare the RMSEs from models Q2,Q5,Q7,Q8 in our HW assignment
# a. which model had the lowest RMSE? .. Q8&9 had the lowest at dc8_clmul.rmse = 1.003
# b. in your opinion , will this best provide effective forcast in the future? ... this all depends on our data observations and nature between qualitiative , quantitaive, and if it is a time series with differing seasonal trends .. the decomposition method adds a new method that more apprppriate in modeling time series which is pivotal in many prediction models today
# c. summarize in a few sentences the signals detected in the housing starts data based on the work performed in this assignment... we have detected a seasonal trend in housing starts peaking mid year and increasing trend over time. additionally, our starts data have been constant since 2010 until recent. However, there seem to be a starts dip around covid time starting from Jan-Jun 2020.


