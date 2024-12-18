#import & examine data
dat = read.csv("sales_fe288f.csv")
str(dat)
summary(dat)

#cleaning data types for analysis
dat$Year = as.factor(dat$Year)
dat$Quarter = as.factor(dat$Quarter)
dat$Month  = as.factor(dat$Month)

library(fpp3)
#converting frame to monthly tsibble
sales.ts <- ts(dat[,5], frequency = 12, start = c(2007, 1)) 
sales.ts <- as_tsibble(sales.ts)
names(sales.ts) <- c("Month", "Sales")
str(sales.ts)
#descriptive stats : deciding on season cycle
# Quarter or Monthly could be used as seasonal variable indicators based on how we define our per season cycle

#building test and training set
use.tr = filter_index(sales.ts, "2007 01"~"2009 12")
use.tst = filter_index(sales.ts, "2010 01"~.)

#eval time series signals
autoplot(sales.ts)
#quick lm for plotting visual purposes
reg0 = lm(Sales~Month,data=sales.ts)
reg0$coefficients #monthly increasing slope = 0.034
trendhat = predict(reg0,sales.ts)
plot(sales.ts$Month, sales.ts$Sales);lines(sales.ts$Month,trendhat,col="red")
#seasonal plots
gg_season(sales.ts, Sales) + ggtitle("seasonal Sales of dat")+ylab("Sales")+xlab("Month")
gg_season(sales.ts, Sales, polar=TRUE) + ggtitle("seasonal Sales of dat")+ylab("Sales")+xlab("Month")
#quick autoplot of train and test
autoplot(use.tr)
autoplot(use.tst)
#descriptive stats : evaluating trend cycle , seasonality , random effects
# Sales increases with Month variable overtime, also there seems to be a seasonal trend in the 4th quarter of each year, no random effect observed.

#autocorrelation
# e. plot the autocorrelation function for the time series. is there significant autocorrelation? what does this mean? 
# our autoplot(ACF()) show plot points above the blue dotted lines which means that our seasonality lag parameter and variables are significant to estimating Sales .. 1-12M seems to have significance under PACF and ACF ; similar results between lag = 12-20 periods. 
salesACF = ACF(sales.ts, lag_max=20)
autoplot(salesACF)
salesPACF = PACF(sales.ts, lag_max=20)
autoplot(salesPACF)
#futher testing
features(sales.ts, Sales, box_pierce, lag = 20, dof = 0)
features(sales.ts, Sales, ljung_box, lag = 20, dof = 0)
#   Ho (null) : data are uncorrelation , Ha (alternate) : data are correlated .. our pvalue is low = 0 < 0.01 .. we would reject the hypothesis that our data is uncorrelated .. we have autocorrelation in our data with significant values

#naive
SN.mod = model(sales.ts, SNAIVE(Sales))
RW.mod = model(sales.ts, RW(Sales~drift()))
SN.forc = forecast(SN.mod, h=72)
RW.forc = forecast(RW.mod, h=72)
autoplot(SN.forc, sales.ts, level=NULL, colour='Black')+autolayer(RW.forc, sales.ts, level = NULL, colour='Red')
#comparing RMSE of lm to naiveSN to naiveRW
reg0hat = predict(reg0,sales.ts)
reg0RSS = sum((sales.ts$Sales - reg0hat)^2)
reg0RSE = (reg0RSS/length(sales.ts$Sales))
reg0RMSE = sqrt(reg0RSE)
#decomposition
mod.add = model(sales.ts, classical_decomposition(Sales, type='additive'))
comp.add = components(mod.add)
comp.add
autoplot(comp.add)
comp.add.mse=mean(comp.add$random^2, na.rm=TRUE)
comp.add.rmse = comp.add.mse^0.5
mod.mul = model(sales.ts, classical_decomposition(Sales, type='multiplicative'))
comp.mul = components(mod.mul)
comp.mul
autoplot(comp.mul)
comp.mul.mse=mean(comp.mul$random^2, na.rm=TRUE)
comp.mul.rmse = comp.mul.mse^0.5; 
#evaluating RSME
reg0RMSE
accuracy(SN.mod)$RMSE
accuracy(RW.mod)$RMSE
comp.mul.rmse
comp.add.rmse

#exponential smoothing ETS models & forecasts
ETS.mod = model(sales.ts,ETS(Sales))
report(ETS.mod)
accuracy(ETS.mod)
#Chosen Model : ETS(M,A,M) , Estimated Coefficients : alpha = 0.1953 , beta = 0.0044 , gamma = 0.5961 , associated model error =  sigma^2 = 0.0000 , 
# RMSE = 6.97 , acf = 0.236
ETS.mod0 = model(sales.ts,MEAN(Sales))
report(ETS.mod0)
accuracy(ETS.mod0)
# benchmark RMSE = 57.3, ACF = 0.897
#graphing components
comp.ETS = components(ETS.mod)
autoplot(comp.ETS)
# d. using the model from part b, forecast the next year for monthly earnings. how confident would you be with respect to the forecasts? 
ETS.forc = forecast(ETS.mod, h=12)
autoplot(ETS.forc,sales.ts)
#in comparison to our mean model , the ets function performs much better with lower RMSE and seasonality factor being evaluated in our prediction

#arima modeling
#checking differencing
features(sales.ts, Sales, unitroot_kpss)
features(sales.ts, Sales, unitroot_ndiffs) # I(d) = 1 = need differencing once
#differencing
sales.ts = mutate(sales.ts,Sales.1=difference(Sales))
features(sales.ts, Sales.1, unitroot_ndiffs)#checking again I(d) = 0
#arima build
arima.mod = model(sales.ts, ARIMA(Sales.1))
report(arima.mod) #Chosen model : ARIMA(4,0,0),(1,1,0)[12] with drift 
accuracy(arima.mod)#RMSE = 6.70 , ACF = -0.0231 , compare sigma to sd(Sales)
aug.fit = augment(arima.mod)
autoplot(sales.ts, Sales)+autolayer(aug.fit, .fitted, colour="red")
features(aug.fit, .resid, ljung_box, lag = 20, dof=0)
