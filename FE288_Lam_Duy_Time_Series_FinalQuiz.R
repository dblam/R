dat = read.csv("sales_fe288final.csv")
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

#Q1
use = filter_index(sales.ts, "2011 01"~.)
#eval time series signals
autoplot(use)
#quick lm for plotting visual purposes
reg0 = lm(Sales~Month,data=use)
reg0$coefficients #monthly usereg0$coefficients #monthly increasing slope = 0.034
trendhat = predict(reg0,use)
plot(use$Month, use$Sales);lines(use$Month,trendhat,col="red")
#seasonal plots
gg_season(use, Sales) + ggtitle("seasonal Sales of dat")+ylab("Sales")+xlab("Month")
gg_season(use, Sales, polar=TRUE) + ggtitle("seasonal Sales of dat")+ylab("Sales")+xlab("Month")
#descriptive stats : evaluating trend cycle , seasonality , random effects
# Sales increases with Month variable overtime, also there seems to be a seasonal trend in the 4th quarter of each year, no random effect observed.
#naive
SN.mod = model(use, SNAIVE(Sales))
RW.mod = model(use, RW(Sales~drift()))
SN.forc = forecast(SN.mod, h=24)
RW.forc = forecast(RW.mod, h=24)
autoplot(SN.forc, use, level=NULL, colour='Black')+autolayer(RW.forc, use, level = NULL, colour='Red')
#comparing RMSE of lm to naiveSN to naiveRW
reg0hat = predict(reg0,use)
reg0RSS = sum((use$Sales - reg0hat)^2)
reg0RSE = (reg0RSS/length(use$Sales))
reg0RMSE = sqrt(reg0RSE)
#decomposition
mod.add = model(use, classical_decomposition(Sales, type='additive'))
comp.add = components(mod.add)
comp.add
autoplot(comp.add)
comp.add.mse=mean(comp.add$random^2, na.rm=TRUE)
comp.add.rmse = comp.add.mse^0.5
mod.mul = model(use, classical_decomposition(Sales, type='multiplicative'))
comp.mul = components(mod.mul)
comp.mul
autoplot(comp.mul)
comp.mul.mse=mean(comp.mul$random^2, na.rm=TRUE)
comp.mul.rmse = comp.mul.mse^0.5; 
#evaluating RSME
reg0RMSE
accuracy(SN.mod)
accuracy(RW.mod)
comp.mul.rmse
comp.add.rmse
sd(use$Sales)
var(trendhat)
summary(reg0)$sigma^2
report(SN.mod)
report(RW.mod)
#Checking : autocorrelation
# e. plot the autocorrelation function for the time series. is there significant autocorrelation? what does this mean? 
# our autoplot(ACF()) show plot points above the blue dotted lines which means that our seasonality lag parameter and variables are significant to estimating Sales .. 1-12M seems to have significance under PACF and ACF ; similar results between lag = 12-20 periods. 
salesACF = ACF(use, lag_max=20)
autoplot(salesACF)
salesPACF = PACF(use, lag_max=20)
autoplot(salesPACF)
#futher testing
features(use, Sales, box_pierce, lag = 20, dof = 0)
features(use, Sales, ljung_box, lag = 20, dof = 0)
#   Ho (null) : data are uncorrelation , Ha (alternate) : data are correlated .. our pvalue is low = 0 < 0.01 .. we would reject the hypothesis that our data is uncorrelated .. we have autocorrelation in our data with significant values

#Q2
#exponential smoothing ETS models & forecasts
ETS.mod = model(use,ETS(Sales))
report(ETS.mod)
accuracy(ETS.mod)
#Chosen Model : ETS(M,A,M) , Estimated Coefficients : alpha = 0.1801 , beta = 0.0077 , gamma = 0.6646 , associated model error =  sigma^2 = 0.0001 , 
# RMSE = 6.97 , acf = 0.194
ETS.mod0 = model(use,MEAN(Sales))
report(ETS.mod0)
accuracy(ETS.mod0)
# benchmark RMSE = 57.3, ACF = 0.897
#graphing components
comp.ETS = components(ETS.mod)
autoplot(comp.ETS)
# d. using the model from part b, forecast the next year for monthly earnings. how confident would you be with respect to the forecasts? 
ETS.forc = forecast(ETS.mod, h=24)
autoplot(ETS.forc,use)
#in comparison to our mean model , the ets function performs much better with lower RMSE and seasonality factor being evaluated in our prediction
fit_sales_AAM = model(use, ETS(Sales~error("A")+trend("A")+season("M")))
accuracy(fit_sales_AAdM)
report(fit_sales_AAdM)
#arima modeling
#checking differencing
features(use, Sales, unitroot_kpss)
features(use, Sales, unitroot_ndiffs) # I(d) = 1 = need differencing once
#differencing
use = mutate(use,Sales.1=difference(Sales))
features(use, Sales.1, unitroot_ndiffs)#checking again I(d) = 0
#arima build
arima.mod = model(use, ARIMA(Sales.1))
report(arima.mod) #Chosen model : ARIMA(1,0,2),(0,1,1)[12] with drift 
accuracy(arima.mod)#RMSE = 6.70 , ACF = -0.0231 , compare sigma to sd(Sales)
aug.fit = augment(arima.mod)
autoplot(use, Sales)+autolayer(aug.fit, .fitted, colour="red")
features(aug.fit, .resid, ljung_box, lag = 20, dof=0)
