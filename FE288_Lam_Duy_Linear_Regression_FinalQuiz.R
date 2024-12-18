dat = read.csv("promo_fe288_for_final.csv")
str(dat)

#Question1
dat$Promo = as.factor(dat$Promo) 
reg1 = lm(Sales~Promo+Age,data=dat)
summary(reg1)
newobs=data.frame(1,"Medium",1,3,3,1,0)
names(newobs) = names(dat)
str(newobs)
newobs$Promo = as.factor(newobs$Promo)
predict(reg1,newobs, interval="prediction", level=0.95)

#Question2
dat.cor = dat
dat.cor$Msize = as.factor(dat.cor$Msize) #large=1, medium=2, small=3
dat.cor$Msize = as.numeric(dat.cor$Msize)
dat.cor$Promo = as.numeric(dat.cor$Promo)
str(dat.cor)
sort(abs(cor(dat.cor)[,7]), decreasing=TRUE) #quick correlation test
str(dat)
dat$Promo = as.factor(dat$Promo) 
dat$Week = as.factor(dat$Week)
dat$Market = as.factor(dat$Market)
dat$Msize = as.factor(dat$Msize)
set.seed(714988)
nr = nrow(dat);nr
b = 485;b 
train = sample(1:nr, b)
dat.tr = dat[train,] #40%
dat.tst = dat[-train,] #60%
nrow(dat.tr)
nrow(dat.tst)
#using leaps library to quicky find the best combination
library(leaps)
regfit.full <- regsubsets(Sales~.-Store, dat.tr)
summary(regfit.full)
summary(regfit.full)$rsq
#building model Q2.b
reg0.tr = lm(Sales~.-Store-Market,data = dat.tr);sr0.tr=summary(reg0.tr);sr0.tr
reg0.tr$coefficients
reg0.hat = predict(reg0.tr,dat.tst)
#evaluating errors
reg0TSS.tr = sum((dat.tr$Sales-mean(dat.tr$Sales))^2); reg0TSS.tr
reg0RSS.tr = with(summary(reg0.tr), df[2] * sigma^2); reg0RSS.tr #or anova(reg1)["Residuals", "Sum Sq"]
reg0RSE.tr = (reg0RSS.tr/nrow(dat.tr));reg0RSE.tr
reg0RMSE.tr = sqrt(reg0RSE.tr);reg0RMSE.tr
#reg0TSS.tst = sum((dat.tst$Sales-mean(dat.tst$Sales))^2); reg0TSS.tst
reg0RSS.tst = sum((dat.tst$Sales - reg0.hat)^2)
reg0RSE.tst = (reg0RSS.tst/nrow(dat.tst))
reg0RMSE.tst=sqrt(reg0RSE.tst)
tab = matrix (c(reg0RMSE.tr, reg0RMSE.tst, reg0RSE.tr, reg0RSE.tst, reg0RSS.tr, reg0RSS.tst), ncol=3, byrow=FALSE)
colnames(tab) = c("RMSE", "RSE", "RSS")
rownames(tab) = c("train", "test")
tab
sd(dat$Sales);sd(dat.tr$Sales);sd(dat.tst$Sales)
mean(sd(dat$Sales),sd(dat.tr$Sales),sd(dat.tst$Sales))

#data visuals
plot(x=dat$Promo, y=dat$Sales, main="Sales to Promotion Type", xlab="Promotion", ylab="Sales")
plot(x=dat$Msize, y=dat$Sales, main="Sales to Market Size", xlab="MarketSize", ylab="Sales")
plot(x=dat$Week, y=dat$Sales, main="Sales to Promo Duration (Weeks)", xlab="Promo Duration (Weeks)", ylab="Sales")
plot(x=dat$Store, y=dat$Sales, main="Sales to Store", xlab="Store", ylab="Sales")
plot(x=dat$Msize, y=dat$Age, main="Age to Market Size", xlab="MarketSize", ylab="Age")
