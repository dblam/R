dat = read.csv("promo_fe288.csv")
#check data
str(dat); head(dat)
dat0 = dat
dat0$Msize = as.factor(dat0$Msize) #large=1, medium=2, small=3
dat0$Msize = as.numeric(dat0$Msize)
#Quick Correlation Check
sort(abs(cor(dat0)[,7]), decreasing=TRUE)
#dat cleaning : data types changed to factor for qualitative variables
dat$Promo = as.factor(dat$Promo) 
dat$Week = as.factor(dat$Week)
dat$Market = as.factor(dat$Market)
dat$Msize = as.factor(dat$Msize)

#building training and test sets
set.seed(714988)
nr = nrow(dat);nr
b = nr*0.4;b 
train = sample(1:nr, b)
dat.tr = dat[train,] #40%
dat.tst = dat[-train,] #60%

#previews data
summary(dat);summary(dat.tr);summary(dat.tst)
sd(dat$Sales);sd(dat.tr$Sales);sd(dat.tst$Sales)
dim(dat);dim(dat.tr);dim(dat.tst)
plot(x=dat$Market, y=dat$Sales, main="Sales to Market", xlab="Market", ylab="Sales")
#data visuals
plot(x=dat$Promo, y=dat$Sales, main="Sales to Promotion Type", xlab="Promotion", ylab="Sales")
plot(x=dat$Msize, y=dat$Sales, main="Sales to Market Size", xlab="MarketSize", ylab="Sales")
plot(x=dat$Week, y=dat$Sales, main="Sales to Promo Duration (Weeks)", xlab="Promo Duration (Weeks)", ylab="Sales")
plot(x=dat$Store, y=dat$Sales, main="Sales to Store", xlab="Store", ylab="Sales")
plot(x=dat$Msize, y=dat$Age, main="Age to Market Size", xlab="MarketSize", ylab="Age")

#lm analysis
regall=lm(Sales~.,data = dat);summary(regall)
#quick regression on original data without store identifier
reg0 = lm(Sales~.-Store,data = dat);sr0=summary(reg0)
reg0.tr = lm(Sales~.-Store,data = dat.tr);sr0.tr=summary(reg0.tr)
reg0.tst = lm(Sales~.-Store,data = dat.tst);sr0.tst=summary(reg0.tst)
#r-squared with different data sets .. mean = 0.7748376
sr0$adj.r.squared;sr0.tr$adj.r.squared;sr0.tst$adj.r.squared
mean(sr0$adj.r.squared,sr0.tr$adj.r.squared,sr0.tst$adj.r.squared)
#evaluating models removing Promo
reg1 = lm(Sales~.-Store-Market,data = dat);sr1=summary(reg1);sr1
reg2 = lm(Sales~.-Store-Market-Promo,data = dat);sr2=summary(reg2);sr2
anova(regall,reg0,reg1,reg2)
#preferred model = reg1 , getting coefficients
reg1$coefficients
#reg1 : errors
reg1TSS = sum((dat$Sales-mean(dat$Sales))^2); reg1TSS
reg1RSS = with(summary(reg1), df[2] * sigma^2); reg1RSS #or anova(reg1)["Residuals", "Sum Sq"]
reg1RSE = (reg1RSS/nrow(dat));reg1RSE

#applying regall model to training data set
# regall.tr = lm(Sales~.,data = dat.tr);srall.tr=summary(reg1.tr);srall.tr
# anova(regall.tr)
# regallTSS.tr = sum((dat.tr$Sales-mean(dat.tr$Sales))^2); regallTSS.tr
# regallRSS.tr = with(summary(regall.tr), df[2] * sigma^2); regallRSS.tr #or anova(reg1)["Residuals", "Sum Sq"]
# regallRSE.tr = sqrt(regallRSS.tr/nrow(dat.tr));regallRSE.tr
#applying reg1 model to training data set
reg1.tr = lm(Sales~.-Store-Market,data = dat.tr);sr1.tr=summary(reg1.tr);sr1.tr
anova(reg1.tr)
reg1TSS.tr = sum((dat.tr$Sales-mean(dat.tr$Sales))^2); reg1TSS.tr
reg1RSS.tr = with(summary(reg1.tr), df[2] * sigma^2); reg1RSS.tr #or anova(reg1)["Residuals", "Sum Sq"]
reg1RSE.tr = (reg1RSS.tr/nrow(dat.tr));reg1RSE.tr

# OUT OF SAMPLE PREDICTION 
reg1.hat = predict(reg1.tr,dat.tst)
# evaluating test set prediction errors
# reg1TSS.tr = sum((dat.tr$Sales-mean(dat.tr$Sales))^2); reg1TSS.tr
reg1RSS.tst = sum((dat.tst$Sales - reg1.hat)^2)
reg1RSE.tst = (reg1RSS.tst/nrow(dat.tst))
reg1RSS.tst;reg1RSS.tr
reg1RSE.tst;reg1RSE.tr
reg1RMSE=sqrt(reg1RSE);reg1RMSE.tr=sqrt(reg1RSE.tr);reg1RMSE.tst=sqrt(reg1RSE.tst)

tab = matrix (c(reg1RMSE, reg1RMSE.tr, reg1RMSE.tst, reg1RSE, reg1RSE.tr, reg1RSE.tst, reg1RSS, reg1RSS.tr, reg1RSS.tst), ncol=3, byrow=FALSE)
colnames(tab) = c("RMSE", "RSE", "RSS")
rownames(tab) = c("original", "train", "test")
tab

reg1.tr$coefficients
