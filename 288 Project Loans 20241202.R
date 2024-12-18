# Group 6 : FE288
# Bobby Lam , Jeremy Lim , Tyler Nguyen
# Predicting Loan Defaults


dat = read.csv("Loan_default.csv")
str(dat)


# make stuff numeric - this section by jeremy
dat$HasMortgage <- as.numeric(as.factor(dat$HasMortgage)) - 1
dat$HasDependents <- as.numeric(as.factor(dat$HasDependents)) - 1
dat$HasCoSigner <- as.numeric(as.factor(dat$HasCoSigner)) - 1
ed <- as.factor(dat$Education)
emptype <- as.factor(dat$EmploymentType)
marital <- as.factor(dat$MaritalStatus)
loanpurp <- as.factor(dat$LoanPurpose)

tmp_educ <- data.frame(model.matrix(~ed - 1))
tmp_EmployType <- data.frame(model.matrix(~emptype - 1))
tmp_MaritalStat <- data.frame(model.matrix(~marital - 1))
tmp_loanpurpo <- data.frame(model.matrix(~loanpurp - 1))
names(dat)
dat <- cbind(dat[,c(2:10,14,15,17,18)],tmp_educ[,1:4],tmp_EmployType[,1:4],tmp_MaritalStat[,1:3],tmp_loanpurpo[,1:5])
str(dat)

# 
names(dat)
# columns
# #  [1] "Age"                  "Income"               "LoanAmount"          
# [4] "CreditScore"          "MonthsEmployed"       "NumCreditLines"      
# [7] "InterestRate"         "LoanTerm"             "DTIRatio"            
# [10] "HasMortgage"          "HasDependents"        "HasCoSigner"         
# [13] "Default"              "edBachelor.s"         "edHigh.School"       
# [16] "edMaster.s"           "edPhD"                "emptypeFull.time"    
# [19] "emptypePart.time"     "emptypeSelf.employed" "emptypeUnemployed"   
# [22] "maritalDivorced"      "maritalMarried"       "maritalSingle"       
# [25] "loanpurpAuto"         "loanpurpBusiness"     "loanpurpEducation"   
# [28] "loanpurpHome"         "loanpurpOther" 
# end section by jeremy



#random seed point - this section is by Bobby
set.seed(2345678)

# original data set .. 88.39% did not default , 11.61% defaulted of 255347 rows
# visualizing proportion of Default Category 
# yes.dat = subset(dat, dat$Default == 1)
# no.dat = subset(dat, dat$Default == 0)
# length(yes.dat$Default)
# length(no.dat$Default)
# percent.yes = length(yes.dat$Default)/length(dat$Default)
# percent.no = 1-percent.yes
# 100*percent.yes
# 100*percent.no
# length(dat$Default)

# 88.39% did not default , 11.61% defaulted of 255347 rows
# simple way to visualize proportions .. use : table(dat$Default)
table(dat$Default)

# drawing & validating 10% of randomized rows from our data set
ten_s = sample(1:length(dat$Default), 0.1*length(dat$Default)) # create 10% or 25534 random numbers between: 0 to length of dat$Default
length(ten_s) # confirming the length of ten_s
head(sort(ten_s, decreasing = TRUE)) #confirming max randomized numbers are greateer than length of ten_s

#downsizing 'dat' from 255k rows to 25k rows & saving it to 'dat1'
dat1 = dat[ten_s,] # drawing out the 25534 random observations from dat and saving to dat1

# checking proportion of 10% sub-sample .. 88.47% yes , 11.53% no of 25534 rows
# also.. creating & filtering subsets to create training and test sets
yes.dat1 = subset(dat1, dat1$Default == 1)
no.dat1 = subset(dat1, dat1$Default == 0)
length(yes.dat1$Default)
length(no.dat1$Default)
percent1.yes = length(yes.dat1$Default)/length(dat1$Default)
percent1.no = 1-percent1.yes
100*percent1.yes
100*percent1.no
length(dat1$Default)
#proportion of 10% sub-sample .. 88.47% yes , 11.53% no of 25534 rows
# again .. simple way to visualize proportions..
dat1.count = table(dat1$Default)
dat1.count
2943/(2943+22591)

# another 10% from 25534 & round to nearest even number , so this is really .. 1% of our total data 
minisample = 2*(round(0.1*length(dat1$Default)/2)) #10% of length(dat1$Default) rounded to even number
length(dat1$Default)
mini.yes = sample(1:length(yes.dat1$Default), minisample/2);length(mini.yes) #1277 .. randomly select row #s from 1:length of yes.dat1
mini.no = sample(1:length(no.dat1$Default), minisample/2);length(mini.no) #1277 .. randomly select row #s from 1:length of no.dat1

#note : length of yes.dat1 and no.dat1 are not the same .. they are 11:89 .. we are currently pulling 50:50 of a 10% subset of dat1 to create a 'fair' training set

#training data set = 2554 rows = 10% of dat1
dat.train <- rbind(yes.dat1[mini.yes,],no.dat1[mini.no,])
str(dat.train); length(dat.train$Default) #2554/25534 = 10%

#this part still confuses me in proportion but math works..
remaining.no = no.dat1[-mini.no,]
length(remaining.no$Default)
#generate random row numbers = length of remaining, size = 1/2 of dat1 length
no.test = remaining.no[sample(1:length(remaining.no$Default), (25534)/2),]
length(no.test$Default) # 12767
length(yes.dat1[-mini.yes,]$Default)
#test set will be remaining yes & 1/2of dat length of no?
dat.test = rbind(yes.dat1[-mini.yes,], no.test)

#saving proportions of default in training and test sets
dat.train.count = table(dat.train$Default)
dat.test.count = table(dat.test$Default)
dat.test.count
dat.train.count

#checking if test set is proportion to dat1 sample 
1666/(12767+1666) #0.1154299
2943/(2943+22591) #0.1152581

#total length are not equal like class example but we have same proportions..
length(dat1$Default)
length(dat.test$Default)+length(dat.train$Default)

# Exporting Subsets for Visuals
# library(readr)
# write.csv(dat1, "loans_10percent.csv")
# write.csv(dat.test, "loans_test_set.csv")
# write.csv(dat.train, "loands_train_set.csv")
# str(dat.test)
# End section by Bobby 
# 
# 


# find correlation of variables. this section by jeremy
str(dat.train)
cor(dat.train)[13,]

# make model 
mod <- glm(Default ~ ., data = dat.train, family = "binomial")
summary(mod)
# 
# make prediction part
yhat.pr <- predict(mod, dat.train, type = "response")
yhat.pr
yhat.cl <- ifelse(yhat.pr > 0.5, 1, 0)
# make confusion matrix
table(Actual = dat.train$Default, Predicted = yhat.cl)
tab <- table(Actual = dat.train$Default, Predicted = yhat.cl)
tab
# find error
err <- (tab[1,2] + tab[2,1])/sum(tab)
err
# 0.316758 error
#
# GLM : training accuracy = 68%
accuracy.glm = sum(diag(tab))/length(dat.train$Default)
accuracy.glm
# 
# make model predict on test data
# 
yhat1.pr <- predict(mod, dat.test, type = "response")
yhat1.cl <- ifelse(yhat1.pr > 0.6, 1, 0)
yhat1.cl
str(yhat1.pr)
# 
# Hint 2:    To create the confusion matrix:
tab1 <- table(Actual = dat.test$Default, Predicted = yhat1.cl)
tab1
err1 <- mean(dat.test$Default != yhat1.cl)
err1
write.csv(tab1, "loans_predicted.csv")
# err (training) and err1 (test) are pretty close. within ~0.003. This is good.
# end section by jeremy
accuracy.glm2 = sum(diag(tab1))/length(dat.test$Default)
accuracy.glm2
#GLM : test accuracy = 68%

#Bobby (cont) 
str(dat.train)
anova(mod, test='Chisq') #checking residuals & Pr(>Chi) ... 

#low Pr(>Chi) with *,**,*** means that the coefficients have a significant relationship with our predictor variable in comparison to model w no prediction variables aka NULL

#better method to remove predictor variales.. step() with the criteria being the LRT to reduce unneeded variables from the model ... REF : https://sscc.wisc.edu/sscc/pubs/glm-r/
step(mod,test='LRT')

#basic model used to make comparison
mod0 = glm(Default~Income,data=dat.train,family='binomial')

#2nd model with only significant variables listed in step()
mod2 <- glm(Default ~ Age + Income + LoanAmount + CreditScore + MonthsEmployed + NumCreditLines + InterestRate + DTIRatio + HasMortgage + HasDependents + HasCoSigner + edBachelor.s + emptypeFull.time + emptypePart.time + emptypeSelf.employed + loanpurpBusiness + loanpurpEducation + loanpurpHome, data = dat.train, family = "binomial")

#making comparisons
anova(structure(list(mod0,mod,mod2),class='glmlist'),test='Chisq')
#REF: https://docs.tibco.com/pub/enterprise-runtime-for-R/6.0.0/doc/html/Language_Reference/stats/anova.glm.html#:~:text=If%20anova.,if%20an%20intercept%20was%20requested).
#anova comparison interpretation ... In a Generalized Linear Model (GLM), a "negative deviance" essentially means that the model is performing better than the "null model" (a model with only an intercept), indicating a good fit to the data; a lower negative deviance signifies a better fit, while a higher negative deviance suggests a poorer fit. 

# 
# make prediction part
yhat.pr2 <- predict(mod2, dat.train, type = "response")
yhat.pr2
yhat.cl2 <- ifelse(yhat.pr2 > 0.6, 1, 0)
# make confusion matrix
table(Actual = dat.train$Default, Predicted = yhat.cl2)
tab2 <- table(Actual = dat.train$Default, Predicted = yhat.cl2)
tab2
# find error
err2 <- (tab2[1,2] + tab2[2,1])/sum(tab2)
err2
# 0.316758 error1
# 0.3136257 error2
accuracy.mod2 = sum(diag(tab2))/length(dat.train$Default)
accuracy.mod2
# GLM mod2 train Accuracy = 0.6863743
# 
# make model predict on test data
# 
yhat1.pr2 <- predict(mod2, dat.test, type = "response")
yhat1.cl2 <- ifelse(yhat1.pr2 > 0.6, 1, 0)
yhat1.cl2
# 
# Hint 2:    To create the confusion matrix:
tab1.2 <- table(Actual = dat.test$Default, Predicted = yhat1.cl2)
tab1.2
err1.2 <- mean(dat.test$Default != yhat1.cl2)
err1.2 
# err (training) and err1 (test) are pretty close. within ~0.003. This is good.
# err2(training) = 0.3136257 , err1.2(test) = 0.3207652
accuracy.mod2t = sum(diag(tab1.2))/length(dat.test$Default)
accuracy.mod2t
#GLM mod2 - test accuracy

#kNN
#install.packages("class")
library(class)
#k=5 , err = 0.4589687
kpred = knn(train=dat.train,test=dat.test,cl=dat.train$Default,k=5)
actualDefault=dat.test$Default
tab.k = table(actualDefault,kpred)
tab.k
err.k = mean(actualDefault != kpred)
err.k
accuracy.k = sum(diag(tab.k))/length(actualDefault)
accuracy.k
#k=10 , err2 = 0.4340865
kpred2 = knn(train=dat.train,test=dat.test,cl=dat.train$Default,k=10)
tab.k2 = table(actualDefault,kpred2)
tab.k2
err.k2 = mean(actualDefault != kpred2)
err.k2
accuracy.k2 = sum(diag(tab.k2))/length(actualDefault)
accuracy.k2
#k=1 err3 = 0.4696424
kpred3 = knn(train=dat.train,test=dat.test,cl=dat.train$Default,k=1)
tab.k3 = table(actualDefault,kpred3)
tab.k3
err.k3 = mean(actualDefault != kpred3)
err.k3
accuracy.k3 = sum(diag(tab.k3))/length(actualDefault)
accuracy.k3
#kNN 100 loop , not really needed but nice to see differences in k errors
knn.err <- 1:50
xrange <- 1:50
for (j in 1:99) {
  if (j %% 2 != 0) { #testing if j is even .. remainder != 0
    xrange[(j+1)/2] <- j
    out <- knn(train=dat.train,test=dat.test,cl=dat.train$Default,j)
    knn.err[(j+1)/2] <- mean(out != actualDefault)
  }
}
xrange
knn.err
mean(knn.err)
plot(xrange, knn.err, xlab = "Value of K (K odd)",
     ylab = "Error from KNN")


# decision tree
library(tree)
treemod1 <- tree(as.factor(Default)~., data = dat.train)
summary(treemod1)
plot(treemod1)
text(treemod1, pretty = 0)
#general tree before pruning .. , error = 0.3738564 , accuracy = 0.6261436
yhat.cl.t <- predict(treemod1, dat.test, type = "class")
tabtree <- table(Actual = actualDefault, Predicted = yhat.cl.t) 
tabtree
tree.err <- mean(actualDefault != yhat.cl.t)
tree.err
accuracy.tree = sum(diag(tabtree))/length(actualDefault)
accuracy.tree
#pruning , err = 0.3738564 , accuracy = 0.6261436
prune = prune.misclass(treemod1)
plot(prune)
ptreemod1 = prune.misclass(treemod1, best = 2)
yhat.cl.t2 <- predict(ptreemod1, dat.test, type = "class")
tabtree2 <- table(Actual = actualDefault, Predicted = yhat.cl.t2) 
tabtree2
tree.err2 <- mean(actualDefault != yhat.cl.t2)
tree.err2
accuracy.tree2 = sum(diag(tabtree2))/length(actualDefault)
accuracy.tree2

# tree bagging .. poppular for classification
#install.packages("rpart")
library(rpart)
#install.packages("mlbench")
library(mlbench)
#install.packages("caret")
library(caret)
#install.packages("ipred")
library(ipred)
mod.bag = bagging(Default~., dat=dat.train,coob=TRUE)

#bagging on training data .. err=0.3163665
bag.hat = predict(mod.bag, dat.train, type = "response") 
yhat.bag <- ifelse(bag.hat > 0.5, 1, 0)
yhat.bag
# 
length(yhat.bag)
length(actualDefault)
# Hint 2:    To create the confusion matrix:
tab.bag <- table(Actual = dat.train$Default, Predicted = yhat.bag)
tab.bag
err.bag <- mean(dat.train$Default != yhat.bag)
err.bag

#bagging on test data .. err= 0.3194483
bag.hat2 = predict(mod.bag, dat.test, type = "response")
bag.hat2
yhat.bag2 <- ifelse(bag.hat2 > 0.5, 1, 0)
yhat.bag2
tab.bag2 <- table(Actual = dat.test$Default, Predicted = yhat.bag2)
tab.bag2
err.bag2 <- mean(dat.test$Default != yhat.bag2)
err.bag2
accuracy.tree3 = sum(diag(tab.bag2))/length(actualDefault)
accuracy.tree3
anova(structure(list(mod0,mod,mod2),class='glmlist'),test='Chisq')
