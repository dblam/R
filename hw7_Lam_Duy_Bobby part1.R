# FE288 - Business Analytics 
# Duy Bobby Lam | dblam@uci.edu
# Professor Ken Murphy
# Nov. 27, 2024
# HW 7 Classification Models

#1 perform cleaning & transformation for logistic regression & other classification methods..
# a. copy script in appendix to window and run it..
dat <- read.csv("hw7_288_bank_term_deposit_big.csv")
str(dat)
#
#  Create factor for some variables
#
jb <- as.factor(dat$job)
mari <- as.factor(dat$marital)
ed <- as.factor(dat$education)
cntct <- as.factor(dat$contact)
m <- as.factor(dat$month)
pout <- as.factor(dat$poutcome)
#
#  Convert variables to indicators
#    Note some variables have 2 levels and some have more
#
tmp_job <- data.frame(model.matrix(~jb - 1))
tmp_marit <- data.frame(model.matrix(~mari - 1))
tmp_educ <- data.frame(model.matrix(~ed - 1))
tmp_contact <- data.frame(model.matrix(~cntct - 1))
tmp_month <- data.frame(model.matrix(~m - 1))
tmp_poutcome <- data.frame(model.matrix(~pout - 1))
dat$loan <- as.numeric(as.factor(dat$loan)) - 1
dat$default <- as.numeric(as.factor(dat$default)) - 1
dat$housing <- as.numeric(as.factor(dat$housing)) - 1
dat$deposit <- as.numeric(as.factor(dat$deposit)) - 1
#
#  Bind stuff together in a new data frame
#
names(dat)
dat1 <- cbind(dat[,c(17,1,5:8,10,12:13,15)],
              tmp_job[,1:11], tmp_marit[,1:2], tmp_educ[,1:3],
              tmp_contact[,1:2], tmp_month[,1:11], 
              tmp_poutcome[,1:3])
names(dat1)
#
#  Get rid of stuff not used later for simplicity
#
rm(tmp_job, tmp_marit, tmp_contact,
   tmp_month, tmp_poutcome, tmp_educ)
rm(jb, mari, ed, cntct, m, pout)
# b. what is the name of resulting data grame? 'dat1' .. how many rows and columns does it have ? rows=45211 , columns=42
str(dat1)
dim(dat1)
# c. what is the Y variable here? Deposit (Y/N) .. what are teh unit of analysis? ... (0, 1) = (No , Yes)
# d. what transformation occurred to the data set using model.matrix command? .. allows assignment of an integer to original qualitative data to be evaluated .. similar to factor but rather than changing the values from Y/N to 1/0 , we pair them to a dummy integer representing the original Y=1/N=0

#2 compute relevant correlations for this set of variables..
# a. what 3 variables appear most strongly correlated with customers who decide to make the term deposit, that is, what predictors are most highly correlated with the 'deposit' variable? ... housing , poutsucess , duration 
cory <-cor(dat1)[1,]
#order(cory)
cory[order(cory)]
# b. explain why these correlations are intuitive or counter-intuitive.. has housing loan (Y/N) , poutcome (unkown, other, failure, success) , duration.. housing being counter-intuitive is due to owners having already invested into CD products; the correlation is negative, poutcome correlates to marketing campaign translate to communication selling efforts of CD product offerings, and duration of the deposit term will have a positive impact based on yields from the CD asset

#3 creating a training & testing data set of 4000 observations each that are randomly selected from the entire data set .. ensure that both are equal on the 'deposit variable' i.e., the training set should include 2000 customers that made the deposit , & 2000 that did not .. same for the test
# a. create a 4000 row training set with a 5050 split on the response variable 'deposit'
table(dat1$deposit)
dat1.depo <- dat1[dat1$deposit == 1,]
dat1.nodepo <- dat1[dat1$deposit == 0,]
set.seed(123456)
train.depo <- sample(1:nrow(dat1.depo),2000)
train.nodepo <- sample(1:nrow(dat1.nodepo),2000)
dat1.tr <- rbind(dat1.depo[train.depo,],
                 dat1.nodepo[train.nodepo,])
table(dat1.tr$deposit)
# b. create a 4000 row test set with 5050 split on the response variable
dat1.depo.notr <- dat1.depo[-train.depo,]
dat1.nodepo.notr <- dat1.nodepo[-train.nodepo,]  
tst.depo <- sample(1:nrow(dat1.depo.notr),2000)
tst.nodepo <- sample(1:nrow(dat1.nodepo.notr),2000)
dat1.tst <- rbind(dat1.depo.notr[tst.depo,],
                  dat1.nodepo.notr[tst.nodepo,])
table(dat1.tr$deposit)
table(dat1.tst$deposit)
# c. why is it useful to use a 5050 split?.. we want to ensure that our training and test sets are of equal splits to neutralize the intended effect of correlations from predictor and response variables having a weighted influence on our model during the training process.. thus, we are eliminating any potential spread in observations
table(dat1$deposit)

#4 using the training data 'dat1.tr' fit a logistic regression model using all predictor variables, & evaluate the model's performance on the training data ('dat1.tr')
# a. run the logistic regression
mod4 <- glm(deposit ~ ., data = dat1.tr, family = "binomial")
# b. identify teh significant variables.. housing , loan , duration , campaign , 2ndary education , contact , month , outcome
summary(mod4)
# c. using the logistic regression model & the training data set 'dat1.tr' make predictions of the customers behaviors..
yhat4.pr <- predict(mod4, dat1.tr, type = "response")
yhat4.pr
# d. run the confusion matrix using the predicted and actual data in 'dat1.tr' .. how was your overall performance, that is , what is your overall misclassification error? we are about 84% correct in our model prediction .. leaving 15.8% in errors between predicted and actual
yhat4.cl <- ifelse(yhat4.pr > 0.5, 1, 0)  
table(Actual = dat1.tr$deposit, Predicted = yhat4.cl)  
tab4 <- table(Actual = dat1.tr$deposit, Predicted = yhat4.cl) 
tab4
err4 <- (tab4[1,2] + tab4[2,1])/sum(tab4)
err4

#5 now take the logistic regression model estimated in Q4 & apply it to the test data to see how it performs in the 'out of sample' setting ..
# a. make predictions using 'mod4' on 'dat1.tr'.. convert the probability predictions to 0 and 1 ..
yhat5.pr <- predict(mod4, dat1.tst, type = "response")
yhat5.cl <- ifelse(yhat5.pr > 0.5, 1, 0)
yhat5.cl 
# b. create a confusion matrix & report the results
tab5 <- table(Actual = dat1.tst$deposit, Predicted = yhat5.cl) 
tab5
err5 <- mean(dat1.tst$deposit != yhat5.cl)
err5
# c. is this a 'high-variance' case that is, did the error increase significantly on the test data wehn comparing it to the training data? .. the error went from 15.8% to 16.9% which was not bad .. it was about the same proportion

#6 fit a logistic regression using any subset of variables of your choice to predict the customers who will make the term deposit ..
# a. justify your choice of variables
#housing , loan , duration , campaign , contact , outcome .. for this part i have decided to reduce the number of predictor variables by using their significance in mod4.. additionaly, months was excluded to keep a more generallized impact of contact type and campaign on the outcome
str(dat1.tr)
mod5 <- glm(deposit ~ housing + loan + duration + campaign + cntctcellular + cntcttelephone + poutsuccess, data = dat1.tr, family = "binomial")
mod0 = glm(deposit~0, data=dat1.tr)
summary(mod4); summary(mod5)
anova(mod0,mod4,mod5)
# b. create a confusion matrix using the training data 'dat1.tr' to verify the bias of the mod5
yhat6.pr <- predict(mod5, dat1.tr, type = "response")
yhat6.pr
yhat6.cl <- ifelse(yhat6.pr > 0.5, 1, 0)
table(Actual = dat1.tr$deposit, Predicted = yhat6.cl)  
tab6 <- table(Actual = dat1.tr$deposit, Predicted = yhat6.cl) 
tab6
err6 <- (tab6[1,2] + tab6[2,1])/sum(tab6)
err6
# c. present the associated confusion matrix from predictions on the test data, 'dat1.tst' to evaluate the variance of the model.. our predictions has gotten worst in terms of errors.. it seems like i took out too many important variables and which would have been better to be left in..
#table(Actual = dat1.tr$deposit, Predicted = yhat6.cl)  
yhat6.pr2 <- predict(mod5, dat1.tst, type = "response")
yhat6.pr2
yhat6.cl2 <- ifelse(yhat6.pr2 > 0.5, 1, 0)
tab6.2 <- table(Actual = dat1.tst$deposit, Predicted = yhat6.cl2) 
tab6.2
err6.2 <- (tab6.2[1,2] + tab6.2[2,1])/sum(tab6.2)
err6.2

#7 use KNN to predict the membership of ea obs. in the data set, that is to predict 'deposit' or 'no deposit' .. present teh associated confusion matrix & overall error rate on the test set.
# a. run kNN with atleast (3) odd values : 1,5,21
yhat7.1 <- knn(dat1.tr[,-1], dat1.tst[,-1], dat1.tr[,1], 1)
yhat7.5 <- knn(dat1.tr[,-1], dat1.tst[,-1], dat1.tr[,1], 5)
yhat7.21 <- knn(dat1.tr[,-1], dat1.tst[,-1], dat1.tr[,1], 21)
# b. now scale the data with the same (3) values
#scaling the data
#k=1
tab7.1 <- table(Actual = dat1.tst$deposit, Predicted = yhat7.1) 
tab7.1 <- table(Actual = dat1.tst$deposit, Predicted = yhat7.1) 
tab7.1
err7.1 <- mean(dat1.tst$deposit != yhat7.1)
err7.1
#k=5
tab7.5 <- table(Actual = dat1.tst$deposit, Predicted = yhat7.5) 
tab7.5
err7.5 <- mean(dat1.tst$deposit != yhat7.5)
err7.5
#k=21
tab7.21 <- table(Actual = dat1.tst$deposit, Predicted = yhat7.21) 
tab7.21
err7.21 <- mean(dat1.tst$deposit != yhat7.21)
err7.21
err7.1;err7.5;err7.21
#scale
dat1.tr.s <- dat1.tr
dat1.tst.s <- dat1.tst
dat1.tr.s[,c(2,4,7:9)] <- scale(dat1.tr.s[,c(2,4,7:9)])
dat1.tst.s[,c(2,4,7:9)] <- scale(dat1.tst.s[,c(2,4,7:9)])
#run test again
tab7.1tr <- table(Actual = dat1.tr.s$deposit, Predicted = yhat7.1) 
tab7.1tst <- table(Actual = dat1.tst.s$deposit, Predicted = yhat7.1) 
tab7.1tr;tab7.1tst
err7.1tr <- mean(dat1.tr.s$deposit != yhat7.1)
err7.1tst <- mean(dat1.tst.s$deposit != yhat7.1)
err7.1tr;err7.1tst;err7.1
#
tab7.5tr <- table(Actual = dat1.tr.s$deposit, Predicted = yhat7.5) 
tab7.5tst <- table(Actual = dat1.tst.s$deposit, Predicted = yhat7.5) 
tab7.5tr;tab7.5tst
err7.5tr <- mean(dat1.tr.s$deposit != yhat7.5)
err7.5tst <- mean(dat1.tst.s$deposit != yhat7.5)
err7.5tr;err7.5tst;err7.5
#
tab7.21tr <- table(Actual = dat1.tr.s$deposit, Predicted = yhat7.21) 
tab7.21tst <- table(Actual = dat1.tst.s$deposit, Predicted = yhat7.21) 
tab7.21tr;tab7.21tst
err7.21tr <- mean(dat1.tr.s$deposit != yhat7.21)
err7.21tst <- mean(dat1.tst.s$deposit != yhat7.21)
err7.21tr;err7.21tst;err7.21
# the best or lowest error value produced was k = 21

#8 fit a classification tree to the training data to predict 'deposit' .. plot the tree . comment on the result.
# a. fit the classification tree on the test data. how well does the tree fit on this data?.. our tree has 3 branch levels via duration and eventually further split between poutsuccess and housing .. this fits our data pretty well
library(tree)
dat1.tr.f <- dat1.tr
dat1.tst.f <- dat1.tst
dat1.tr.f$deposit <- as.factor(dat1.tr.f$deposit)
dat1.tst.f$deposit <- as.factor(dat1.tst.f$deposit)
tree8 <- tree(deposit ~ ., data = dat1.tr.f)
tree8
# b. how many terminal nodes are there in the tree from part a? terminal nodes = 7 .. how many of the terminal nodes are assigned a '1' vs '0' ? (4)=1s , (3)=0s.. what is the variable and split value for the top split in the tree? duration < 346.5 & duration >= 346.5
# c. plot the tree which was fit in part a
plot(tree8)
text(tree8, pretty = 0)
# d. use the tree from part a to classify deposit behavior on the test data set .. create the confusion matrix & compute associated error
yhat8.cl <- predict(tree8, dat1.tst, type = "class")
tab8 <- table(Actual = dat1.tst$deposit, Predicted = yhat8.cl) 
tab8
err8 <- mean(dat1.tst$deposit != yhat8.cl)
err8

#9	Prune the tree from the previous question and select a reasonable sized subtree..
# a.	Prune the tree from Q8.  Pick the pruned tree that is preferred. 
prune9 <- prune.misclass(tree8)
prune9
plot(prune9)
ptree9 <- prune.misclass(tree8, best = 3)
# b. plot the chosen tree
plot(ptree9)
text(ptree9, pretty = 0)
# c.	Compare the test error rate of the two trees (the pruned tree from part a, and the tree from Q8) and comment on the difference. 
yhat9.cl <- predict(ptree9, dat1.tst, type = "class")
tab9 <- table(Actual = dat1.tst$deposit, Predicted = yhat9.cl) 
tab9
err9 <- mean(dat1.tst$deposit != yhat9.cl)
err9
err8
# .. there was an increase in error percentage from err8 to err9 after tree pruning .. this is expected with less predictor variable parameters..
# d.	What is the argument for using a pruned version of the tree? about 90% of our observations is predicted using the best 3 approach .. making the model less complex is desired to capture more variability in our data

#10.	Compare all the techniques implemented here. 
# a.	Which technique and model performed best in out of sample prediction?  How do you know?  
# by calculating the err of each model , logistic regression from mod4 seem like be best model for this data set .. however, in other data sets, the best model could be different
#   b.	Which model is your choice?  Why?
# my model of choice would be decision trees based on the simplicity of explaining predictions through bucket splitting, this seem like the most intuitive approach to explain probabilities between decisions.
#   c.	What are three key takeaways for the bank’s management regarding customers who do or do not make these deposits?  Asked another way, what are the meaningful variables and how meaningful do these variables appear to be? .. Depending on the data set and applications, different models could be more useful in our analysis and process for developing a sound model.. 
#   
  


