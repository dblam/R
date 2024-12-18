#
#  Predictive Analytics (FE 288) Week 7 
#  Introduction to Classification Models
#  Last updated:  11/16/24
#
#  These notes cover:
#  1.  Classification data (y is a qualitative variable)
#  2.  Creating a training and test data sets for classification
#  3.  Logistic regression model fitting
#  4.  K-Nearest Neighbor
#  5.  Building and Pruning Classification Trees
#

#
#  There are a two new packages used here:  
#  "class" and "tree"



#  
#  Reading in the Universal Bank Data
#  Setting up training and test data sets
#
dat <- read.csv("week7_universal_bank_for_Mac.csv")
str(dat)
names(dat)
#
#  Checking to see what percentage and number took the 
#    loan offer (Acpt_Offer variable)
#
dat$Acpt_Offer
mean(dat$Acpt_Offer)
loan.count <- table(dat$Acpt_Offer)
loan.count
loan.perc <- table(dat$Acpt_Offer)/nrow(dat)
loan.perc
#
#  Now set up our data for this week.
#  There are 3 education variables - 
#    One integer variable with 3 levels and 
#    two indicator variables corresponding 
#    to 2 of the 3 levels.  NO need for the
#    first (integer) variable.  Also get rid 
#    of the ID as it is not germane to the 
#    analysis.  Finally, put the "Y" variable 
#    first in data set-just a personal pet 
#    peeve!
#
names(dat)
dat0 <- dat
#reducing & reorganizing columns
dat <- dat[,c(15,2:6,10,8:9,11:14)]
names(dat)
head(dat)
str(dat)





#
#  Build a training and test data sets for 
#   Classification Problems
#
#  There are a total 256 of the 2500 observations
#    who accepted the Bank's offer
#
#  The training data will be 128 (half) 
#    of those 256 and another 128 of those 
#    customers who did not take the loan 
#    offer.

# splitting the training data to 128:128 of 1s:0s
#
#  Q:  Why would you want to make the "successes"
#    equal to the "fails"?
#
#
#  The test data will be the other half (128) 
#    who took the loan and a subset of the 
#    remainder of those who did not take 
#    the loan offer.  More details below.
#
#  First separate the data into two the groups, 
#    the "Successes" (took the loan) and the "Fails"
#    who did not
#
#  Several ways to to this, "subset" is one of 
#    them.
#
succ <- subset(dat, dat$Acpt_Offer == 1)
fail <- subset(dat, dat$Acpt_Offer == 0)
#
#  Data set "succ" contains only customers 
#    who took the loan, "successes"
#  Data set "fail" has only customers who
#    turned the offer down or "failures"
#
#  Check the first column to verify
#
nrow(succ)
succ[,1]
nrow(fail)
fail[1:100,1]
#
#  
#  Now create training and test data sets
#
#  Set a seed to get reproducible results
#  Then use the "sample" command to select 
#  random rows for the training and test data
#
set.seed(112233) 
#
#  Randomly select 128 numbers from each group of 
#    of numbers (1-->256 and 1-->2244)
#
tr.succ <- sample(1:nrow(succ),128)
tr.fail <- sample(1:nrow(fail),128)
tr.succ
tr.fail
#
#  Q:  Do any of the above numbers repeat?  
#
#  To complete the creation of the training and 
#    test data sets use rbind to add the rows 
#    in the success and failure samples back 
#    together.  Create a single data frame, 
#    the training data set, of 256 observations, 
#    called, dat.train.  
#
#  Then, repeat the process to create a test data 
#    set of 1250 observations
#
dat.tr <- rbind(succ[tr.succ,],fail[tr.fail,])
dim(dat.tr)
loan.count.tr <- table(dat.tr$Acpt_Offer)
loan.count.tr
#
#  Now take a sample of 1122 of the "failure" 
#    observations from the 2244 - 128 = 2116 
#    that are left over after building the 
#    training data set
#
newfail <- fail[-tr.fail,]
dim(newfail)
tst.fail <- newfail[sample(1:nrow(newfail),1122),]
#
#  Now bind the "other" 128 successes, there were
#    not part of the training set with the newly
#    selected 1122 rows of fails
#
dat.tst <- rbind(succ[-tr.succ,],tst.fail)
#
#  Check results of setting up the test data set
#
loan.count.tst <- table(dat.tst$Acpt_Offer)
loan.count.tst
mean(dat.tst$Acpt_Offer)
mean(dat$Acpt_Offer)
#
#  The reason to choose the 1122 is now revealed.
#    The test data set that looks just like the 
#    original data set in terms of percent of 
#    success and fails
#
#  Remove some not needed stuff
#
rm(succ, fail, tst.fail, newfail, 
   loan.count.tr, loan.count.tst,
   tr.fail, tr.succ)
#





#
#  Logistic Regression
#
#  ****Start running logistic regression here*****
#
#  Run the logistic regression modeling using 
#    the training data set created earlier.
#  To run logistic regression use of the "glm" 
#    command (versus "lm") and the modifier:  
#      family = "binomial"
#
#  Just before running this, let's consider the 
#    variable correlations
#
cor(dat)[1,]
#
#  Run a logistic regression with the three most 
#    highly correlated variables
#  Note the modeling syntax
#
logreg1 <- glm(Acpt_Offer ~ Income + Crdt_Crd_Avg + 
                 CD_Account, data = dat.tr, 
               family = "binomial")
summary(logreg1)
#
#  Q:  Which variables are significant in the first 
#    logistic regression?
#
#  The results of a logistic regression model have  
#    a lot of similarities with regular regression:
#    1. There are estimates of the slopes and intercept, 
#        (bi and bo)
#    2. There are tests to see if the coefficients 
#       are = 0 complete with P-values
#       Note that these tests are Z-tests, but they are 
#       dispositioned in the same way
#    3. There are measures of fit for the overall model 
#       known as deviance.
#       a. Null Deviance is the deviance from the "naive" 
#         model (using only an intercept)
#      b. Residual Deviance is the deviance left over after 
#         fitting the model (like sum of squared errors or
#         residual sum of squares, RSS)
#
#  There are meaningful and significant differences.
#  
#  First the relationship between qualitative y variable, 
#    here assumed to be binary, and the x variables.  
#    Due to the nature of y, we predict the probability
#    that y is equal to 1, that is, P(Y=1).  
#    
#  If we estimate that there is a high probability for 
#    P(Y=1), then we estimate that that customer will 
#    accept the offer.  If we estimate P(Y=1) to be low,
#    then we will classify the customer as likely non-
#    accepter of the offer
#
#  The function we use to model this in logistic 
#    regression is
#    P(Y=1|X = x) = exp(b0 + b1*x1 + b2*x2) /
#                             (1 + exp(b0 + b1*x1 + b2*x2))
#  This is known as the logistic function.
#
#  Bottom Line:  A one unit change in X does not change
#    Y (or the probability of Y) by beta_x 
#  Next week this will be addressed.
#
#  Second, the mechanism to estimate the model is 
#    different --> Maximum Likelihood Estimation
#
#  To pick the best estimates for the slopes bi and the 
#    intercept bo,we do not use the same criteria 
#    function in logistic regression that we used 
#    in linear regression.  
#    
#    In linear regression we used the 
#      RSS = sum(yi - yi-hat)^2 
#        where yi-hat = bo + b1*x1 + b2*x2.  
#    We picked bo and the slopes bi to minimize the RSS.
#
#  In logistic regression we use maximum likelihood 
#    estimation.
#
#  In maximum likelihood estimation, one assumes 
#    the data follows a specific distribution.  
#    Here for a binary "y" variable, the bernoulli 
#    distribution makes sense.  
#  The Bernoulli distribution has an underlying random 
#    variable that results either in Success or Failure 
#    on a single trial.  
#    In this setting, Success means Y = 1 and failure 
#      is Y = 0.  
#    There is a constant probability of success on
#    the trial of with value p.  Use the notation 
#    of p(y|x) above in the place of just p.  This
#    means, what is the probability of y given the 
#    values of x?
#
#  The probability distribution of the Bernoulli 
#    distribution is:
#    f(w) = p^w * (1-p)^(1-w)  where w = 1 or 0
#  So when w = 1, f(1) = p and when w = 0, 
#    f(0) = 1 - p
#
#  So to find the best estimate for p using maximum 
#    likelihood, we would use this  distribution 
#    in the following way:
#    L(p|x1, x2, ..., xn) = p^x1*(1-p)^(1-x1)*.....*
#                                         p^xn*(1-p)^(1-xn)
#  The function L here, that is L(p|x1, x2, ..., xn) is 
#    called the likelihood function.
#
#  Given the values for x1,...,xn, (that is, the data) what 
#    is the value of p that maximizes the likelihood 
#    function, L?
#  The p that does this is called the maximum likelihood 
#    estimator (MLE).
#
#  Now this case is still more complicated than this.  
#    It is more complex because we plug in the equation 
#    p(y|x) = exp(b0 + b1*x1 + b2*x2) /
#                            (1 + exp(b0 + b1*x1 + b2*x2))
#    which we plug in for p in the likelihood equation 
#    above.  Also, it is likely that there are more 
#      than 2 predictors as well.
#  
#  Unlike the bernoulli case, where one can apply 
#    calculus to solve for the p that maximizes 
#    the likelihood, this situation calls for  
#    the use numerical methods to compute the 
#    values of b0 and b1 and so on, that maximize L.  
#    These numerical methods usually boil down to a 
#    gradient search algorithm-which is a tool that 
#    "climbs the hill" or "finds the valley" of the 
#    Likelihood function iteration by iteration.
#
#  All of this happens everytime with compute a logistic 
#    regression model!
#
#    In almost all optimization involving the 
#      likelihood function, L, is transformed.  
#      The normal transform is to take the natural 
#      log of L and multiply by -2.  
#    Because of this, when logistic regression is fit
#      the function -2ln(L) is actually minimized!
#
#    The null deviance is the value of -2*ln(L) at the 
#      beginning, that is, before fitting the model.
#
#    The residual deviance is the value of -2*ln(L) after 
#      fitting the model.  
#   
#
#  The fitted (or predicted) values for the personal loan 
#    variable are in a variable called "fitted.values"
#
logreg1$fitted.values[1:20]
logreg1$fitted.values[120:128]
logreg1$fitted.values[129:139]
#
#  One can also get the fitted values by using the 
#    "predict" command
#
yhat.tr <- predict(logreg1, dat.tr, 
                      type = "response")
#
#  Either way one can see that the fitted values are 
#    probabilities, yes, the estimate P(Y=1|X =x).
#
yhat.tr[1:20]


#  Compare these predicted values for y to 
#   the actual values
#
yhat.tr.plus.act <- cbind(yhat.tr, dat.tr$Acpt_Offer)
yhat.tr.plus.act[1:20,]
#
#  Q:  How did the logistic regression model do
#    on the training data?
#
#  Now build a "confusion matrix" on the training data 
#    using the results our logistic regression to 
#    check overall performance.
#
#  But first classify the observations as 0 or 1
#    based on the probabilities obtained from the
#    "predict"
#  Use the ifelse command with a cutoff value of 0.5 
#   to do this. 
#
yhat.tr.class <- ifelse(yhat.tr > 0.5, 1, 0)
yhat.tr.class[1:20]
#
#  Q:  Why was 0.5 chosen as the cutoff?
#
#  So the predicted probabilities of buying have been 
#    converted to 0/1 predictions.  Now build a 
#    little table to compare these.
#
tab.lr1.tr <- table(Actual = dat.tr$Acpt_Offer, 
                    Predicted = yhat.tr.class)
tab.lr1.tr
#
#  Q:  What does this table tell us?
#
#  Now compute the error using logistic regression 
#    on the training data via the confusion matrix
#
lr1.tr.err <- (tab.lr1.tr[1,2] + 
                    tab.lr1.tr[2,1])/sum(tab.lr1.tr)
lr1.tr.err
#
# QUICK ERROR COMPUTE
# A more expedient way to compute the error is
#
mean(yhat.tr.class != dat.tr$Acpt_Offer)
#
#  Q:  How are the results?
#
#
#  The real question of interest is how did the 
#    logistic regression model estimated here
#    perform on the test data set?
#  
#  Thus make the forecasts on the test data.
#
yhat.tst <- predict(logreg1, dat.tst, 
                     type = "response") 
yhat.tst[1:20]
#
#  Once again, based on the probability estimates, 
#    classify the test data as 0s and 1s.
#    If the probability is above 0.5, classify 
#    the data point as a "1" (accept offer).  
#    If the probability is less than 0.5, 
#      then they are classified as a "0"
#
yhat.tst.class <- ifelse(yhat.tst > 0.5, 1, 0)
yhat.tst.class[1:20]
#
#  Build a confusion matrix of results.
#
tab.lr1.tst <- table(Actual = dat.tst$Acpt_Offer, 
                     Predicted = yhat.tst.class)
tab.lr1.tst
#
#  Q:  How was the logistic regression model 
#    performance on the test data?
#
#  Compute the overall error on the test data
#
mean(yhat.tst.class != dat.tst$Acpt_Offer)
lreg1_err <- mean(yhat.tst.class != dat.tst$Acpt_Offer)
lreg1_err
#
#  Q:  Are both loan takers equally important?
#  
#  Compute the error on the "takers" and the 
#    "rejecters"
#
class1.tst.err <- tab.lr1.tst[2,1]/128
class1.tst.err
class0.tst.err <- tab.lr1.tst[1,2]/1122
class0.tst.err
#
#  Q:  How did the model do on these groups?
#
#  Generally, one would expect any model to 
#    perform better on the training data as 
#    compared to the test data
#





#
#  Aside:  Adjusting the Success Cut-off Value
#  
#  A "cut-off" probability of 0.5 was use before, 
#    what if one tries a different value?
#
#  One can use any set of predicted values for this 
#    exercise-here the logistic regression model from
#    above is used
#
#  Use the test data predictions from the prior 
#    section as the input data
#
yhat.tst[1:10]
#
#  Try a couple values of the cut off.
#  And then evaluate classification performance
#
cutoff <- 0.8
yhat.tst.class <- ifelse(yhat.tst > cutoff, 1, 0)
tab1.tst <- table(Actual = dat.tst$Acpt_Offer, 
                  Predicted = yhat.tst.class)
tab1.tst
overall_err <- mean(dat.tst$Acpt_Offer != yhat.tst.class)
overall_err
class1_err <- tab1.tst[2,1]/128
class1_err
class0_err <- tab1.tst[1,2]/1122
class0_err
#
#  Q:  What happened when cutoff was reduced from 0.5?
#    Increased from 0.5?
#
#  It's a bit of work, but let's capture error rates 
#    for 99 cutoff values 0.01 to 0.99
#
#  First set up some ranges to capture informatin
#
overall_err <- 1:999
class1_err <- 1:999
class0_err <- 1:999
#
#  The run a loop that computes error rates at 
#    every value of the cutoff probability, val
#
for (i in 1:999){
  val <- i/1000
  yhat.tst.class <- ifelse(yhat.tst > val, 1, 0)
  overall_err[i] <- mean(dat.tst$Acpt_Offer 
                         != yhat.tst.class)
  class1_err[i] <- mean(dat.tst$Acpt_Offer[1:128] 
                        != yhat.tst.class[1:128])
  class0_err[i] <- mean(dat.tst$Acpt_Offer[129:1250] 
                        != yhat.tst.class[129:1250])
}
#
plot(1:999,overall_err)
#  Check out Class 0 and Class 1 error rates  
#
class1_err[1:200]
class0_err[1:200]
#
#  Plot these values:  the chart shows the 
#    class 0 and class 1 error rates for different
#    cutoff probabilities
#
xrange <- 1:999/1000
plot(xrange, class0_err, xlab = "Cutoff Value", 
     ylab = "Error Rate", col = "Red", type = "b", 
     pch = 19, cex = 0.75)
points(xrange, class1_err, xlab = "Cutoff Value", 
       col = "Blue", pch = 15, cex = 0.75)
legend(0.30, 0.99, 
       legend = c("Class 0 Err Rate", "Class 1 Err Rate"),
       col = c("red","blue"), lty = 1:2, cex = 0.8)
#
#  Q:  Can you explain this chart?  
#
#  This next section is an example of a lift chart
#    A commonly used visual metric for classifiers.
#
#  A lift chart is a plot of the classifier's 
#    Sensitivity versus the Class 0 Error Rate
#  
#  Definitions:
#    Sensitivity = 1 - Class 1 Error Rate
#    Specificity = 1 - Class 0 Error Rate
#
?plot
#lift chart .. closer the AUC is to 1 , the better the classifier 
sensit = 1 - class1_err
plot(class0_err, sensit, xlab = "Class 0 Error Rate",
     ylab = "Sensitivity = 1 - Class 1 Error Rate", 
     col = "red", xlim = c(0,1), ylim = c(0,1))
abline(0,1)
#
#  Q:  How does one read this?
#
#  Lift Charts are a common way to visually display
#    the quality of a classifier.
#  AUC = area under curve, can be computed and
#    measures predictors' quality
#
#  An estimate of the AUC can be computed here
#
sensit
sum(sensit)
AUC <- sum(sensit)*0.001
AUC
#


#  
#  K-Nearest Neighbor (kNN) classifier
#
#  kNN:  Algorithm
#    1. Pick any observation that is not classified
#       put it in the group with the majority of the 
#       k closest (nearest) neighbors
#
#  kNN is interesting for a number of reasons
#   
#  Q:  Can you provide a pro and a con of this method?
#
#  Need the "class" library
#
#install.packages("class")
library(class)
#
#  The knn command takes in the training data and uses 
#    it to classify the test data
#  One does not "fit" the model on the training data
#    and apply it to the test data-it just classifies
#    the test data straight away.
#
#  The object "out" created as output is the class of 
#    the test data.  In the "out" object, a 
#    "1" indicates that kNN found that a majority
#    of a customer's nearest neighbors accepted the 
#    Bank's offer.  A "0" indicates that a majority 
#    of the customer's nearest neighbors did not 
#    accept the Bank's offer
#
#  The parameter k specifies how many neighbors kNN 
#    considers when classifying each observation.
#
#  Q:  Why does one use an odd value for k in 
#    binary classification?
#
#  It turns out that k = 1 is the most flexible 
#     option.  arger values of k are less flexible, 
#     but provide more stable results.
#
#    The parameter, K, is generally "tuned" to obtain 
#     the best results.
#
#  Run kNN
#
out1 <- knn(dat.tr[,2:13], dat.tst[,2:13], dat.tr[,1], k=1)
out1[1:25]
#
#  Create the confusion matrix with the results on 
#     the test data.  
#
tab.knn1 <- table(Actual = dat.tst$Acpt_Offer, 
                  Predicted = out1)
tab.knn1
knn1.err <- mean(dat.tst$Acpt_Offer != out1)
knn1.err
#
#  Q:  How was the performance?
#
#  Try another value of k, k = 5
#
out5 <- knn(dat.tr[,2:13], dat.tst[,2:13], dat.tr[,1], k=5)
#
#  Now create a second confusion matrix with the 
#    results on predicting the tst data.  
#
tab.knn5 <- table(Actual = dat.tst$Acpt_Offer, 
                  Predicted = out5)
tab.knn5
knn5.err <- mean(dat.tst$Acpt_Offer != out5)
knn5.err
#
#  kNN with k = 5 seems to perform a bit better than k = 1  
#  Try another value of k, k = 13
#
out13 <- knn(dat.tr[,2:13], dat.tst[,2:13], dat.tr[,1], k = 13)
#
#  Now create a third confusion matrix with the results 
#    on predicting the test data. 
#
tab.knn13 <- table(Actual = dat.tst$Acpt_Offer, 
                   Predicted = out13)
tab.knn13
knn13.err <- mean(dat.tst$Acpt_Offer != out13)
knn13.err
#
#  Compare all four models estimated so far:
#    1.  Logistic regression (all in)
#    2.  kNN with k = 1
#    3.  kNN with k = 5
#    5.  kNN with k = 13
#
lreg1_err
knn1.err
knn5.err
knn13.err
#
#  Now set up a loop to run a bunch kNNs
#    with the normalized data
#  knn.err keeps track of the errors
#
knn.err <- 1:50
xrange <- 1:50
for (j in 1:99) {
  if (j %% 2 != 0) {
    xrange[(j+1)/2] <- j
    out <- knn(dat.tr[,2:13], dat.tst[,2:13], dat.tr[,1], j)
    knn.err[(j+1)/2] <- mean(out != dat.tst[,1])
  }
}
#  Check what was obtained
#
xrange
knn.err
#
#  Plot the errors versus k
#
plot(xrange, knn.err, xlab = "Value of K (K odd)",
     ylab = "Error from KNN")
lines(xrange, knn.err)
#
#
#  Normalizing/Scaling Data for KNN
#
#  Actually kNN works in a more appropriate way
#    when the data is normalized (scaled.  
#  Normalizing the data means subtracting the mean from 
#    every observation in a column and dividing by the 
#    standard deviation.
#
#  Q:  Why would normalizing the data make a 
#    difference when looking at "nearest"
#    neighbors?
#
#  Normalize (Scale) the data and check results
#
str(dat)
dat.tr.s <- dat.tr
dat.tst.s <- dat.tst
dat.tr.s[,2:7] <- scale(dat.tr.s[,2:7])
dat.tst.s[,2:7] <- scale(dat.tst.s[,2:7])
mean(dat.tst.s[,4])
sd(dat.tr.s[,7])
# 
#  Now set up the loop to run a bunch kNNs
#    with the normalized data
#    knn.err keeps track of the errors
#
knn.err <- 1:50
xrange <- 1:50
for (j in 1:99) {
  if (j %% 2 != 0) {
    xrange[(j+1)/2] <- j
    out <- knn(dat.tr.s[,2:13], dat.tst.s[,2:13], 
               dat.tr[,1], j)
    knn.err[(j+1)/2] <- mean(out != dat.tst$Acpt_Offer)
  }
}
#
#  Check what was obtained
#
xrange
knn.err
#
#  Plot the errors versus k
#
plot(xrange, knn.err, xlab = "Value of K (K odd)",
     ylab = "Error from KNN")
lines(xrange, knn.err)
#
#  Q:  What do these results show?
#


#
#  Classification Tree Modeling
#  
#  Building classification and regression trees uses
#    package "tree"
#
#install.packages("tree")
library(tree)
#
#  Classification trees are useful from the perspective
#    of interpret-ability.  It is likely easier to build
#    one first and then interpret it
#
#  In classification tree models, one assumes that 
#    the Y variable is qualitative
#  Thus, convert the Y-variable to a "factor"
#
dat.tr[,1] <- as.factor(dat.tr[,1])
dat.tst[,1] <- as.factor(dat.tst[,1])
#
#  Build the first classification tree
#
tree1 <- tree(Acpt_Offer ~ ., data = dat.tr)
summary(tree1)
#
#  Q:  What does the summary say?
#
#  It is likely helpful to understand, what does the 
#    tree look like?
#
#  Create a plot of the tree just built.
#
plot(tree1)
text(tree1, pretty = 0)
#
#  Q:  Can you interpret the plot of the tree?
#   Can you interpret the first split?
#
#  To obtain the meta data on the tree use:
#
tree1
#
#  Take a look at a particular terminal node in the tree
#  These may be referred to as "leaves"
#
Node10 <- dat.tr[dat.tr$Income < 90.5 
                    & dat.tr$Crdt_Crd_Avg > 2.9 
                    & dat.tr$Crdt_Crd_Avg < 3.65,]
Node10
#
#  A criticism of trees is that they overfit to
#    the training data
#
#  Use the tree to make predictions on the training
#    and test data
#
tree.pred.tr <- predict(tree1, dat.tr, type = "class")
table(Actual = dat.tr$Acpt_Offer, Predicted = tree.pred.tr)
mean(dat.tr$Acpt_Offer != tree.pred.tr)
tree.pred.tst <- predict(tree1, dat.tst, type = "class")
table(Actual = dat.tst$Acpt_Offer, Predicted = tree.pred.tst)
mean(dat.tst$Acpt_Offer != tree.pred.tst)
#
#  Q:  How did the tree do?
#
#  As mentioned before trees can over fit easily
#
#  One can "prune" the tree, that is "cut" off 
#    it's leaves to provide a more stable model
#  Actually, this gets at the very essence of the 
#    Bias-Variance tradeoff
#
#  Q:  A simpler tree would have lower or higher
#   bias?  Variance?
#
#  Lets prune!
#
prune1 <- prune.misclass(tree1)
names(prune1)
#
#  Prune 1 is not actually a tree, but a set
#    of trees.
#
#  Plot the results of the prune
#
plot(prune1)
plot(prune1$size, prune1$dev, xlab = "Size of Tree",
     ylab = "Misclassified")
lines(prune1$size, prune1$dev)
#
#  The plot can help to identify the right tree size
#    smaller is, of course, better from the robustness
#    perspective
#
#  If the tree size is specified the result of the
#    command is a tree!
#
#  For example, pick a tree with 6 terminal nodes:
#
prune.tree6 <- prune.misclass(tree1, best = 6)
summary(prune.tree6)
prune.tree6
plot(prune.tree6)
text(prune.tree6, pretty = 0)
#
#  Now that there are two trees, tree1 and prune.tree6
#    compare predictions on the test data
#
tree1.pred <- predict(tree1, dat.tst, type = "class")
table(Actual = dat.tst$Acpt_Offer, Predicted = tree1.pred)
mean(dat.tst$Acpt_Offer != tree1.pred)
pt1.pred <- predict(prune.tree6, dat.tst, type = "class")
table(Actual = dat.tst$Acpt_Offer, Predicted = pt1.pred)
mean(dat.tst$Acpt_Offer != pt1.pred)
#
#  Q:  How did the results look?
#
#
#  Q:  Which classification process worked best?
#





#
#  End Week 7 R Presentations
#


