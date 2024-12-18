#
#  Predictive Analytics (FE288) Week 7: 
#  Introduction to Classification
#  Last updated:  4/13/20
#
#  These notes cover
#  1.  Classification data (y is a qualitative variable)
#  2.  Creating a training and test data sets for classification
#  3.  Logisic regression
#  4.  K-Nearest Neighbor
#  5.  Discriminant Analysis
#  6.  Building Classification Trees


#  Video 07-06
#  Reading in the Universal Bank Data
#  Setting up training and test data sets
#
dat <- read.csv("week7_universal_bank_data.csv")
names(dat)
str(dat)
#
#  Checking to see what percentage and number took the loan offer 
#  (Acpt_Offer variable)
#
mean(dat$Acpt_Offer)
loan.count <- table(dat$Acpt_Offer)
loan.count
loan.perc <- table(dat$Acpt_Offer)/nrow(dat)
loan.perc
#
#  Now we'll set up our data for this week.
#  There are 3 education variables - 
#    One integer variable with 3 levels and 
#    two indicator variables corresponding 
#    to 2 of the 3 levels.  We don't need the
#    first (integer) variable.  We'll also get rid 
#    of the ID as it is not germane to the analysis.
#    Finally, we will put the "Y" variable first in 
#    data set-just a personal pet peeve!
#
names(dat)
dat1 <- dat[,c(15,2:6,8:14)]
names(dat1)
dat1[1:20,]
#
#  Build a training and test data sets
#
#  There 256 of the 2500 who accepted the Bank's offer
#  The training data will be 128 (half) of those 256 
#    and another 128 of those customers who did not 
#    take the loan offer.
#
#  The test data will be the other half (128) who 
#    took the loan and a subset of the remainder 
#    of those who did not take the loan offer.
#
#  First separate the data into two the groups, 
#    the "Successes" (took the loan) and the "Fails"
#    who did not
#
succ <- subset(dat1, dat1$Acpt_Offer == 1)
fail <- subset(dat1, dat1$Acpt_Offer == 0)
nrow(succ)
succ[,1]
nrow(fail)
fail[1:100,1]
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
train.succ <- sample(1:nrow(succ),128)
train.fail <- sample(1:nrow(fail),128)
#
#  To complete the creation of the training and test data 
#    sets use rbind to add the rows in the success 
#    and failure subsets back together.  Create a single data 
#    frame, a training data set of 256 observations, 
#    called, dat.train.  
#  Then, repeat the process to create a test data set 
#    of 1250 observations
#
dat.train <- rbind(succ[train.succ,],fail[train.fail,])
str(dat.train)
loan.count.train <- table(dat.train$Acpt_Offer)
loan.count.train
#
#  Now just to be extra cool, take a sample of 1122 of
#    the failure observations from the 2244 - 128 = 2116 that
#    are left over after building the training data set
#
newfail <- fail[-train.fail,]
test.fail <- newfail[sample(1:nrow(newfail),1122),]
#
#  The test data set that looks just like the original
#    data set in terms of percent of success and fails
#
dat.test <- rbind(succ[-train.succ,],test.fail)
loan.count.test <- table(dat.test$Acpt_Offer)
loan.count.test







#  Video 07-07:  Logistic Regression
#  ****Start running logistic regression here*****
#
#  We run the logistic regression on the training data 
#    set created earlier.
#  Note the use of the "glm" command (versus "lm")
#    and the modifier:  family = "binomial"
#  This signals R to run a logistic regression.
#
logreg1 <- glm(Acpt_Offer ~ Age + Work_Exp + Income + Fam_Size, 
               data = dat.train, family = "binomial")
summary(logreg1)
#
#  The first thing is the relationship between y, 
#    here assumed to be binary or 0/1 and the x variables.  
#    Due to the nature of y, we predict the probability
#    that y is equal to 1, that is, P(Y=1).  
#    
#  If we estimate that there is a high probability for 
#    P(Y=1), then we estimate that that customer will 
#    accept the offer.  If we estimate P(Y=1) to be low,
#    then we will classify the customer as likely non-
#    accepter of the offer
#
#  The function we use to model this in logistic regression is
#    P(Y=1|X = x) = exp(b0 + b1*x1 + b2*x2) /
#                                 (1 + exp(b0 + b1*x1 + b2*x2))
#  This is known as the logistic function
#
#  To pick the best estimates for the slopes bi and the 
#    intercept bo,we do not use the same criteria function 
#    in logistic regression that we used in linear regression.  
#    
#    In linear regression we used the RSS = sum(yi - yi-hat)^2 
#    where yi-hat = bo + b1*x1 + b2*x2.  
#    We picked bo and the slopes bi to minimize the RSS.
#
#  In logistic regression we use maximum likelihood estimation.
#
#  In maximum likelihood estimation, we assume that the data 
#    follows a specific distribution.  Here for a binary y 
#    variable, the bernoulli distribution makes sense.  
#  The Bernoulli distribution has an underlying random variable 
#    that results either in Success or Failure on a single trial.  
#    For us, Success means Y = 1 and failure is Y = 0.  
#    There is a constant probability of success on
#    the trial of p.  For us, we will use the value of 
#     p(y|x) above in the place of p.
#
#  The probability distribution of the Bernoulli distribution is:
#    f(w) = p^w * (1-p)^(1-w)  where w = 1 or 0
#  So when w = 1, f(1) = p and when w = 0, f(0) = 1 - p
#
#  So to find the best estimate for p using maximum likelihood, 
#    we would use this  distribution in the following way:
#    L(p|x1, x2, ..., xn) = p^x1*(1-p)^(1-x1)*.....*
#                                           p^xn*(1-p)^(1-xn)
#  The function L here, that is L(p|x1, x2, ..., xn) is 
#    called the likelihood function.
#
#  Given the values for x1,...,xn, (that is, the data) what 
#    is the value of p that maximizes the likelihood 
#    function, L?
#  The p that does this is called the maximum likelihood 
#    estimator (MLE).
#
#  Now our case is still more complicated than this.  
#    It is more complex because we plug in the equation 
#    p(y|x) = exp(b0 + b1*x1 + b2*x2) /
#                            (1 + exp(b0 + b1*x1 + b2*x2))
#    which we plug in for p in the likelihood equation above.  
#    Also remember we are likely to have more than 2 
#      predictors as well.
#  
#  Unlike the bernoulli case, where we can use calculus to 
#    solve for the p that maximizes the likelihood we have 
#    to use numerical methods tocompute the values of b0 and b1 
#    and so on, that maximize L.  These numerical 
#    methods usually boil down to gradient search-which is a 
#    tool that "climbs the hill" of the function iteration 
#    by iteration.
#
#  All of this happens everytime with compute a logistic 
#    regression model!
#
#  When you look at the results of a logistic regression model, 
#    you'll see a lot of similarities with regular regression:
#    1. There are estimates of the slopes and intercept, 
#        (bi and bo)
#    2. There are tests to see if the coefficients are = 0 
#       complete with P-values
#    Note that these tests are Z-tests, but we evaluate the 
#      result in the same way
#    3. There are measures of fit for the overall model known 
#      as deviance.
#      a. Null Deviance is the deviance from the "naive" model 
#         (using only an intercept)
#      b. Residual Deviance is the deviance left over after 
#         fitting the model (like sum of squared errors or
#         residual sum of squares, RSS)
#    The likelihood function, L, discussed earlier is 
#      transformed by taking thenatural log and multiplying 
#      it by -2.  The null deviance is the value 
#      of -2*ln(L) at the beginning-before fitting the model.  
#      The residualdeviance is the value of -2*ln(L) after 
#      fitting the model.  Just as we try to maximize the 
#      likelihood, L, we try to minimize -2*ln(L).
#   
#  Can you pick out the significant variables in the first 
#    logistic regression?
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
yhat.train <- predict(logreg1, dat.train, type = "response")
#
#  Either way we can see that the fitted values are 
#    probabilities, yes, the estimate P(Y=1|X =x).
#
yhat.train[1:20]
#
#  We can compare these predicted values for y to 
#   the actual values
#
yhat.train.plus.act <- cbind(yhat.train,dat.train$Acpt_Offer)
yhat.train.plus.act[1:20,]
#
#  Not bad!
#
#  We now build a confusion matrix on the training data 
#    using the results our logistic regression.  
#  But first we need to classify the observations as 0 or 1
#    based on the probabilities we obtained in "predict"
#  We use the ifelse command with a cutoff value of 0.5 
#   to do this. 
#
yhat.train.class <- ifelse(yhat.train > 0.5, 1, 0)
yhat.train.class[1:20]
#
#  So our prediction or fitted probabilities have been 
#    converted to 0/1 predictions.  We now build a 
#    little table to compare these.
#
tab.lr1.train <- table(dat.train$Acpt_Offer, yhat.train.class, 
                      dnn = c("Actual","Predicted"))
tab.lr1.train
#
#  We now compute the error using logistic regression 
#    on the training data via the confusion matrix
#
lr1.train.err <- (tab.lr1.train[1,2]+tab.lr1.train[2,1])/sum(tab.lr1.train)
lr1.train.err
#
#  A more expediant way to compute the error
#
mean(yhat.train.class != dat.train$Acpt_Offer)
#
#  How are the results?
#
#  Next question of interest is how did our model perform
#    on the test data set?
#  
#  So we make the forecasts on the test data.
#
yhat.test <- predict(logreg, dat.test, type = "response") 
yhat.test[1:20]
#
#  Once again, based on the probability estimates 
#    we classify the test data to those who are predicted 
#    accept the Bank's offer and those who aren't
#    If the probabibility is abve 0.5, we classify the data point
#    as a "1" (accept offer).  If the probability is less 
#    than 0.5, then they are classified as a "0"
#
yhat.test.class <- ifelse(yhat.test > 0.5, 1, 0)
yhat.test.class[1:20]
tab.lr1.test <- table(dat.test$Acpt_Offer, yhat.test.class, 
                     dnn = c("Actual","Predicted"))
tab.lr1.test
#
#  We now compute the error using logistic regression 
#   built on the training data, but applied to the test
#   data
#
mean(yhat.test.class != dat.test$Acpt_Offer)
class1.test.err <- tab.lr1.test[2,1]/128
class1.test.err
class0.test.err <- tab.lr1.test[1,2]/1122
class0.test.err
#
#  How did we do?
#
#  One would expect the model to perform better on the 
#    training data as compared to the test data
#









#  Video 07-08:  Adjusting the Success Cut-off Value
#  Let's play around with the cutoff value
#  We were using 0.5 before, what if we try a different value?
#
#  One can use any set of predicted values for this exercise
#  We will use the test data predictions from the 
#   logistic regression in the previous video
#
yhat.test[1:10]
#
#  Let's try a couple values of the cut off.
#  And evaluate performance
#
cutoff <- 0.8
yhat.test.class <- ifelse(yhat.test > cutoff, 1, 0)
tab1.test <- table(dat.test$Acpt_Offer, yhat.test.class, 
                    dnn = c("Actual", "Predicted"))
tab1.test
overall_err <- mean(dat.test$Acpt_Offer != yhat.test.class)
overall_err
class1_err <- tab1.test[2,1]/128
class1_err
class0_err <- tab1.test[1,2]/1122
class0_err
#
#  What happened?
#
#  It's a bit of work, but let's capture error rates for 
#    99 cutoff values 0.01 to 0.99
#
overall_err <- 1:99
class1_err <- 1:99
class0_err <- 1:99
for (i in 1:99){
  val <- i/100
  yhat.test.class <- ifelse(yhat.test > val, 1, 0)
  overall_err[i] <- mean(dat.test$Acpt_Offer 
                         != yhat.test.class)
  class1_err[i] <- mean(dat.test$Acpt_Offer[1:128] 
                        != yhat.test.class[1:128])
  class0_err[i] <- mean(dat.test$Acpt_Offer[129:1250] 
                        != yhat.test.class[129:1250])
}
#
#  Class 0 and Class 1 error rates  
#
class1_err[1:10]
class0_err[1:10]
#
#  Let's plot these
#  This chart shows the class 0 and class 1 error rates
#
xrange <- 1:99/100
plot(xrange,class0_err, xlab = "Cutoff Value", 
     ylab = "Error Rate", col = "Red", type = "b")
points(xrange,class1_err, xlab = "Cutoff Value", col = "Blue")
#
#  How would the results shown in the chart be described?
#
#  This is a lift chart
#  A life chart is a put of the classifier's 
#    Sensitivity versus the Class 0 Error Rate
#  
#  Recall the definitions:
#  Sensitivity = 1 - Class 1 Error Rate
#  Specificity = 1 - Class 0 Error Rate
#
sensit = 1 - class1_err
plot(class0_err, sensit, xlab = "Class 0 Error Rate",
     ylab = "Sensitivity = 1 - Class 1 Error Rate", 
     col = "red")
abline(0,1)
#
#  What happens?
#  Lift Charts are a common way to measure the quality of 
#    a classifier
#







#  Video 07-09:  K-Nearest Neighbor Classification
#  This section covers the K-Nearest Neighbor (kNN) classifier
#
#  kNN is interesting for a number of reasons and simple
#  It has pros and cons
#
#install.packages("class")
library(class)
#
#  Set up to run kNN
#
dat.train.x <- dat.train[,2:13]
dat.train.y <- dat.train[,1]
dat.test.x <- dat.test[,2:13]
dat.test.y <- dat.test[,1]
#
#  The knn command takes in the training data and uses 
#    it to classify the test data.  The object "out" created as 
#    output is the class of the test data.
#    "1" indicates that kNN found a customer's nearest neighbors
#       would accept the Bank's offer
#    "0" indicates that kNN thinks that customer would not 
#       accept the Bank's offer
#
#  The parameter k specifies how many neighbors kNN 
#    considers when classifying each observation.  
#  k = 1 is the most flexible option.  
#    Larger values of k are less flexible, but potentially 
#    provide more stable results.  Generally, use odd 
#    values for tie breaking reasons when performing binary
#    classification (2 groups).
#
#    Also, this parameter, K, is "tuned" to obtain the 
#      best results.
#
out1 <- knn(dat.train.x, dat.test.x, dat.train.y, k=1)
out1[1:25]
#
#  Create the confusion matrix with the results on 
#     the test data.  
#
tab.knn1 <- table(dat.test.y, out1)
tab.knn1
knn1.err <- mean(dat.test != out1)
knn1.err
#
#  The kNN shows an error rate of 20.3% on the test data here 
#
#  Try another value of k, k = 5
#
out5 <- knn(dat.train.x, dat.test.x, dat.train.y, k=5)
#
#  Now create a second confusion matrix with the results 
#    on predicting the test data.  
#
tab.knn5 <- table(dat.test.y, out5)
tab.knn5
knn5.err <- mean(dat.test != out5)
knn5.err
#
#  kNN with k = 5 seems to perform a bit better than k = 1  
#  Try another value of k, k = 13
#
out13 <- knn(dat.train.x, dat.test.x, dat.train.y, k=13)
#
#  Now create a third confusion matrix with the results 
#    on predicting the test data. 
#
tab.knn13 <- table(dat.test.y, out13)
tab.knn13
knn13.err <- mean(dat.test != out13)
knn13.err
#
#  Compare all four models estimated so far:
#    1.  Logistic regression (all in)
#    2.  kNN with k = 1
#    3.  kNN with k = 5
#    5.  kNN with k = 13
#
lr.test.err
knn1.err
knn5.err
knn13.err
#
#  Set up a loop to run a bunch kNNs
#
knn.err <- 1:50
xrange <- 1:50
for (j in 1:99) {
  if (j %% 2 != 0) {
    xrange[(j+1)/2] <- j
    out <- knn(dat.train.x, dat.test.x, dat.train.y, j)
    knn.err[(j+1)/2] <- mean(out != dat.test.y)
  }
}
xrange
knn.err
plot(xrange, knn.err, xlab = "Value of K (K odd)",
     ylab = "Error from KNN")
#
#  What do these results show?
#




#  Video 07-10:  Building Discriminant Analyses
#  In this section, Linear Discriminant Analysis (LDA)
#    and Quadratic Discriminant Analysis (QDA) are executed
#
#install.packages("MASS")
library(MASS)
#
#  Linear discriminant analysis uses Bayes Rule to update the 
#    probailities of accepting the Bank's offer  
#    given the X values, customer information and demographics
#
#  Bayes rule takes the priors for y and updates them based 
#    on the likelihood of X given y.
#  The formula is p(y|x) = (p(x|y)*p(y)) 
#                         / (p(x|y)*p(y)+p(x|not y)*p(not y))
#
#  The priors, p(y), are simply estimated by using the 
#    percentage of offer accepters and others
#    in the data set.  Easy to compute based on the Y variable    
#
#  We computed this earlier
#
loan.count
#
#  We set up the training data as 50/50
#  So the priors would be
#
priors <- table(dat.train$Acpt_Offer)/256
priors
#
#  The likelihoods, p(x|y), are assumed to be (multivariate) 
#    normal distributions where the mean is the mean for each 
#    x variable given y = 1 (up) or y = 0 (down).  
#
#  In linear discriminant analysis the variance/covariance 
#    matrix of the multivariate normal distribution
#    is assumed to be the same over all groups (up and down).
#
#  Run LDA using whatever subset of the X variables we'd like, 
#    just like a regression or a logistric regression.  
#  Here the model uses all variables.
#
lda.fit <- lda(Acpt_Offer ~ ., data = dat.train)
lda.fit
#
#  lda.fit gives us the priors and the means of the two groups
#  It also provides the discriminant coefficients.
#  The discriminant function is linear function of the x 
#    variables.
#
#  Another way of interpreting LDA is to say that it finds 
#    a function that divides the data in such a way that 
#    the two groups have means that are separated as far 
#    as possible and such that the standard deviations 
#    within the groups are as small as possible
#
names(lda.fit)
#
#  We can plot the results of LDA.
#
#  The X axis is showing the values of the discriminant function
#  Here it shows the two groups are barely discriminated.
#  This goes along with the results we already found above.
#
par(mar=c(1,1,1,1))  #This command resets the screen for plots
plot(lda.fit)
#
#  We can use our LDA model to make predictions on 
#    the test data set.  From there generate a confusion 
#    matrix and make an error calculation
#
lda.pred <- predict(lda.fit, dat.test)
names(lda.pred)
lda.pred$class
lda.test.class <- lda.pred$class
tab.lda <- table(dat.test$Acpt_Offer, lda.test.class)
tab.lda
err.lda <- mean(dat.test$Acpt_Offer != lda.test.class)
err.lda
#
#  How did the LDA model perform?
#
#  QDA = Quadratic Discriminant Analysis QDA is a generalization 
#    of LDA where given the Y (up or down) the Xs are assumed 
#    to come from multivariate normal distribution (as before), 
#    but the variance/covariance matrix can now be different for 
#    the different groups.  
#
#  This additional assumption provides QDA with a more flexible 
#    (curvalinear) boundary or discriminant function.
#
#  In fact the discriminant function is quadratic in x
#
#  To run QDA use a similar syntax with command: "qda"
#
qda.fit <- qda(Acpt_Offer ~ ., data = dat.train)
qda.fit
qda.pred <- predict(qda.fit, dat.test)
qda.test.class <- qda.pred$class
tab.qda <- table(dat.test$Acpt_Offer, qda.test.class)
tab.qda
err.qda <- mean(dat.test$Acpt_Offer != qda.test.class)
err.qda
#
#  How did QDA perform?
#  




#  Video 07-11:  Classification Tree Modeling
#  This section applies Classification Tree models
#
#install.packages("tree")
library(tree)
#
#  In classification tree models, we assume that the Y
#    variable is qualitative
#  So we convert the Y-variable to a "factor"
#
dat.train[,1] <- as.factor(dat.train[,1])
dat.test[,1] <- as.factor(dat.test[,1])
#
#  Build the first classification tree
#
tree1 <- tree(Acpt_Offer~., data = dat.train)
summary(tree1)
#
#  What does the tree look like?
#
#  Create a plot of the tree we just built.
#
plot(tree1)
text(tree1, pretty = 0)
#
#  Interpret the plot of the tree.
#
#  To obtain the meta data on the tree use:
#
tree1
#
#  Take a look at a particular terminal node in the tree
#  These may be referred to as "leaves"
#
Node10 <- dat.train[dat.train$Income < 90.5 
                    & dat.train$Crdt_Crd_Avg > 2.9 
                    & dat.train$Crdt_Crd_Avg < 3.65,]
Node10
#
#  One can prune the tree-this gets at the very
#    essence of the Bias-Variance tradeoff
#  A criticism of trees is that they overfit
#
prune1 <- prune.misclass(tree1)
names(prune1)
#
#par(mar = c(5.1, 4.1, 4.1, 2.1))
plot(prune1)
plot(prune1$size, prune1$dev, xlab = "Size of Tree",
     ylab = "Deviation")
#
#  The plot can help to identify the right tree size
#    smaller is, of course, better from the robustness
#    perspective
#
prune.tree1 <- prune.misclass(tree1, best = 4)
summary(prune.tree1)
prune.tree1
plot(prune.tree1)
text(prune.tree1, pretty = 0)
#
#  Now that there are two trees, tree1 and prune.tree1
#    lets make some predictions on the test data
#
tree1.pred <- predict(tree1, dat.test, type = "class")
table(dat.test$Acpt_Offer, tree1.pred)
mean(dat.test$Acpt_Offer != tree1.pred)
pt1.pred <- predict(prune.tree1, dat.test, type = "class")
table(dat.test$Acpt_Offer, pt1.pred)
mean(dat.test$Acpt_Offer != pt1.pred)
#
#  How did the results look?
#
#
#  Run cross-validation on our original tree
#
cv.tree <- cv.tree(tree1, FUN = prune.misclass)
cv.tree
plot(cv.tree)
#
#  The results from the cross-validation and the plot show
#  the # of mis-classified level are minimized around a tree 
#  of size 5.
#  
#  This is the pruned tree already built: Prune.tree 1
#
#
#  End Week 7 R Presentations
#


