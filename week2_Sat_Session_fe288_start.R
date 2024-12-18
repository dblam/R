#
#  Predictive Analytics (FE288) Week 2 In-Class R Script: 
#  Modeling with Regression
#  Last updated:  10/11/24
#
#
#  In these notes we cover
#  1.  Review of descriptive statistics (for HW1)
#  2.  Setting up the data set for analysis
#        In particular, setting up indicator variables as 
#        factors
#  3.  Running regression models, interpreting results and 
#        evaluating fit
#  4.  Model selection by adding/removing variables 
#  5.  (Aside)  Comparing nested models (F-Test)
#  6.  Validating regression models
#  7.  Automating model selection via "best subsets"
#  8.  Applying adjusted R-squared, Cp, and BIC to compare models
#  9.  Stepwise Regression as an alternative to "best subsets"
#
#
#
#

#
#  Hour 1:  Getting Started with R/RStudio
#
#  Read the Bank Salary data
#
dat0 <- read.csv("week02_Salaries_for_Saturday.csv")

dat1 <- dat0
#
#  Always nice to see what cool commands there are
#
#  These are useful commands:
#
names(dat1)
str(dat1)
head(dat1)
#




#
#  Examples of plots (examples from Week01/HW1)
#
table(dat1$Gender)
tab <- table(dat1$Gender)
pie(tab, main = "Proportion of Employees by Gender")
barplot(tab, main = "Employee Counts by Gender")
boxplot(dat1$Salary, horizontal = T,
        main = "Distribution of Salary for Employees")
boxplot(dat1$Salary ~ dat1$Gender, horizontal = T,
        main = "Distribution of Salary for Employees",
        xlab = "Salary", ylab = "Gender")
#
#  To show the outliers, try
#
boxplot(dat1$Salary ~ dat1$Gender, horizontal = T)$out
box <- boxplot(dat1$Salary ~ dat1$Gender, horizontal = T) 
#
#  To see the statistics from a boxplot object that we saved
#
box$stats
#
#  Just for the heck of it, let's try a scatter plot
#
plot(dat1$YrsExper, dat1$Salary)
plot(dat1$YrsExper, dat1$Salary, xlab = "Years of Experience",
     ylab = "Salary", main = "Salary on Work Exp")
abline(lm(Salary~YrsExper, data = dat1))
#

#
#  Setting up a Data Set for Analysis
#
#  Addressing Qualitative Variables in R/RStudio
#
#  Convert Gender and Analytics Experience 
#    to binary (0/1) variables
#  These variables are also known as Dummy or Indicator vars
#  This operation overwrites the text values that were in these 
#    columns and converts these variables binary variables.
#
#  I use "str" often to make sure that I did things correctly
#  
#  In this data set Gender is a character data type
#
#  Gender is a qualitative variable with three values 
#    but since one of the three occurs rarely, we convert 
#    the variable to an "indicator" variable
#    Synonyms are "binary" or "dummy" variable
#
#  The conversion occurs in two steps, first make it factor
#    and second make it numeric with 0 and 1 values.
#
#  Note here, we are making an assumption because the third
#    gender has a very small sample size to group it with one
#    of the other two - see line 116
#
#dat1 <- dat0
str(dat1)
dat1$Gender
dat1$Gender <- as.factor(dat1$Gender)
dat1$Gender
dat1$Gender <- as.numeric(dat1$Gender)
dat1$Gender
dat1$Gender <- dat1$Gender - 1
dat1$Gender
dat1$Gender[dat1$Gender==2] <- 0
dat1$Gender
#
#  Also we can change the name of the "Gender" variable 
#    to make it more clear.  Do you know why 1 = Male?
#
names(dat1)
names(dat1)[6] <- "Male" 
names(dat1)
#
#
#  For Analytics Experience it is the same process
#    lets do it all once
# ==> CONVERT: Categorical to numerical for analysis
dat1$AnlExp <- as.numeric(as.factor(dat1$AnlExp)) - 1
dat1$AnlExp
#


#
#  Factor Variables (Qualitative Variables with more than 2 levels)
#
#  There are two other qualitative variables in this data set
#  Education level (EducLev) has 5 levels and job grade (JobGrade) 
#    has 6 levels.  Why are these qualitative variables not
#    quantitative?
#
#  Currently these variables are integers (see str command above)
#
#  However these are not quantitative variables as one cannot 
#    assume that different levels of these variables are 
#    in proportion to each other.  More specifically, one
#    cannot assume that someone at educational level 3 has 
#    three times the effect on the salary that someone who is 
#    at education level 1.
#
#  The next set of scripts converts the 5 levels for EducLev 
#    and 6 for JobGrade to a factor
#
#  Basic rule-if there are 5 levels for education, we will need 
#    4 indicator variables to represent this.  Same for job grade, 
#    we will need 5 (not 6) indicator variables in this case.
#    When a variable is a factor data type, R will take care of 
#    this issue for us.
#
dat2 <- dat1
str(dat2)
dat2$EducLev <- as.factor(dat2$EducLev)
dat2$JobGrade <- as.factor(dat2$JobGrade)
str(dat2)
#
#  Now both Education Level and Job Grade are factors
# 

#
#  Note Employee Number is not a useful modeling variable
#
#  While it might be useful to identify a specific employee
#    for example, as an outlier, we would want to use it 
#    for modeling purposes (nuisance variable)
#
#  Remove the first column from the data set
#
str(dat2)
dat2 <- dat2[,-1]
str(dat2)
#
#  Now we're ready to rock and roll with regression
#





#
#  Hour 2:  Linear Regression and Model Selection
#
#  Modeling with Regression
#
#  Start modeling with regression.  There are many 
#    ways to go, but one could 
#    a.  Build up a model variable by variable OR
#    b.  Start with all variables and remove
#
#  Which variables should we consider as "good" 
#    predictors?
#


#
#  Checking the correlation is one way to do this
#
cor(dat2)
str(dat2)
#
#  Note the error - the "cor" function does not like 
#    the factor variables we created
#
cor(dat0)
str(dat0)
#
#  dat0 was the original data.  Let's convert the two
#    columns of character valued variables again and
#    retry
#
str(dat1)
cor(dat1)
cor(dat1)[,9]
cor(dat1)[9,]
#
#  The most strongly correlated variables with Salary
#    are Years of Experience, Job Grade, and Education
#    Level
#
#  This diagram may or may not be useful
#
plot(dat0)
#
#  Zooming is often useful on a diagram like this
#  What is the relationship bewteen this and the 
#    correlation matrix?
#


#
#  Run a Simple Linear Regression
#
#  Start out with a one-variable model using Years of 
#    Experience
#  
reg1 <- lm(Salary ~ YrsExper, data = dat2)
summary(reg1)
#
#  Draw a picture for effect too!
#  Note - we already did this.....
#
plot(dat2$YrsExper, dat2$Salary)
abline(reg1)
#
#  Note that we save the regression to an output object
#
#  That output object "reg1" has a bunch of stuff inside
#    On the right you will see it as a "list" object 
#    which essentially means it is
#    composed of a bunch of various data structures
#
#  Re-displaying the regression result:
#
summary(reg1)
#
#  What can we obtain from this result:
#
#  a.  The estimated model
#  b.  Interpret the slope coefficients  
#  c.  The models fit (coefficient of determination)
#  d.  Is the slope significant?
#  e.  The standard error of regression


#
#  Aside on Regression Results and (somewhat) Important
#    Quantities
#
#  First the total sum of squares in the Y-values, TSS
#    Recall, TSS does not change no matter what model is 
#    chosen as it depends only on the Y-values, here that
#    is the Salary column.
#
TSS <- sum((dat2$Salary - mean(dat2$Salary))^2) 
TSS
#  
#  We can also save the result of the regression summary 
#    to an object, sum 1  For example, the residuals or
#    errors (distances of Y-values from the line) are 
#    in there
#
sum1 <- summary(reg1)
sum1$residuals
#
#  Just to see that these are indeed the residuals of the 
#    regression, let's compute the residuals by the standard
#    method, subtracting Y from the forecast Y-hat
#
#  Really, this is all just practice in R
#
#  Use the "predict" command to get the Y-hats.  Note you need
#    the data set, dat, and the model, reg1
#
#  Then compare the residuals from above to the residuals
#    computed another way - plugging them into the equation
#    using the "predict" command
#
yhat1 <- predict(reg1, dat2)
plot(dat2$YrsExper, dat2$Salary)
points(da2$YrsExper, yhat1)
#
#  The residuals (errors) are computed using Y minus Yhat
#
resid1 <- dat2$Salary - yhat1
plot(resid1, sum1$residuals)
#
#  From this plot, the two ways we obtained the residuals
#    gave us the same answers
#
#  The cbind command just puts our two sets of residuals 
#    next to each other for ease of viewing.  We compare
#    the first ten residuals
#
cbind(sum1$residuals, resid1)[1:10,]
#
#  They are the same!
#
#  So, we can compute the residual sum of squares
#    by using the residuals inside of sum1
#
RSS1 <- sum(sum1$residuals^2)
RSS1
#
#  The regression and error (residual) sums of squares
#    can also be found using the "anova" command
#
anova(reg1)
#
#  Just for the heck of it, lets also compute the R-squared
#    by hand.  Remember the R-squared is the overall measure
#    of fit of a linear regression model.
#
Rsq1 <- 1 - RSS1/TSS
Rsq1
#
#  Indeed this matches the "Multiple R-squared" on the
#    regression output
#
summary(reg1)
#
#  So to summarize this section, we
#    1. Ran a simple linear (one-variable) regression
#    2. Interpreted the output (important!)
#    2. We computed the residuals and the R-squared
#       even though we don't need to do that in practice
#       It's all inside the summary
#


#
#  Running Multiple Regression Models
#
#  For example, another way to begin the modeling process
#    is to fit the "All-In" Model.  This means use all
#    the variables.
#  Note the "." notation in the "lm" command
#
reg.all <- lm(Salary ~ ., data = dat2)
summary(reg.all)
#
#  Let's interpret the results of multiple regresion 
#  a.  Model
#  b.  Slopes - meaning?
#  c.  Fit? - R-squared?
#  d.  Significant - which variables?
#
#  Save the regression results for later
#
sumall <- summary(reg.all)
#
#  In examining the P-values from this "all-in" model
#    we see that Age, YrsPrior and EducLev are not 
#    significant.  That is, these P-values are not less
#    than any alpha starting from 0.10
#
#  These three variables may not be needed in the model
#    So pull them out and rerun the regression model.  
#    We'll call this third regression, reg2
#  Note the "-" notation in this "lm" command
#
reg2 <- lm(Salary ~ . - Age - YrsPrior - EducLev, 
           data = dat2)
summary(reg2)
sum2 <- summary(reg2)
#
#  Note that models Reg2 and Reg.all have about
#    the same R-squared.  Reg2 is slightly less 
#    than reg.all.  Again, no surprising, less
#    variables in Reg2
#
sumall$r.squared
sum2$r.squared
#
#  Try one more model that is even simpler with only 
#    the Job Grade and the Years of Experience variables.   
#  Note the "+" notation in this "lm" command
#
str(dat2)
reg3 <- lm(Salary ~ JobGrade + YrsExper, data = dat2)
summary(reg3)
sum3 <- summary(reg3)
#
#  Even with just these 5 predictors, the R-squared is 
#    very close to all the prior models-it only dropped 
#    slightly
#
#  Let's summarize all the r-squared values so far
#
sum1$r.squared
sumall$r.squared
sum2$r.squared
sum3$r.squared
#
#  A major goal in regression modeling is to find an
#    effective model that is also expedient (simpler).
#
#  There are three motivations for this
#    1.  Occam's Razor - a scientific principle that states
#      when two competing explanations are similar then the
#      simpler of the two is the better choice
#    2.  Generally explanability - it is easier to explain 
#      to your boss what is happening in a simpler model
#    3.  The bias-variance tradeoff - more on this later
#    
#  We now consider these three models and compare them
#    on effectiveness.  For three of them, their R-squared 
#    values were pretty close
#

#
#  Aside:  The Nested-Models F-Test to compare models
#
#  One way to compare "nested" models is to perform a statistical 
#    test (F-test) to check if the model with less variables 
#    (Small Model) is just as good as the model with more 
#    variables (Big Model)
#
#  Here reg1, reg2, and reg3 are all nested within reg.all
#    Also reg1 and reg3 are nested within reg2
#    So we can apply the nested values F-test
#
#  The hypothesis test (in theory) - it will be must easier 
#    in practice!
#
#  Ho:  Models are same -->  Pick the small model
#  Ha:  Model with more variables is better --> 
#                               Pick the big model
#
#  F-calc = ((RSS.Small - RSS.Big)/ n1 ) / ((SRR.Big)/n2)
#    where n1 = Difference in # of variables in BIG and SMALL
#    and n2 = n - number of variables in Big model - 1
# 
#  If F-calc > F-crit(alpha, n1, n2), then 
#    RSS.Small >>> RSS.Big and the Big model is better
#    in this case we would Reject Ho
#
#  Hopefully it is clear that the rules about small P 
#    --> Reject Ho still hold.
#
#  We can use Anova to compare models using this nested
#    F-test.  Note, that since reg1 had a much smaller
#    value for R-squared, this is not considered
#
anova(reg2, reg.all)
anova(reg3, reg.all)
anova(reg3, reg2)
#
#  The P-value from line 414, comparing models reg2 to 
#    reg.all is 0.435.  Since P is not less than alpha,
#    that is, not small, we fail to reject Ho.  This means 
#    that the "small" model, reg2, is just as good as the 
#    model with all the variables, reg.all
#
#  So between reg2 and reg.all, our choice would be reg2
#
#  For both lines 468 and 469, the P-values are small
#    That is, P < alpha.  In these cases the "big" model
#    does a better job than the "small" model.
#
#  These results point to using reg2 is the best trade-off
#    between a model that does just as well as the model
#    with all variables and doesn't have superfluous 
#    variables
#
summary(reg2)
#



#
#  Hour 3:  Model Validation (James et al, Chapter 5)
#
#  Now let's consider model validation
#
#  Model validation means building the model on one data 
#    set and checking it on another data set
#
#  To accomplish this, divide the data set into two subsets
#    a training data set and a test data set.  The idea is
#    "train" the model using the training data set (build it
#    on the training data) and evaluate its quality on the
#    test data set.
#
#  Somewhat arbitrarily, we will put 128 observations into
#    the training set and the remaining 80 in the test set
#  
#  Let's take a random sample of the observations of size 
#    128 for the training data set
#
#  Set a seed to get repeat-able results
#
set.seed(1357911)
train <- sample(208,128)
train
#
#  Now use the 128 numbers selected from 1 through 208
#    to put those rows from the data set to create
#    the training data 
#
dat2.train <- dat2[train,]
dim(dat2.train)
#
#  Use the other rows, that is, those not selected to 
#    create the test data.  The test data is the 
#    observations we will use to evaluate the model.
#    The test data set should be 80 rows
#
dat2.test <- dat2[-train,]
dim(dat2.test)
#
#  Compute regression model on training data
#  Here the "all-in" model is used, but any model 
#    could be used for this exercise
#
reg.train <- lm(Salary~., data = dat2.train)
summary(reg.train)
#
#  Use the model to make predictions on the test 
#    data set
#
yhat.test <- predict(reg.train, dat2.test)
yhat.test[1:10]
dat2.test$Salary[1:10]
#
#  Aside on making forecasts
#
#  Here we make a prediction of the 46th data point
#
?predict
dat2.test[46,]
predict(reg.train, dat2.test[46,], interval = "prediction",
        level = 0.95)
dat2.test$Salary[46]
#





#
#  Comparing the Training and Test Models
#  (Model Validation)
#
resid.test <- dat2.test$Salary - yhat.test
RSS.test <- sum(resid.test^2)
RMSE.test <- (RSS.test/80)^0.5
RMSE.test
#
#  Compare the training model error (RMSE) to the test
#    error (RMSE)
#
sum.reg.train <- summary(reg.train)
sum.reg.train$sigma
RMSE.train <- sum.reg.train$sigma
RMSE.train
RMSE.test
RMSE.test/RMSE.train
#
#  Test error is 47% bigger than training error.  
#
#  We would expect it to be a bit bigger as one expects 
#    the model to fit a bit better on the data used 
#    to build the model itself.  
#
#  However, if the test error is much larger 
#    than the training error, then we would call
#    this model high variance 
#  
#  Perhaps the model, with all the variables, is 
#    overfit to the training data in this case.
#
#  Question:  Doesn't this process depend on the rows 
#    of the original data set that were selected for
#    training and test sets.  Answer:  Yes!  If we
#    changed the seed and selected new rows, we would
#    obtain a somewhat different regression model and 
#    different values for the RMSEs.
# 
#  What can be done about this?  
#
#    Answer:  Cross Validation
#    (more soon!)
#






#
#  Dimensionality Reduction in Modeling
#    (James et al Chapter 6)
#
#  Modeling is fun, but it is also time consuming.
#    Chapter 6 provides a number of ways to reduce the 
#    complexity of models.  The next several sections
#    discuss this
#  
#
#  Automated Linear Regression Model Selection
#  Use package "leaps" to do this
#
#install.packages("leaps")
library(leaps)
#
#  The basic command in leaps is "regsubsets"
#  This command runs models with subsets of the 
#    predictor or independent variables
#
#  For example try
#
regfit.full <- regsubsets(Salary ~ ., dat2)
summary(regfit.full)
#
#  Each line of the output shows the best model
#    with that number of variables
#
#  Here "best" means, the model with the highest R-squared
#
#  So, for example the best model with 1 predictor is 
#    the model that uses Jobgrade 6, by itself.
#
#  The best model with two variables is the model that uses
#    Jobgrade 6 and Jobgrade 5...and so on
#
#  It is interesting to look at the R-squared plot produced
#    by regsubsets (best subsets regression)
#
#  The plot shows that levels off after a while
#  Somewhere around 6 to 7 to 8 predictors
#
summary(regfit.full)$rsq
plot(summary(regfit.full)$rsq)
#
#  The chart provided shows that regsubsets command stops 
#    after 8 variables
#
summary(regfit.full)$which
summary(regfit.full)$which[8,]
#
#
#  However there are 16 predictor variables, so run the 
#    "regsubsets" command again going up to 16 variables  
# 
str(dat2)
regfit.full1 <- regsubsets(Salary~., dat2, nvmax = 16)
reg.summary1 <- summary(regfit.full1)
#
#  Let's plot the R-squareds again
#
plot(summary(regfit.full1)$rsq)
reg.summary1$rsq
#
#  Here it's clear that they start leveling off around 
#    6, 7 or 8 variables
#



#
#  Comparing Regression Models using Adjusted R-squared
#    and its variants
#
#  There are various metrics to compare models
#    Adjusted R-squared, Mallow's Cp, AIC and BIC are
#    examples of these model metrics
#
#  Essentially, these metrics have a penalty term
#    which begins to penalize when adding a variable
#    doesn't improve the RSS by very much.  Recall it is
#    the goal to minimize the RSS, so when we say improve,
#    we mean make it smaller.  Every variable added likely 
#    reduces RSS by a little, and hence increases R-squared,
#    but is the additional complexity worth it?  This is 
#    place where these "penalty" metrics come in
#
#  Let's look at some plots of Adj-Rsquared, Cp and BIC
#
summary(regfit.full1)$adjr2
plot(summary(regfit.full1)$adjr2)
summary(regfit.full1)$cp
plot(summary(regfit.full1)$cp)
summary(regfit.full1)$bic
plot(summary(regfit.full1)$bic)
#
#  While the goal is to make adjusted R-squared as high
#    as possible, both BIC and Cp show be as low as possible
#
#  The results of regsubsets can be plotted in various ways
#    Here's a slight more fancy way of ploting all these 
#    results together
#
par(mfrow=c(2,2))
#
plot(reg.summary$rsq, xlab = "Number of Variables", 
     ylab ="R-squared")
#
plot(reg.summary$adjr2, xlab = "Number of Variables", 
     ylab ="Adj R-squared")
which.max(reg.summary$adjr2)
madjr2 <- which.max(reg.summary$adjr2)
points(madjr2,reg.summary$adjr2[madjr2], col = "red", 
       cex = 2, pch = 20)
#
plot(reg.summary$cp, xlab = "Number of Variables", 
     ylab ="Mallows Cp")
which.min(reg.summary$cp)
mincp <- which.min(reg.summary$cp)
points(mincp,reg.summary$cp[mincp], col = "blue", 
       cex = 2, pch = 20)
#
plot(reg.summary$bic, xlab = "Number of Variables", 
     ylab ="Bayesian Info Crit")
which.min(reg.summary$bic)
minbic <- which.min(reg.summary$bic)
points(minbic,reg.summary$bic[minbic], col = "green", 
       cex = 2, pch = 20)
#
#  Adjusted R-squared, Mallows Cp and BIC all point to models 
#    with different numbers of variables
#
#  Take Away:
#  Choosing the "best" model is still an art even with 
#    all this machinery!
#  Does one go simple (less flexible) with 6 variables? 
#  -OR- 
#  Does one go higher R-squared (better fit, less bias) 
#    with 8, 9 or 10 variables?
#
#  To fix the graphics to 1 window instead of the 4, use
#
par(mfrow=c(1,))


#
#  Aside:  
#    If it is useful, one can find the "nbest" highest 
#    R-squared models with each fixed number of x variables 
#    up to 16 variables  
#
#  This command finds the 3 best models with each number
#    of variables
# 
regfit.full2 <- regsubsets(Salary~., dat2, 
                           nvmax = 14, nbest = 3)
summary(regfit.full2)
#
#  The above shows the 3 best (in the sense of R-squared) models 
#    for each number of predictor variables.
#




#
#  Stepwise Regression
#
#  To find the highest R-squared model with say, 5 variables, 
#    regsubsets has to look at all subsets of 5 variables out
#    of the 16 predictors.  There are "16 choose 5" or 4368 
#    ways to pick 5 items (variables) out of the set of 16
#    variables 
#
choose(16,5)
#
#  Indeed, the computational complexity of choosing the best
#    subset is high, that is, it could take a very long time
#    examine all subsets.  That is to get the highest R-squared 
#    model for a subset of a certain size the computation 
#    time is exponential in the size of the problem.
#
#  Here the size of the problem is 16 independent variables, 
#    2^16 = 65536 subsets.  If there are a lot of X variables
#    this can be a very, very time consuming process.
#
2^16
#
#  Suppose we had 30 variables, then the number of models 
#    regsubsets would have to choose would be
#
2^30
#
#  You can see the exponential growth
#
#  One way to get around this is to use stepwise regression.
#    Stepwise regression essentially adds variables, 
#      one by one, based only on the current model.
#    Stepwise does not try to find the best subset, it simply 
#      says: we have a "good" model of size V variables, what 
#      is the best variable I can add now to make a model with
#      V+1 variables?
#
#  In this way, stepwise regression has a lower computational 
#    complexity than best subsets.  We say the computation time
#    of stepwiseregression is polynomial (rather than exponential) 
#    in this size of the problem (in the number of predictors)
#  Here is stepwise regression using regsubsets command.
#
regfit.fwd <- regsubsets(Salary ~ ., data = dat2, nvmax = 16,
                         method = "forward")
summary(regfit.fwd)
#
#  Actually, stepwise regression may not give us the same 
#    answers for "best" model as did the "exhaustive" 
#    best subsets routine.  So in that sense, it is a 
#    heuristic.  
#    
#  Using this data the two procedures appear to agree
#    For example, consider the eight variable "best"
#    model:
#
coef(regfit.fwd, 8)
coef(regfit.full1, 8)
#
#  Stepwise regression can be forward, add variables 
#    one by one that INCREASE the r-squared the most 
#    at each step.
#
#  Stepwise regression can also be backward.  Take out 
#    variables from the model with all variables, that 
#    DECREASE the r-squared the least at each step.
#
regfit.bwd <- regsubsets(Salary ~ ., data = dat2, nvmax = 16,
                         method = "backward")
summary(regfit.bwd)
#
#  Backward stepwise regression did give the same 
#    answer for the model with 8 variables as did forward 
#    stepwise and (exhaustive) best subsets.
#
coef(regfit.bwd, 8)
coef(regfit.fwd, 8)
coef(regfit.full1, 8)
#





#
#  End Saturday Week 2 
#     Whew!
#





