#
#  Predictive Analytics (FE288) Week 3: 
#  Modeling with Regression
#  Last updated:  3/30/20
#
#  In these notes we cover
#  1.  Setting up indicator variables for factors
#  2.  Interpreting indicator variables
#  3.  Running mulitple regession models
#  4.  Applying adjusted R-squared, Cp and BIC to compare models
#  5.  Model selection by removing variables 
#  6.  Comparing nested models in R (F-Test)
#  7.  Validation and Cross-validation of regression models
#
#  Video 1:  Read the Bank Salary data
#
getwd()
#setwd("C:/Users/Ken Murphy/Desktop/MGMT288/week3")
dat <- read.csv("week3_salaries_new.csv")
names(dat)
str(dat)
#
#  Video 2:  Convert Gender and Analytics Experience 
#    to binary (0/1) variables
#  These variables are also known as Dummy or Indicator vars
#  This operation overwrites the text values that were in these 
#    columns and converts these variables binary variables.
#
dat$Gender <- as.numeric(dat$Gender) - 1
dat$Gender
dat$AnlExp <- as.numeric(dat$AnlExp) - 1
dat$AnlExp
#
#  Also we can change the name of the "Gender" variable 
#    to make it more clear.  Do you know why 1 = Male?
#
names(dat)[6] <- "Male" 
#
#  There are two other qualitative variables in this data set
#  Education level (EducLev) has 5 levels and job grade (JobGrade) 
#   has 6 levels.
#
#  Currently these variables are integers (see str command above)
#
#  However these are not quantitative variables as one cannot 
#    assume that different levels of these variables are 
#    in proportion to each other.  More specifically, one
#    cannot assume that someone at educational level 3 has 
#    three times the effect on the salary that someone who is 
#    at education level 1.  
#  The next set of scripts converts the 5 levels for EducLev 
#    and 6 for JobGrade to a set of indicator variables for 
#    each factor
#  Basic rule-if there are 5 levels for education, we will need 
#    4 indicator variables to represent this.  Same for job grade, 
#    we will need 5 (not 6) indicator variables in this case.
#
#  The process is first to convert Education Level (EducLev) 
#    to a factor.
#  Use model matrix to create the columns and data.frame 
#    to make into a data frame.  
#  The -1 part does something-check it out!
#  
factor(dat$EducLev)
Edulevel <- factor(dat$EducLev)
str(Edulevel)
Edulevel
tempdat1 <- data.frame(model.matrix(~Edulevel))
tempdat1[1:10,]
tempdat1 <- data.frame(model.matrix(~Edulevel-1))
names(tempdat1)
tempdat1[1:10,]
#
#  Now we repeat for Job Grade
#
Jobgrade <- factor(dat$JobGrade)
tempdat2 <- data.frame(model.matrix(~Jobgrade-1))
names(tempdat2)
tempdat2[c(1,15,45,70,150,175,199,208),]
#
#  Create a new data frame with all the data in one frame
#
#  The cbind command binds together data frames into one data frame
#
newdat <- cbind(dat, tempdat1, tempdat2)
names(newdat)
newdat[1:10,]
#
#  Finally create the data frame to use for analysis
#  Run the following steps:
#    1. Get rid of the employee number variable
#    2. Eliminate EducLev and JobGrade b/c they are 
#       replaced with the indicator vars
#       Also remove 1 indicator variable from Edulevels 
#         and Jobgrades variables
#    3. Lastly put the response variable (dependent variable), 
#       Salary, first--Things are just more handy that way.
#
nndat <- newdat[,c(9, 4:8, 10:13, 15:19)]
names(nndat)



#
#  Video 3:  Modeling with Regression ("All-In" Model)
#  Now run a standard linear regression on all the variables 
#    in our data set
#  Our goal is to determine the "best" multiple linear 
#    regression model
#  "Best" is, of course, a relative term
#
reg.all <- lm(Salary ~ ., data = nndat) 
summary(reg.all)
#
#  90.89% of the variation in bank salaries is explained 
#    by the 14 variables in the equation
#
#  Variable interpretations:
#  Each additional year of experience increases Salary by 
#    $515.58 all other factors held constant
#  Males earn $2554.47 more than females all other factors 
#    held constant
#  Employees at eduction level 1 earn $2109.27 less than 
#    employees in ed level 5, all other factors held constant
#
#  Which variables are significant?
#
#  Seek to build an expedient (low variance) and 
#    quality (low bias) model
#  One way to start is with a correlation table to see which 
#    variablesare correlated with Salary
#
#
#  Video 4:  Comparing Regression Models (Adjusted-R2)
#
cor(nndat)[,1]
#
#  Using this process then the modeler could begin moving 
#    "forward" through the models by adding variables 
#    one by one.  Or the modeling process could start with 
#    all the variables in the model and remove those that 
#    are not significant.
#
reg.all <- lm(Salary ~ ., data = nndat)
summary(reg.all)
#
#  There is a high R-squared:  0.91
#  Also there are a number of highly significant variables:
#    Jobgrade variables, YrsExper, AnlExp and others as well
#  Don't forget that Male and Edulevel2 are significant at 
#    5% alpha!
#
#  Should this be the model?  
#  Or, is it over specified (potentially high variance)?
#  
#  There are insignificant variables. 
#  Why not remove these and see what happens?
#
#
reg2 <- lm(Salary ~ . - Age - YrsPrior - Edulevel4, data = nndat)
summary(reg2)
#
#  Still a high R-squared at 0.91 (slightly less than reg1)
#  Also all the variables are now significant except the 
#    Education variables
#
#  So which model to pick: "All-In" or "reg2"?
#  Thoughts:
#    The R-squared is slightly lower-not surprising as 
#      R-squared always drops with the removal of variables
#    But this model (model 2) is a bit simpler which is nice.
#
#  Adjusted R-squared provides a mechanism for comparing models
#    Adj-R-squared for the all in model was 0.9023 and here 
#    it is 0.9024.
#
#    So the Adj-R-squared actually increased when the three 
#      variables above were removed.
#
#    This is one signal that perhaps this model, model 2, 
#      is preferred, that is, lower variance.
#
#  What happens when all the Education variables are removed?
#
reg3 <- lm(Salary ~ . - Age - YrsPrior - Edulevel1 
           - Edulevel2 - Edulevel3 - Edulevel4, data = nndat)
summary(reg3)
#
#  Again regression 3 is a strong model.  All the variables are 
#    now significant and the R-squared is still 0.91 when rounded.  
#  The adjusted-R-squared did not move much either.
#


#  Video 5:  The Nested-Models F-Test
#  Another way to compare models is to perform a statistical 
#    test (F-test) to check if the model with less variables 
#    (Small Model) is just as good as the model with more 
#    variables (Big Model)
#
#  Ho:  Models are same
#  Ha:  Model with more variables is better
#
#  F-calc = ((SSR.Small - SSR.Big)/ n1 ) / ((SRR.Big)/n2)
#    where n1 = Difference in # of variables
#    and n2 = n-number of variables in Big - 1
# 
#  If F-calc > F-crit(alpha, n1, n2), then 
#    SSR.Small >>> SSR.Big and the Big model is better
#    in this case we would Reject Ho
#  Obviously, the rules about small P --> Reject Ho still hold.
#       if F-calc is close to 0 and SSR.small & SSR.big are close to eachother, then we will fail to reject the null hypothesis
#  Do this by hand once and then do it the easy way.
#  To the compute residuals for a regression, e.g., 
#    the "All-In" regression, use
#
reg.all$residuals
#
#  Thus RSS is how far off our prediction to what the Salary should be .. errors
#
RSS.all <- sum((reg.all$residuals)^2)
RSS.all
#
#  And RSS for model 3 is
#
RSS.3 <- sum((reg3$residuals)^2)
RSS.3
#
#  Thus, the calculated F statistic is :
#6 = difference in the 2 models in terms of variables , 14 = # in large model , 1 = intercept ==> 15 parameters in large model
#
F.calc <- ((RSS.3 - RSS.all)/6) / (RSS.all/(208 - 14 - 1))
F.calc
#
#  Now compare F.calc to F.crit
#  Note that the qf function always uses the area to the left
#  So use the "1 - qf" with the alpha = 0.05
#  The F-test here is always a one-tailed test!
#
F.crit <- qf(1 - 0.05, 6, 193) #one tail test to the right .. 208-14-1 = 193
F.crit
# we fail to reject hypothesis => small is just as good as large .. where f.calc < f.crit


#  Note that F-calc is NOT greater than F-crit
#  So, Fail to Rej Ho at the 5% significance level
#  We do not have evidence that the Big model is better than 
#    the Small model
#  Thus we say, the Small model (model 3) is just as good 
#    as the Big model (model all)
#  So, Occam's Razor would say to go with the small model
#  
#  Compute P-value of this test (really all is needed)
#
P.value <- 1 - pf(F.calc, 6, 193)
P.value 
#
#  P.value = 0.435 is NOT less than alpha = 0.05, 
#    Fail to Reject Ho (same conclusion)
#
#  Thankfully, one doesn't have to do all this to run the F-test
#  We can just use the ANOVA command
#
anova(reg3,reg.all)
#
#  Note that F-calc is under the F part of the table
#  The P-value is under the Pr(>F)
#  The RSSs are in the RSS column
#  The ANOVA saves ALOT of time!
#
#  Is model 3 as simple as we can get and still explain Salary 
#    as well as possible?
#  Let's check even a simpler model, one that uses only Jobgrade
#
reg4 <- lm(Salary ~ Jobgrade1 + Jobgrade2 + Jobgrade3 
           + Jobgrade4 + Jobgrade5, data = nndat)
summary(reg4)
#
#  How good is model 4 as compared to model "All"?
#
anova(reg4, reg.all)
#
#  Here P = 0.000001028 < alpha, we reject Ho
#  Model 4 (with JobGrade only) is not as good at explaining Salary as model "All"
#
#  What about model 4 versus model 3?
#
anova(reg4, reg3)
#
#  Here P = 0.00000001417 < alpha, reject Ho
#  Model 4 is not as good at explaing Salary as model "All"
#
#  So far model 3 is the simplest model that explains Salary
#  But model 4 is even simpler and still has R-squared at 0.89
#  That's pretty good!  It's just not as good at explaining Salary
#  as model 3, but its close (in terms of R-squared)!
#
#  Consider the idea of the "naive" model, that is, the model
#  where one uses only an intercept to explain Salary
#
reg0 <- lm(Salary ~ 1, data = nndat)
summary(reg0)
#
#  The naive model (Reg 0) is useful as a mechanism for 
#    comparison
#  For example we can compare the "All-In" model to model 0
#
anova(reg0,reg.all)
#
#  Obviously, (since P = 0.00), model "All-In" is preferred
#
anova(reg0,reg4)
#
#  Model 4 is better at explaining Salary than model 0 as well!  
#
#  So remember there is no one right answer when it comes to 
#    choosing the "best" model.  There are many factors to 
#    consider:  R-squared, P-values of the coefficients, 
#    adjusted R-squared and the F-tests
#  
#  In this case, one could choose either model 3 or model 4.  
#  Model 3 because it makes sense, all variables are signficant 
#    and it accounts for things beyond just the job grade.  
#  Model 4 is also a good candidate because it is so simple
#
#
#


#  Video 6:  Automated Model Selection
#
str(nndat)
#install.packages("leaps") .. look at all subset of predictors & filter out the best as predictors
library(leaps)
regfit.full <- regsubsets(Salary~., nndat)
summary(regfit.full)
summary(regfit.full)$rsq
plot(summary(regfit.full)$rsq)
#
#  The table provided shows which variables were chosen in "best" 
#    models, that is, the highest r-squared with 1 variable, 
#    2 variables, etc.  The command stops after 8 variables
#
#  Since there are 14 predictors variables, one can look at all 
#    highest R-squared models up to 14 variables  
# 
regfit.full1 <- regsubsets(Salary~., nndat, nvmax = 14)
reg.summary <- summary(regfit.full1)
plot(summary(regfit.full1)$rsq)
summary(regfit.full1)$rsq
#
#  We can also look at the nbest highest R-squared models with
#    each fixed number of x variables up to 14 variables  
# 
regfit.full2 <- regsubsets(Salary~., nndat, nvmax = 14, nbest = 5)
summary(regfit.full2)
summary(regfit.full2)$rsq
#
#  The above shows the 5 best (in the sense of R-squared) models 
#    for each number of predictor variables.
#
#  Best subsets provides additional information that can be 
#  process using the summary command
#
reg.summary <- summary(regfit.full1)
names(reg.summary)
#
#  The results of regsubsets can be plotted in various ways
#  For example one can look at r-squared for each of the models
#    chosen.  Not surprising, the r-squared increases as 
#    variables are added.
#
reg.summary$rsq
plot(reg.summary$rsq)
#
#  Plot the diagnostic statistics computed by regsubsets
#  Plot adjusted R-squared, Mallows Cp and the Bayesian 
#    Information Criterion (BIC)
#
par(mfrow=c(2,2))
plot(reg.summary$rsq, xlab = "Number of Variables", ylab ="R-squared")
plot(reg.summary$adjr2, xlab = "Number of Variables", 
     ylab ="Adj R-squared")
which.max(reg.summary$adjr2)
points(11,reg.summary$adjr2[11], col = "red", cex = 2, pch = 20)
plot(reg.summary$cp, xlab = "Number of Variables", ylab ="Mallows Cp")
which.min(reg.summary$cp)
points(10,reg.summary$cp[10], col = "blue", cex = 2, pch = 20)
plot(reg.summary$bic, xlab = "Number of Variables", 
     ylab ="Bayesian Info Crit")
which.min(reg.summary$bic)
points(6,reg.summary$bic[6], col = "green", cex = 2, pch = 20)
#
#  Adjusted R-squared, Mallows Cp and BIC all point to models 
#    with different numbers of variables (11, 10 and 6 
#    respectively)
#
#  Choosing the "best" model is still an art even with all this 
#    machinery!
#  Does one go simple (less flexible) with 6 variables? 
#  -OR- 
#  Does one go higher R-squared (better fit, less bias) 
#    with 10 or 11 variables?
#



#
#  Video 7:  Stepwise Regression
#  To find the highest R-squared model with say, 5 variables, 
#    regsubsets has to look at all subsets of 5 variables out
#    of the 14 predictors.  There are "14 choose 5" or 2002 
#    ways to pick 5 items (variables) out of the set of 14
#    variables 
choose(14,5)
#  Indeed, the computational complexity of choosing the best
#    subset is high, that is, it could take a very long time
#    examine all subsets.  That is to get the highest R-squared 
#    model for a subset of a certain sizem the computation time
#    is exponential in the size of the problem.
#  Here the size of the problem is 14 independent variables, 
#    2^14 = 16,384 subsets.  If there are a lot of X variables
#    this can be a very, very time consuming process.
2^14
#  One way to get around this is to use stepwise regression.
#    Stepwise regression essentially adds variables, one by one,
#      based only on the current model.
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
regfit.fwd <- regsubsets(Salary~.,data=nndat, nvmax = 14,
                         method = "forward")
summary(regfit.fwd)
#
#  Actually, stepwise regression gave us the same answer for our
#    "best" model as did the best subsets routine.  Here is  the 
#    comparison.
#
coef(regfit.fwd, 10)
coef(regfit.full1, 10)
#
#  Stepwise regression can be forward, add variables one by one
#    that INCREASE the r-squared the most at each step.
#
#  Stepwise regression can also be backward.  Take out variables
#    from the model with all variables, that DECREASE the 
#    r-squared the least at each step.
#
regfit.bwd <- regsubsets(Salary~.,data=nndat, nvmax = 14,
                         method = "backward")
summary(regfit.bwd)
#
#  Backward stepwise regression did NOT give the same answer for 
#    the model with 10 variables as did forward stepwise and best
#    subsets.
#
coef(regfit.bwd, 10)
coef(regfit.full1, 10)

