#
#  Predictive Analytics (FE288) Week 2 Part 2
#  Multiple linear regression in R
#  Last updated:  3/18/20
#  
#  In this class we cover the following topics:
#  1. Running multiple regressions and interpreting results
#  2. Basic ideas of model selection, the RSS and R-squared
#  3. Limitations of R-squared
#  4. Making predictions (of y-hat) using regression
#  5. The Bias Variance Tradeoff and an example of this
#
#  Video 2.6:  Read in and describe the Grad Rate data    
#  We begin by reading in and checking the data
#
#getwd()
#setwd("C:/Users/kmurphy/Desktop/288")
dat <- read.csv("wk2_288_college_fixed.csv")
names(dat)
dim(dat)
dat[1:10,]
#
#  The variable of interest is graduation rate (Grad Rate)
#    This is the Y variable!
#  Before fitting regressions, we look at the correlations of 
#    the predictor variables with the reponse (graduation rate).
#  See the data dictionary in the Excel file for the definitions
#    of the variables.
#  Before we can do this we have to take care of the columns that 
#    are not quantitative
#
dat1 <- dat[,3:19]
names(dat1)
cor1 <- cor(dat1)[,17]
cor1
#
#  There are several variables stronger correlations:
#  Outstate, TopXXperc variables, Perc.Alumni and some others
#
#  Video 2.7:  Building models using Multiple Regression  
#  Start with predicting Grad Rate as a function of Outstate
#
reg1 <- lm(Grad.Rate~Outstate, data = dat1)
summary(reg1)
#
#  The estimated model is
#    Grad Rate (hat) = 40.0 + 0.00244 * Outstate Tuition
#  32.64% of the variation in Grad Rate is explained by the 
#    out of state tuition
#  A $1000 increase in out of state tuition increases 
#    graduation rate by 2.44%
#  This could make sense as high tuition is a defacto measure 
#    of quality
#  Outstate is a significant predictor of Grad Rate 
#    (P < 0.00, that is very small)
#
#  Now, we run a multiple regression model with two variables 
#    (note the syntax), both out of state tuition and percentage 
#    of admits in the top 10% of their high school class (Top10perc)
#
reg2 <- lm(Grad.Rate~Outstate+Top10perc, data = dat1)
summary(reg2)
#
#  The estimated model is
#    Grad Rate (hat) = 39.6 + 0.00189 * Outstate + 0.247 * Top10perc
#  37.05% of the variation in Grad Rate is explained by the out of 
#    state tuition and by the percentage of admits in the top 10% 
#    of their high school class
#  A $1000 increase in out of state tuition increases graduation 
#    rate by 1.89% all other factors held constant
#  A 1% increase in the percentage of student admitted in the top 
#    10% of their high school class increases the graduation rate 
#    by 0.247% all other factors held constant
#  The positive slopes of both predictor variables make sense as 
#    high tuition is a defacto measure of quality and more good 
#    high schools leads to a a greater degree of success in 
#    college
#  Outstate is a significant predictor of Grad Rate (P is 
#    appromixately 0.00)
#  Top10perc is also a significant predictor of Grad Rate 
#    (P is approx 0.00)
#
#  Now we run a multiple linear regression model predicting 
#    graduation rate as a function of all predictor variables 
#    in the data set
#
reg3 <- lm(Grad.Rate~., data = dat1)
summary(reg3)
#
#  The "all variable" regression shows r-squared of 0.4587
#
#  As we add variables to a regression model the residual 
#    sum of squares, RSS will always either stay the same 
#    or decrease.  That is because each variable
#    we add makes the model more flexible (and complex).  
#    The result of this increasing model flexibility is better 
#    fit.  Better fit means less error or or smaller residuals 
#    and hence a smaller residual sum of squares, RSS.
#  
#  Since the RSS generally decreases as we add variables, the 
#    R-squared = 1 - RSS/TSS will increase 
#    (or, in the extreme, stay the same).
#    By the way, the TSS is the total sum of squares in Y
#
#  This is good and bad.  Good because for a given set of variables, 
#    the R-squared is biggest when all variables are in the model.  
#    Bad because the increases in R-squared can be small as variables 
#    are added.  This raises the question when is an R-squared 
#    increase big enough to warrant adding the variable?
#
#  Video 2.8:  Introducing the Bias/Variance Tradeoff
#
#
TSS <- sum((dat1$Grad.Rate-mean(dat1$Grad.Rate))^2)
TSS
anova(reg1)
anova(reg2)
anova(reg3)
#
#  In the first regression the RSS was 154245
#  In the second regression the RSS was 144138
#  In regression 3, the RSS was 123940
#
#  Just as the R-squared increases when variables are added,
#    the RSS decreases when variables are added.
#
#  There is a way to rigorously test whether these are 
#    statistically significant decreases in the RSS.  There are 
#    statistical tests to evaluate the benefits of adding 
#    variables  and increasing the predictive ability of 
#    the model. These are covered in the upcoming classes.
#
#  For now we say that picking a set of variables that minimize 
#    the RSS and hence maximizes R-squared is the goal with an 
#    eye on the significance of the variables as well as the 
#    amount of flexbility and complexity of the model.  
#
#  Two comments/questions:
#    1.  In regression 3 some variables are significant and 
#      some are not-Can you tell which are which?
#    2.  Regression 3 is much more flexbile (and complex) model 
#      than models 1 and 2.  Is this additional complexity worth 
#      it for the increase in R-squared that was obtained?
#  
#  This brings us to the bias-variance tradeoff.
#    The more flexible, the lower the bias.
#    However, what about variance.
#  Let's see how we can check this...
#
#  To check the tradeoff we have to fit the model on one data set 
#    and check the model on another one.  To do this, we will split 
#    our 777 observations.  We will do this by randomly 
#    selecting 100 of the 777 rows and creating a new data set, dat2.  
#  We will run the three regressions on dat2.  Then we will see what 
#    the RSS is on using these three models on the other 677 
#    observations (dat3).  How well do the models perform on data 
#    that was not used to build the model?
#
#  This next set of code lines takes a random sample of 100 rows from 
#    the 777.
#  It then creates two new data sets, one with the 100 rows and 
#    the other with the rest-677 rows. 
set.seed(123456)
nrows.train <- 100
train <- sample(1:nrow(dat1),nrows.train)
train[1:10]
dat2 <- dat1[train,]
dat3 <- dat1[-train,]
#
#  Now we run the three regressions from before with the 400 
#    observations
#
reg21 <- lm(Grad.Rate~Outstate, data = dat2)
summary(reg21)
reg22 <- lm(Grad.Rate~Outstate+Top10perc, data = dat2)
summary(reg22)
reg23 <- lm(Grad.Rate~., data = dat2)
summary(reg23)
#
#  Now we use the predict command to predict y-hat, the 
#    estiamted value of Graduation Rate using the three 
#    models.  The predictions are 377 observations each.
#
new.y1hat <- predict(reg21, dat3)
new.y2hat <- predict(reg22, dat3)
new.y3hat <- predict(reg23, dat3)
#
#  Now we compute the RSS in the three cases using 
#    the predictions and the actuals in dat3
#
RSS21test <- sum((dat3$Grad.Rate-new.y1hat)^2)
RSS22test <- sum((dat3$Grad.Rate-new.y2hat)^2)
RSS23test <- sum((dat3$Grad.Rate-new.y3hat)^2)
#
#  Next we compute the RSS for the three training regressions 
#    (100 observations) 
#  Note the "redisdual" variable inside the reg object stores 
#     the residuals for us.
#
RSS21train <- sum(reg21$residuals^2)
RSS22train <- sum(reg22$residuals^2)
RSS23train <- sum(reg23$residuals^2)
#
#  Now we convert RSS to RSE (root mean squared error) or 
#    standard error of regression
#  By doing this we can more fairly compare the performance of 
#    training and test data sets as we wish to compare 
#    apples to apples.  
#
#  The RSS values for training and test are based on a differing 
#    number of observations 100 and 677.
#
RSE21train <- sqrt(RSS21train/(nrow(dat2)-1-1))
RSE22train <- sqrt(RSS22train/(nrow(dat2)-2-1))
RSE23train <- sqrt(RSS23train/(nrow(dat2)-16-1))
RSE21test <- sqrt(RSS21test/nrow(dat3))
RSE22test <- sqrt(RSS22test/nrow(dat3))
RSE23test <- sqrt(RSS23test/nrow(dat3))
#
#  Now we put the training and test RSE values in a table 
#   (to make it pretty)
#
tab <- matrix(c(RSE21train, RSE22train, RSE23train, 
                RSE21test, RSE22test, RSE23test), ncol=2, byrow=FALSE)
colnames(tab) <- c("Training", "Test")
rownames(tab) <- c("1 variable model", "2 variable model", 
                   "16 variable model")
tab
#  
#  The RSEs in table Tab are shown for the 6 regressions, 
#    3 training models and 3 test models
#  Essentially the RSEs will generally decline in the left column 
#    as more variables are added, the RSS declines and so will the 
#    RSE (at least most of the time)
#  However the test RSEs (right column), may be better or worse 
#    than the training RSEs.  The 16 variable model would 
#    generally show a worse RSE for the test data set than the 
#    training data set. 
#
#  Run the code starting from the Sample command to see how 
#    the test RSEs change for different samples - but, 
#    change the seed.
#
#  Ultimately, the flexible and complex model with 16 variables 
#    begins to show issues of increasing variance when fitted 
#    on the test data set
