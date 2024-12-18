#
#  Predictive Analytics (FE288) Week 2 Part 1: 
#  Review of simple linear regression in R
#  Last updated:  3/18/20
#
#  In this part of the class we cover the following topics:
#  1. A review of simple linear (one variable regression)
#  2. Drawing a scatter plot to identify the X-Y relationship 
#  3. Fitting the regression model
#  4. Interpreting the slope (important) and intercept (also 
#       sometimes important)
#  5. Obtaining and interpreting the coefficient of determination, 
#       r-squared
#  6. Executing T-tests for significance of the slope
#  7. Obtaining the RSS and TSS from the regression output 
#  8. Making forecasts using the regression model
#
#  Video 2.1:  Setting up the data folder and
#    read in the Sunflower Apparel data set
#
#getwd()
#setwd("C:/users/kmurphy/Desktop/288/Week2")
#
#  Read in the data after setting up the folder.
#  Check the names of the variables
#  Display the data
#
dat <- read.csv("week2_sunflowers_apparel.csv")
getnames(dat)
dat
#
#  Annual Sales = Annual Sales for a store in million of $
#  Square Feet = Size of the store in 1000s of square feet.
#
#
#  Run a scatter plot of the variables  
#
plot(dat$Square.Feet, dat$Annual.Sales,  
     xlab = "Square Feet in 1000s (X)", 
     ylab = "Annual Sales in $1,000,000s (Y)")
#
#
#  Video 2.2:  Running a Regression
#  Running a first regression using the size of store 
#    (in square Feet) to preduct the annual store sales
#  Summary is the command used to show the results
#
reg <- lm(Annual.Sales ~ Square.Feet, data = dat)
summary(reg)
#
#  Based on the result of the Summary command, the 
#    estimated model is:
#    Estimated Annual Sales = 0.9645 + 1.6699 * Square Feet
#
#  Interpretations of the Slope and Intercept:
#  (Slope)  For each additional 1000 square feet of store 
#    space the annual sales goes up by $1.67M annually.
#
#  (Intercept)  If the square feet of the store is 0, 
#    the sales would be $954,500 annually.  This makes sense 
#    in the context of this problem.
#
sumreg <- summary(reg)
sumreg$r.squared
#
#  The overall model fit is relatively strong
#  The R-squared (multiple r-squared) is 0.9042
#  
#  Interpretation of R-squared:
#  90.42% of the variation in Annual Sales is explained by 
#    the size of the store variable (Square Feet)
#
#  To see the fit of the line on the plot we can use the 
#    abline command
#  Note that the points cluster close to the line when 
#    the R-squared is high.
#
abline(lm(dat$Annual.Sales~dat$Square.Feet))
#
#  Video 2.3:  Tests for Significance of Slope in R
#  Is there a significant relationship between size of the 
#    store and the annual sales of that store?  That is, 
#    is the slope signficantly different from 0?
#
#  To test the significance of a slope coefficient, we have
#    Ho:  Beta-1 = 0  (means the X variable does not affect Y)
#    Ha:  Beta-1 not equal 0  (means changes in X affect Y)
#  This test is a Student's T-test:  
#  T-Calc = (b0 - 0)/se_b0 = (1.6699 - 0)/0.1569 = 10.641 
#    with n - p - 1 = 14 - 2 = 12 degrees of freedom
#  
#  Video 2.4:  Calculating Sums of Squares  
#
sumreg$coefficients[2,1]/sumreg$coefficients[2,2]
#
#  At the alpha = 5% level and 12 degrees of freedom, 
#    the critical value is 2.18.  We get the critical 
#    value from R by:
#
qt(0.05/2, 12, lower.tail = FALSE)
#  
#  Since T-Calc > T-alpha/2 (10.641 > 2.29), we reject Ho at the 
#    5% level
#  We have a significant relationship between Size of the Store 
#    and Annual Sales at alpha=0.05.
#
#  Another way to draw the conclusion to the test is to use 
#    P-values.
#  The P-Value of this hypothesis test is 1.823e-07.
#
#  Since P = 0.0000001823 < alpha = 0.05 (or 0.01 or 0.10 
#    or even 0.001) we would reject Ho.
#  Rule:  Small P, smaller than alpha that is, means rejecting 
#    the null hypothesis.
#
#  This entire t-test is available on the regression 
#    summary report.  You should identify where it is.
#
crit_val <- qt(0.975, df = 12)
crit_val
#
#  Here is how to get the P-value in R 
#    (of course the P-value is on the R output):
#
arealeft <- pt(10.641124, df = 12)
arealeft
p_val <- 2*(1-arealeft)
p_val
#
#  R-squared is computed by this formula 
#  R-squared = 1 - RSS/TSS
#
#  To get RSS (Residual Sum of Squares or Sum of 
#    Squared Errors) we can use
#  the anova (analysis of variance) command on the 
#    earlier regression
#
anova(reg)
?anova
#
#  The RSS is 11.207
#  TSS or SST is the total sum of squares of the response (y-variable)
#  We can compute this value by the following formula
#  
RSS <- 11.207 #Sum of Squared Errors or SSE
RSS_other <- sum(sumreg$residuals^2)
RSS_other
TSS <- sum((dat$Annual.Sales - mean(dat$Annual.Sales))^2)
TSS
#  
#  The value of TSS is 116.954.  So we can compute R-squared
#  
Rsqd <- 1 - RSS/TSS
Rsqd
#
#  R-squared is "multiple R squared" on the regression report
#
#  Video 2.5:  Making Predictions with Regression
#
names(dat)
?predict.lm
#
#  Set up a new data point-suppose it is a store with 
#    4000 square feet
#
newdat <- data.frame(Square.Feet = 4)
#
#  Use the regression model to predict annual sales for this
#    4000 square foot store
#
#    Estimated Annual Sales = 0.9645 + 1.6699 * 4
#
0.9645 + 1.6699 * 4
predict(reg, newdat)
#
#  Here we show prediction intervals at the 95% and 99% level.
#
predict(reg, newdat, interval = "prediction", level = 0.95)
predict(reg, newdat, interval = "prediction", level = 0.99)
