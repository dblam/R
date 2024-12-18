dat = read.csv("Loan_default.csv")
str(dat)
# Columns : [1]LoanID , [2]Age , [3]Income , [4]LoanAmount , [5]CreditScore , [6]MonthsEmployed , [7]NumCreditLines , [8]InterestRate , [9]LoanTerm , [10]DITRatio , [11]Education , [12]EmploymentType , [13]MaritalStatus , [14]HasMortgage , [15]HasDependents , [16]LoanPurpose , [17]HasCoSigner , [18]Default

#random seed point
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
length(dat.train$Default)
length(dat.test$Default)

