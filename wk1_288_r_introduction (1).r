#
#  Predictive Analytics (FE288) Week 1:  
#  Introduction to R/RStudio and descriptive statstics in R
#  Last updated:  3/18/20 by Murphy
#  
#  In this class we covered the following topics
#  1.  A basic introduction to R and R Studio
#  2.  Basic mathematics in R
#  3.  Vectors and Data Frames in R
#  4.  Reading in Files in R
#  5.  Displaying the contents of data frames
#  6.  Creating graphical and quantitative descriptive statistics
#  7.  Using packages in R
#
#  Any line with a # in front is a comment.
#
#  Video 1.2:  We begin with some basic mathematics in R.
#  Remember to hit CONTROL-ENTER to run commands in the 
#    scripting window.
#
#  We begin with some basic mathematics.  Run each of the lines
#    below individually and see what happens in the console below.
#
1 + 2
2 - 5
2 * 6
2 / 8
2 ^ 7
#
#  Now try hightlighting them all at the same time and hit
#    Control-Enter.  What happens?
#
#
#  Now we assign values to variables using the "gets" syntax
#  Once you do that, you can run algebraic operations on the 
#    variables
#
x <- 2
x
y <- 6
y
x + y
x ^ y
x / y
#
#  Here we use the concatenate command "c" to assign values to 
#    vector variables z and w.
#  Concatenate winds up being a VERY useful command in R.
#
z <- c(1, 2, 3, 4, 5)
z
w <- c(6, 7, 8, 9, 10)
w
#
#  Now we manipulate z and w and show the results to show how 
#    vector operations work.
#
z + w
z * w
z ^ w









#
#  Video 1.3
#  Next we prepare to read data into R.  
#
#  First we find out what folder R is looking at using the getwd() 
#    command.  If we would like, we can change the folder using 
#    the setwd() command
#
#  Make sure to enter the path exactly correctly-otherwise this 
#    will fail to work
#  Good practice is not to use spaces in file or folder names.
#    I'd use an "_" (underscore) instead.
#
#  One way to check to see if the folder you are looking at in R 
#    is correct is to use the list.files command.  It will show 
#    (in R) the files in the current folder.
#
#  Note the path name on your computer to your files won't be 
#    the same as mine!
#
getwd()
setwd("C:/Users/kmurphy/Desktop/MGMT288")
#
#  We use the getwd command again to see if the folder path 
#    is correct
#
getwd()
#
#  Look at the files in the current path
#
list.files(path=".")
#
#  Another way to get RStudio to point at the right folder 
#    on your computer (likely easier!)  is to use the 
#    "Session" command on the above menu in RStudio
#
#  In the menu above choose Session-->Set Working Directory
#    -->Choose Directory
#  Find the folder that was set up for this class-mine is
#    called "MGMT288"
#  Don't worry if the folder appears blank-all you want to do 
#    is "OPEN" the folder which points RStudio at it.  After
#    that RStudio will know to always look there.
#
#  From now on make sure that all your files to be used with 
#    R-Studio are in this folder.
#
#
#  Now we read in the file from the current folder and assign 
#    it to an object called "dat"
#
#  Note that R/RStudio is sensitive to capital letters and to 
#    spaces and other characters.  So the name of the file must
#    be EXACTLY as the name is on your harddrive with the ".csv"
#    extension as well.
#
#  Note the file we are reading in is in .csv format
#
dat <- read.csv("week1_288_Salaries.csv")
#
#  Video 1.4:  To see the variable names of the data we 
#    use the names command  The object "dat" is a data frame.
#
names(dat)
nam1 <- names(dat)
nam1
#
#  The next set of commands displays the data in dat using the 
#    rows, cols format with square ([]) brakets
#  Rows is always the first argument and columns the second.
#  
#  The first one displays rows 1-20 of dat (and all columns)
#  The third one displays rows 1-5 and columns 3-6
#  The last one displays one variable (one column) in dat using 
#    the variable name
#
dat[1:20,]
dat
dat[1:5,3:6]
dat[c(1,3,5),c(2,4,6)]
#
#  One way to refer to the variables is using the "$" notation
#
dat$Gender
dat$YrsPrior
dat$Salary



#
#  Video 1.5:  Graphic Descriptive Statistics
#  To get started on describing data, we begin with 
#    qualitative data:  Gender
#  Most of the time it is useful to display counts 
#    or percentages by category
#  To do this we create a table and call the table 
#    "tab1" from the Gender variable
#
tab1 <- table(dat$Gender)
tab1
#
#  The barplot can be used with the object tab1 to display 
#    the data.  The second command shows some of the barplot 
#    options.  Also, putting a question mark first will 
#    show the syntax of a command
#
barplot(tab1)
barplot(tab1, ylab = "This is kind of cool actually...", 
        xlab = "Count", horiz = T, col = "Red")
?barplot
#
#  Another graphical device for qualitative data is a pie chart
#
pie(tab1, names(tab1))
#
#  One type of graphical display for quantitative data is a 
#    histogram
#  Here we create a histogram with the Years of Experience 
#    variable
#
hist(dat$YrsExper)
#
#  A box plot is another way to show the data graphically
#  
#  Box plots indicate the shape of the data and the spread
#  Three examples are shown with varying levels of complexity
#
boxplot(dat$Salary)
boxplot(Salary~Gender, data = dat, 
        horizontal = T, col = c("Red","Blue"))
boxplot(Salary~Gender*JobGrade, data = dat, 
        horizontal = F, col = c("Red","Blue"))
#
#  Finally to compare two quantitative variables a scatter plot 
#    is useful.  The PLOT command works for this
#
plot(dat$YrsExper,dat$Salary)
plot(dat$YrsExper,dat$Salary, 
     xlab="Years of Experience", ylab = "Salary")





#
#  Video 1.6:  Descriptive Statistics
#  R can also compute basic descriptive statistics
#  In this case we show how to compute mean, standard deviation 
#    and correlation of a couple variables in R
#
mean(dat$Salary)
sd(dat$Salary)
cor(dat$Salary,dat$YrsExper)







#
#  Actually, we can compute and display many statistics at once
#  However, before we can do this we will need to remove the 
#    non-numerical columns of data from the data set
#
names(dat)
dat2 <- dat[,c(2:5,7,9)]
names(dat2)
#
#  Examples of using R to compute many things at one time.
#
plot(dat2)
summary(dat2)
cor(dat2)
var(dat2)
#
#  Video 1.7:  Packages in R
#  R is powerful in part because of the packages available in it
#  Packages are function libraries with a certain application
#  Here we install and use a library called "lattice"
#
#  We will use many different packages in this course!
#
#  The first time you use a package you have to install it.
#  Everytime you use a command not in base R, but in a
#    package you have to run the "library" command.
#
install.packages("lattice")
library(lattice)
#
#  The commands below illustrate some of the functionality 
#    of package lattice
#  Lattice creates beautiful graphics (with some work!)
#  Lattice was written by Deepayan Sarkar
#
#  Can you figure out what all these graphs are....
#  
#
histogram(~Salary, data = dat)
histogram(~Salary | Gender, data = dat)
histogram(~Salary | JobGrade, data = dat)
densityplot(~YrsExper|Gender, data = dat)
xyplot(Salary~YrsExper | JobGrade, data = dat)
bwplot(~JobGrade | Gender, data = dat)
splom(dat)
