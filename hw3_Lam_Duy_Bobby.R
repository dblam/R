#1.	Read the hourly bike sharing data into R/RStudio.  Use the script provided in the Appendix below to preprocess the data for the assignment.  After doing this answer the following questions:
# a.	What is the size of the data set?  That is how many rows and how many columns? [rows , columns] ==> [17379 , 17] 
# b.	What is the response (dependent or Y) variable in the data? cnt = # of rentals per hour
# c.	Give an example of one quantitative variable  in the data set?  What is the variable and what is its scale? temp is an example of a quantitative variable , its scale (given : min = 0.20 and max = 1.00) is Normalized temperature in Celsius. The values are divided to 41 (max).
# d.	Give an example of one qualitative variable in the data set?   What is the variable, what values does it take on, and what do these values represent?
#    workingday : if day is neither weekend nor holiday is 1, otherwise is 0.
# Hint:  To read in the data use:

dat <- read.csv("hw3_288_bike_hour.csv")
  