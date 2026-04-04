# ============================================================
#  BCA 4th Semester | R Programming – Bug Fix Assignment
#  Student  : Astha Rana 
#  Status   : DEBARRED
#  Tasks    : Fix 6 Bugs (lines marked with BUG comment)
#  Deadline : 10 April 2026
#  NOTE     : Each fixed bug must have a comment explaining
#             WHAT was wrong and WHY you fixed it that way.
# ============================================================

# BUG 1 [Unit 1 - Variable Assignment]
# Code should print the square of 7
x <- 7
square = X * X         
cat("Square:", square)
# YOUR FIX below:
x <- 7
square<-x*x
cat("Square:",sqaure)
#The variable name is inconsistent.'x' was defined , but 'X' uppercase is used here.As R is case sensitive, so both 'x' and 'X' is treated different.

# BUG 2 [Unit 1 - Vector Indexing]
# Should print the 3rd element of the vector
fruits <- c("Apple", "Banana", "Cherry", "Date")
cat("Third fruit:", fruits[4])  
# YOUR FIX below:
fruits <- c("Apple", "Banana", "Cherry", "Date")
cat("Third fruit:", fruits[3])
#The code was printing the 4th element using fruits[4],but we need the 3rd fruit. R uses one-based indexing , it means the indexing starts from one not by 0.

# BUG 3 [Unit 2 - Data Frame Column Access]
# Should print the Marks column
#Add Grade Column and Percentage column 
students <- data.frame(Name=c("A","B","C"), Marks=c(78,85,90))
print(students$marks)   
# YOUR FIX below:
students <- data.frame(Name=c("A","B","C"), Marks=c(78,85,90)) # R is a case sensitive language , data frame defines the column as Marks(M) but print statement attempt to access marks(m).
students$Percentage <- (students$Marks / 100) * 100  #Assuming total marks out of 100
students$Grade <- c(students$Marks>="A+" , "A" ,"B" )
print(students)

# BUG 4 [Unit 3 - for Loop Range]
# Loop should print numbers 1 to 55 only

for (i in 1:10) {       
  cat(i, "")
}
# YOUR FIX below:
for (i in 1:55) {
  cat(i, " ")
  }
# The operator : defines the start and end value.By using 1:10 the code was set to run 10 times only,printing from 1 to 10.

# BUG 5 
# Function should return the sum of two numbers

Create a vector of 15 random temperatures between 20-45°C. Find mean, median, and count how many days exceeded 38°C.
# YOUR FIX below:
# No function was defined to return sum of two numbers
sum_two_numbers <- function(a, b) {
  return(a + b) # returns the sum
  }
# Create a vector of 15 random temperatures between 20-45°C
temps <- sample(20:45, 15, replace = TRUE)
# Print temperature
cat("Temperatures:", Temps, "\n")

# Find mean
mean_temp <- mean(temps)
cat("Mean:", mean_temp,"\n")

# Find median
median_temp <- median(temps)
cat("Median:", median_temp,"\n")

#ccount how many days exceeded 38°C.
count_above_38 <- sum(temps > 38)
cat("Days above 38°C:",
    count_above_38, "\n")


# BUG 6 [Unit 3 - if/else Condition]
# Should print "Pass" when marks >= 40
marks <- 55
if (marks > 60) {       
  cat("Pass")
} else {
  cat("Fail")
}
# YOUR FIX below:
marks <- 55
if (marks >= 40) {       
  cat("Pass")
} else {
  cat("Fail")
}
# The if statement needs to be updated to marks>=40 to correctly evaluate the condition
