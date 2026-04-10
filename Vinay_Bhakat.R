# ============================================================
#  BCA 4th Semester | R Programming Assignment
#  Student  : Vinay Bhakat
#  Roll No  : BCA/4/SET4/03
#  Deadline : 10 April 2026
# ============================================================
#  R Programming Assignment (FINAL WITH FIXES)
# ============================================================


# ── Q1: Calculator with Formatted Output ─────────────────────
cat("===== Q1: Calculator with Formatted Output =====\n")

num1 <- 48
num2 <- 7

ops_results <- data.frame(
  Operation  = c("Addition", "Subtraction", "Multiplication",
                 "Division", "Integer Div", "Modulus", "Power"),
  Expression = c(
    sprintf("%g + %g", num1, num2),
    sprintf("%g - %g", num1, num2),
    sprintf("%g × %g", num1, num2),
    sprintf("%g ÷ %g", num1, num2),
    sprintf("%g %/% %g", num1, num2),
    sprintf("%g %% %g",  num1, num2),
    sprintf("%g ^ %g",   num1, num2)
  ),
  Result = c(
    num1 + num2,
    num1 - num2,
    num1 * num2,
    ifelse(num2 != 0, round(num1 / num2, 6), NA),   # FIXED (zero division safe)
    ifelse(num2 != 0, num1 %/% num2, NA),           # FIXED
    ifelse(num2 != 0, num1 %% num2, NA),            # FIXED
    num1 ^ num2
  )
)

cat(sprintf("Numbers entered: a = %g, b = %g\n\n", num1, num2))
cat(sprintf("%-18s %-16s %s\n", "Operation", "Expression", "Result"))
cat(strrep("-", 50), "\n")

for (i in seq_len(nrow(ops_results))) {
  cat(sprintf("%-18s %-16s %g\n",
              ops_results$Operation[i],
              ops_results$Expression[i],
              ops_results$Result[i]))
}
cat("\n\n")


# ── Q2: Student Record Using Named List ──────────────────────
cat("===== Q2: Student Record Using Named List =====\n")

student_record <- list(
  name      = "Vinay Bhakat",
  roll      = 3L,
  subjects  = c("Data Structures", "DBMS", "OS", "R Programming", "Maths"),
  marks     = c(82, 76, 68, 91, 55),
  cgpa      = 7.9
)

student_record$pass_fail <- student_record$marks >= 40
student_record$percentage <- round(mean(student_record$marks), 1)   # FIXED (correct percentage)

get_grade <- function(m) {   # FIXED (clean grading function)
  if (m >= 80) "A"
  else if (m >= 65) "B"
  else if (m >= 50) "C"
  else if (m >= 40) "D"
  else "F"
}

cat("Student Record:\n")
cat(sprintf("Name    : %s\n", student_record$name))
cat(sprintf("Roll No : %d\n", student_record$roll))
cat(sprintf("CGPA    : %.1f\n\n", student_record$cgpa))

cat(sprintf("%-20s %5s %6s %6s\n", "Subject", "Marks", "Pass?", "Grade"))
cat(strrep("-", 45), "\n")

for (i in seq_along(student_record$subjects)) {
  cat(sprintf("%-20s %5d %6s %6s\n",
              student_record$subjects[i],
              student_record$marks[i],
              ifelse(student_record$pass_fail[i], "YES", "NO"),
              get_grade(student_record$marks[i])))
}

cat(sprintf("\nAverage: %.1f | Passed: %d/%d subjects\n\n",
            student_record$percentage,
            sum(student_record$pass_fail),
            length(student_record$marks)))

# ── Q3 [Unit 2] mtcars Dataset — Histogram + Density + Line ─

cat("===== Q3: mtcars MPG Analysis =====\n")

data(mtcars)
mpg <- mtcars$mpg

mpg_mean <- mean(mpg)
mpg_med  <- median(mpg)
mpg_sd   <- sd(mpg)

# FIXED: Added proper formatted statistical output
cat(sprintf("Total Cars: %d\n", nrow(mtcars)))
cat(sprintf("Mean: %.2f | Median: %.2f\n", mpg_mean, mpg_med))
cat(sprintf("Std Dev: %.2f | Min: %.1f | Max: %.1f\n\n",
            mpg_sd, min(mpg), max(mpg)))

# FIXED: Clean histogram + better labels
hist(mpg,
     breaks = 10,
     col = "lightblue",
     main = "MPG Distribution",
     xlab = "Miles Per Gallon (MPG)")   # FIXED (better label)

# FIXED: Added density + mean + median lines
lines(density(mpg), lwd = 2)
abline(v = mpg_mean, lty = 2)
abline(v = mpg_med,  lty = 3)

# FIXED: Improved grouped analysis output
cat("MPG by Cylinders:\n")
for (cyl in sort(unique(mtcars$cyl))) {
  grp <- mtcars$mpg[mtcars$cyl == cyl]
  cat(sprintf("%d cyl -> Mean: %.2f | Min: %.1f | Max: %.1f\n",
              cyl, mean(grp), min(grp), max(grp)))   # FIXED
}
cat("\n\n")

# ── Q4 [Unit 3] is_prime + Sieve of Eratosthenes ────────────

cat("===== Q4: Prime Numbers =====\n")

# FIXED: Improved prime function (efficient using sqrt)
is_prime <- function(n) {
  if (n < 2) return(FALSE)
  if (n == 2) return(TRUE)            # FIXED (edge case)
  if (n %% 2 == 0) return(FALSE)      # FIXED (skip even numbers)

  for (i in seq(3, floor(sqrt(n)), by = 2)) {   # FIXED (optimized loop)
    if (n %% i == 0) return(FALSE)
  }
  TRUE
}

cat("Prime check (1–20):\n")
for (i in 1:20) {
  cat(sprintf("%2d -> %s\n", i, is_prime(i)))
}

# FIXED: Efficient prime generation
cat("\nPrimes up to 100:\n")
primes <- c()
for (i in 1:100) {
  if (is_prime(i)) {
    primes <- c(primes, i)
  }
}
cat(primes)

# FIXED: Added count of primes
cat(sprintf("\n\nTotal primes ≤ 100: %d\n", length(primes)))
