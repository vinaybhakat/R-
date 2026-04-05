# ============================================================
#  BCA 4th Semester | R Programming Assignment
#  Student  : Sunny Rana
#  Roll No  : BCA/4/SET4/01
#  Deadline : 10 April 2026
# ============================================================

# ── Q1 [Unit 1] Variables, I/O & String Formatting ──────────
# Task: Input name + age → personalised greeting + birth year

cat("===== Q1: Personalised Greeting =====\n")

name     <- "Aaditya Chauhan"
age      <- 20L
branch   <- "BCA"
semester <- 4L

birth_year   <- 2026L - age
days_lived   <- age * 365L
next_bday    <- 2026L - age + 21L

cat(sprintf("Hello, %s!\n", name))
cat(sprintf("Branch  : %s | Semester : %d\n", branch, semester))
cat(sprintf("Age     : %d years  |  Born in : %d\n", age, birth_year))
cat(sprintf("You have lived approximately %s days.\n",
            format(days_lived, big.mark = ",")))
cat(sprintf("You will turn 21 in %d.\n\n", next_bday))


# ── Q2 [Unit 1] Vectors, Statistics & Logical Filtering ─────
# Task: Marks vector → full stats + pass/fail classification

cat("===== Q2: Marks Analysis =====\n")

subjects <- c("Maths", "Physics", "Chemistry", "English", "Computer", "Hindi")
marks    <- c(87, 73, 91, 65, 95, 58)

# descriptive stats
cat(sprintf("Subjects  : %s\n", paste(subjects, collapse = ", ")))
cat(sprintf("Marks     : %s\n", paste(marks,    collapse = ", ")))
cat(sprintf("Total     : %d / %d\n",  sum(marks), length(marks) * 100L))
cat(sprintf("Percentage: %.2f%%\n",   mean(marks)))
cat(sprintf("Highest   : %d (%s)\n",  max(marks),  subjects[which.max(marks)]))
cat(sprintf("Lowest    : %d (%s)\n",  min(marks),  subjects[which.min(marks)]))
cat(sprintf("Std Dev   : %.2f\n",     sd(marks)))
cat(sprintf("Median    : %.1f\n\n",   median(marks)))

# pass/fail per subject
pass_thresh <- 60
result <- ifelse(marks >= pass_thresh, "PASS", "FAIL")
for (i in seq_along(subjects)) {
  cat(sprintf("  %-12s : %3d  [%s]\n", subjects[i], marks[i], result[i]))
}

# overall grade
pct <- mean(marks)
grade <- dplyr::case_when(
  pct >= 90 ~ "A+",
  pct >= 75 ~ "A",
  pct >= 60 ~ "B",
  pct >= 45 ~ "C",
  TRUE      ~ "Fail"
)
# using base R instead of dplyr for portability:
grade <- if (pct >= 90) "A+" else if (pct >= 75) "A" else if (pct >= 60) "B" else if (pct >= 45) "C" else "Fail"
cat(sprintf("\nOverall Grade: %s (%.2f%%)\n\n", grade, pct))


# ── Q3 [Unit 2] Data Frame, Analysis & Bar Chart ────────────
# Task: Student data frame → analysis → bar chart with colour

cat("===== Q3: Student Data Frame & Chart =====\n")

students <- data.frame(
  Name    = c("Aaditya", "Riya", "Mohit", "Priya", "Deepak",
              "Sunita",  "Rahul","Neha",  "Arjun", "Kavya"),
  Maths   = c(87, 92, 68, 75, 55, 80, 63, 90, 72, 88),
  Science = c(79, 85, 70, 82, 60, 74, 77, 91, 66, 83),
  English = c(65, 78, 80, 70, 72, 68, 85, 76, 90, 71),
  stringsAsFactors = FALSE
)

students$Total      <- rowSums(students[, 2:4])
students$Percentage <- round(students$Total / 300 * 100, 2)
students$Grade      <- ifelse(students$Percentage >= 75, "Distinction",
                       ifelse(students$Percentage >= 60, "First Class",
                       ifelse(students$Percentage >= 45, "Second Class", "Pass")))

# sort by percentage
students <- students[order(-students$Percentage), ]
students$Rank <- seq_len(nrow(students))

cat("Student Report Card:\n")
print(students[, c("Rank","Name","Maths","Science","English","Total","Percentage","Grade")])

cat(sprintf("\nClass Topper : %s (%.1f%%)\n",
            students$Name[1], students$Percentage[1]))
cat(sprintf("Class Average: %.2f%%\n\n", mean(students$Percentage)))

# bar chart
bar_colors <- colorRampPalette(c("#1a73e8", "#34a853", "#fbbc04", "#ea4335"))(nrow(students))
bp <- barplot(
  students$Percentage,
  names.arg = students$Name,
  col       = bar_colors,
  main      = "BCA Set-4 | Student Performance",
  xlab      = "Student Name",
  ylab      = "Percentage (%)",
  ylim      = c(0, 110),
  las       = 2,
  cex.names = 0.85,
  border    = "white"
)
abline(h = 75, col = "red",    lty = 2, lwd = 2)
abline(h = 60, col = "orange", lty = 2, lwd = 2)
text(bp, students$Percentage + 2,
     labels = paste0(students$Percentage, "%"),
     cex = 0.75, col = "black")
legend("topright",
       legend = c("Distinction (75%)", "First Class (60%)"),
       col    = c("red", "orange"), lty = 2, lwd = 2, cex = 0.8)


# ── Q4 [Unit 3] Functions, Loops & Recursion ────────────────
# Task: Grade calculator + Fibonacci + prime check

cat("\n===== Q4: Functions & Algorithms =====\n")

# 4a: Grade calculator function
calculate_grade <- function(marks) {
  if (!is.numeric(marks) || marks < 0 || marks > 100)
    stop("Marks must be a number between 0 and 100.")
  grade <- if      (marks >= 90) "A+"
            else if (marks >= 75) "A"
            else if (marks >= 60) "B"
            else if (marks >= 45) "C"
            else                  "Fail"
  status <- if (marks >= 45) "PASS" else "FAIL"
  cat(sprintf("  Marks: %5.1f  →  Grade: %-4s  [%s]\n", marks, grade, status))
  invisible(list(grade = grade, status = status))
}

cat("Grade Calculator:\n")
test_marks <- c(95, 82, 67, 50, 38, 100, 44.5)
for (m in test_marks) calculate_grade(m)

# 4b: Fibonacci sequence using a loop
cat("\nFibonacci Sequence (first 15 terms):\n")
fibonacci <- function(n) {
  fib <- integer(n)
  fib[1] <- 0L; if (n > 1) fib[2] <- 1L
  for (i in 3:n) fib[i] <- fib[i-1] + fib[i-2]
  return(fib)
}
fib_seq <- fibonacci(15)
cat(paste(fib_seq, collapse = " → "), "\n")

# 4c: Prime checker
is_prime <- function(n) {
  if (n < 2)  return(FALSE)
  if (n == 2) return(TRUE)
  if (n %% 2 == 0) return(FALSE)
  for (i in 3:floor(sqrt(n))) {
    if (n %% i == 0) return(FALSE)
  }
  return(TRUE)
}

cat("\nPrime numbers between 1 and 50:\n")
primes <- Filter(is_prime, 1:50)
cat(paste(primes, collapse = ", "), "\n")
cat(sprintf("Total primes found: %d\n", length(primes)))
