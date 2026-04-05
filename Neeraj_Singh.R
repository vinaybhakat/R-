# ============================================================
#  BCA 4th Semester | R Programming Assignment
#  Student  : Neeraj Singh
#  Roll No  : BCA/4/SET4/05
#  Deadline : 10 April 2026
# ============================================================

# ── Q1 [Unit 1] Sequences, rep() & Vector Operations ────────
cat("===== Q1: Sequences and Vectors =====\n")

# Sequence with step 3
seq1 <- seq(from = 1, to = 50, by = 3)
cat("seq(1, 50, by=3):\n ")
cat(seq1, "\n")
cat(sprintf("Length: %d  |  Sum: %d  |  Mean: %.2f\n\n",
            length(seq1), sum(seq1), mean(seq1)))

# 10 equally spaced values 0–1
seq2 <- seq(0, 1, length.out = 10)
cat("seq(0, 1, length.out=10):\n ")
cat(round(seq2, 4), "\n")
cat(sprintf("Length: %d\n\n", length(seq2)))

# rep() patterns
cat("rep() Examples:\n")
cat("  rep(1:4, times=3)  :", rep(1:4, times = 3), "\n")
cat("  rep(1:4, each=2)   :", rep(1:4, each  = 2), "\n")
cat("  rep(c(0,1), len=11):", rep(c(0,1), length.out = 11), "\n\n")

# Vector arithmetic
v1 <- seq(2, 20, by = 2)   # even numbers
v2 <- seq(1, 19, by = 2)   # odd numbers
cat("Even vector : ", v1, "\n")
cat("Odd  vector : ", v2, "\n")
cat("Sum of evens:", sum(v1), "| Sum of odds:", sum(v2), "\n")
cat("Cumulative sum of evens:", cumsum(v1), "\n\n")


# ── Q2 [Unit 1] String Manipulation ─────────────────────────
cat("===== Q2: String Operations =====\n")

first_name <- "Neeraj"
last_name  <- "Singh"
roll_no    <- 5L
branch     <- "BCA"

# Build student ID
student_id <- paste0(toupper(branch), "/SEM4/", sprintf("%03d", roll_no))
full_name  <- paste(first_name, last_name)

cat(sprintf("Full Name  : %s\n",    full_name))
cat(sprintf("Student ID : %s\n",    student_id))
cat(sprintf("Upper case : %s\n",    toupper(full_name)))
cat(sprintf("Lower case : %s\n",    tolower(full_name)))
cat(sprintf("nchar name : %d\n",    nchar(full_name)))
cat(sprintf("Reversed   : %s\n",
            paste(rev(strsplit(full_name, "")[[1]]), collapse = "")))

# String split and manipulate
words <- strsplit(full_name, " ")[[1]]
cat(sprintf("Word count : %d\n",    length(words)))
cat(sprintf("First word : %s\n",    words[1]))
cat(sprintf("Initials   : %s\n",
            paste(substr(words, 1, 1), collapse = ".")))

# gsub / sub demo
sentence <- "R programming is very very powerful and R is fun"
cat(sprintf("\nOriginal : %s\n", sentence))
cat(sprintf("Replace 1: %s\n",  sub("very", "extremely", sentence)))
cat(sprintf("Replace all: %s\n\n", gsub("very", "extremely", sentence)))


# ── Q3 [Unit 2] airquality Dataset Deep Dive ────────────────
cat("===== Q3: Air Quality Analysis =====\n")

data(airquality)
aq <- na.omit(airquality)   # remove rows with NA
cat(sprintf("Rows after removing NAs: %d (was %d)\n\n",
            nrow(aq), nrow(airquality)))

# Summary stats
for (col in c("Ozone", "Wind", "Temp")) {
  vals <- aq[[col]]
  cat(sprintf("%-6s | Mean=%-6.2f Med=%-6.2f SD=%-5.2f Min=%-5.1f Max=%.1f\n",
              col, mean(vals), median(vals), sd(vals), min(vals), max(vals)))
}

cat(sprintf("\nCorrelation(Ozone, Temp) : %.4f\n",   cor(aq$Ozone, aq$Temp)))
cat(sprintf("Correlation(Ozone, Wind) : %.4f\n\n",   cor(aq$Ozone, aq$Wind)))

# Line chart of Temp for first 30 days
first30 <- head(aq, 30)
plot(
  first30$Day, first30$Temp,
  type = "b",
  col  = "#1565C0",
  lwd  = 2,
  pch  = 19,
  cex  = 0.8,
  main = "Daily Temperature — First 30 Observations (airquality)",
  xlab = "Day",
  ylab = "Temperature (°F)",
  ylim = c(min(first30$Temp) - 5, max(first30$Temp) + 5)
)
abline(h = mean(first30$Temp), col = "red", lty = 2, lwd = 1.5)
legend("topright",
       legend = c("Temp", sprintf("Mean (%.1f°F)", mean(first30$Temp))),
       col = c("#1565C0","red"), lty = c(1,2), pch = c(19,NA), lwd = 2)

# Month-wise Ozone average
cat("Month-wise Average Ozone:\n")
for (m in sort(unique(aq$Month))) {
  sub_m <- aq[aq$Month == m, ]
  month_name <- month.name[m]
  cat(sprintf("  %-10s (Month %d): %.1f ppb  (n=%d)\n",
              month_name, m, mean(sub_m$Ozone), nrow(sub_m)))
}
cat("\n")


# ── Q4 [Unit 3] FizzBuzz + Extended Pattern ─────────────────
cat("===== Q4: FizzBuzz & Number Patterns =====\n")

cat("FizzBuzz 1 – 30:\n")
for (i in 1:30) {
  label <- ""
  if      (i %% 15 == 0) label <- "FizzBuzz"
  else if (i %%  3 == 0) label <- "Fizz"
  else if (i %%  5 == 0) label <- "Buzz"
  else                   label <- as.character(i)
  cat(sprintf("%8s", label))
  if (i %% 10 == 0) cat("\n")
}
cat("\n")

# Extended: also mark squares
cat("Extended FizzBuzz 1–50 (also marks perfect squares):\n")
perfect_squares <- (1:7)^2
for (i in 1:50) {
  parts <- character(0)
  if (i %% 3 == 0) parts <- c(parts, "Fizz")
  if (i %% 5 == 0) parts <- c(parts, "Buzz")
  if (i %% 7 == 0) parts <- c(parts, "Woof")
  if (i %in% perfect_squares) parts <- c(parts, "SQ")
  label <- if (length(parts) > 0) paste(parts, collapse="") else as.character(i)
  cat(sprintf("%10s", label))
  if (i %% 10 == 0) cat("\n")
}
cat("\n")
