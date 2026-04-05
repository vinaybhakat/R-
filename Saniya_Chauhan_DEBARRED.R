# ============================================================
#  BCA 4th Semester | R Programming Assignment
#  Student  : Saniya Chauhan
#  Status: Debarred
#  Deadline : 10 April 2026
# ============================================================

# ── Q1 [Unit 1] All Data Types with Verification ────────────
cat("===== Q1: R Data Types =====\n")

# Integer
age <- 21L
cat(sprintf("integer   | value: %-8s | is.integer: %s | class: %s\n",
            age, is.integer(age), class(age)))

# Double
height <- 5.9
cat(sprintf("double    | value: %-8s | is.double : %s | class: %s\n",
            height, is.double(height), class(height)))

# Character
name <- "Saniya Chauhan"
cat(sprintf("character | value: %-12s | is.character: %s | nchar: %d\n",
            name, is.character(name), nchar(name)))

# Logical
is_pass <- TRUE
cat(sprintf("logical   | value: %-8s | is.logical : %s\n",
            is_pass, is.logical(is_pass)))

# Complex
z <- 3 + 4i
cat(sprintf("complex   | value: %-8s | Mod: %.2f | Arg: %.4f rad\n",
            z, Mod(z), Arg(z)))

# Raw
raw_val <- chartr("", "", "A")
r <- as.raw(65L)  # ASCII 65 = 'A'
cat(sprintf("raw       | value: %s | is.raw: %s\n\n", r, is.raw(r)))

# Type conversion table
cat("Type Conversion Table:\n")
cat(sprintf("  %-10s → integer: %-5s char: %-10s logical: %s\n",
            "3.7", as.integer(3.7), as.character(3.7), as.logical(3.7)))
cat(sprintf("  %-10s → integer: %-5s double: %-10s logical: %s\n",
            "TRUE", as.integer(TRUE), as.double(TRUE), as.logical(TRUE)))
cat(sprintf("  %-10s → double : %-10s logical: %s\n\n",
            '"0"', as.double("0"), as.logical("TRUE")))


# ── Q2 [Unit 2] Matrix Operations ───────────────────────────
cat("===== Q2: Matrix Operations =====\n")

M <- matrix(c(2, 4, 1,
              7, 3, 8,
              5, 6, 9),
            nrow = 3, ncol = 3, byrow = TRUE)

cat("Original Matrix M:\n"); print(M)

cat("\nTranspose of M:\n");         print(t(M))
cat("\nRow Sums  :", rowSums(M),   "\n")
cat("Col Sums  :", colSums(M),    "\n")
cat("Row Means :", rowMeans(M),   "\n")
cat("Col Means :", colMeans(M),   "\n")
cat("\nM × 5:\n");                  print(M * 5)
cat("\nM² (element-wise):\n");      print(M ^ 2)
cat("\nMatrix Multiplication M × M:\n"); print(M %*% M)

# Determinant & inverse (needs non-singular matrix)
det_M <- det(M)
cat(sprintf("\nDeterminant of M : %.2f\n", det_M))
if (abs(det_M) > 1e-10) {
  inv_M <- solve(M)
  cat("Inverse of M:\n"); print(round(inv_M, 4))
  cat("Verify M × inv(M) ≈ Identity:\n")
  print(round(M %*% inv_M, 6))
}

# Diagonal and trace
cat(sprintf("\nMain Diagonal : %s\n", paste(diag(M), collapse = ", ")))
cat(sprintf("Trace (sum of diag): %d\n\n", sum(diag(M))))


# ── Q3 [Unit 2] iris Dataset — Colour-coded Scatter ─────────
cat("===== Q3: iris Dataset — Species Scatter Plot =====\n")

data(iris)
cat(sprintf("Dataset: %d rows × %d columns\n", nrow(iris), ncol(iris)))
cat("Species:\n")
for (sp in levels(iris$Species)) {
  n <- sum(iris$Species == sp)
  cat(sprintf("  %-15s: %d observations\n", sp, n))
}

# Summary stats per species
cat("\nSepal.Length by Species:\n")
for (sp in levels(iris$Species)) {
  sub <- iris[iris$Species == sp, "Sepal.Length"]
  cat(sprintf("  %-15s: mean=%.2f  sd=%.2f  range=%.1f–%.1f\n",
              sp, mean(sub), sd(sub), min(sub), max(sub)))
}

# Scatter plot with colour per species
species_cols <- c("setosa" = "#E53935",
                  "versicolor" = "#1E88E5",
                  "virginica"  = "#43A047")
plot(
  iris$Sepal.Length, iris$Sepal.Width,
  col  = species_cols[as.character(iris$Species)],
  pch  = c(setosa=15, versicolor=17, virginica=19)[as.character(iris$Species)],
  cex  = 1.2,
  main = "iris Dataset — Sepal Length vs Width by Species",
  xlab = "Sepal Length (cm)",
  ylab = "Sepal Width (cm)"
)
legend("topright",
       legend = levels(iris$Species),
       col    = unname(species_cols),
       pch    = c(15,17,19),
       title  = "Species", cex = 0.9)

# Correlation per species
cat("\nCorrelation(Sepal.Length, Sepal.Width) by Species:\n")
for (sp in levels(iris$Species)) {
  sub <- iris[iris$Species == sp, ]
  r   <- cor(sub$Sepal.Length, sub$Sepal.Width)
  cat(sprintf("  %-15s: r = %+.4f\n", sp, r))
}
cat("\n")


# ── Q4 [Unit 3] Nested Loops — Star Patterns ────────────────
cat("===== Q4: Star Patterns with Nested Loops =====\n")

rows <- 6L

# Pattern 1: Right triangle
cat("Pattern 1 — Right Triangle:\n")
for (i in 1:rows) {
  cat(strrep("* ", i), "\n", sep = "")
}

# Pattern 2: Inverted triangle
cat("\nPattern 2 — Inverted Triangle:\n")
for (i in rows:1) {
  cat(strrep("* ", i), "\n", sep = "")
}

# Pattern 3: Pyramid
cat("\nPattern 3 — Pyramid:\n")
for (i in 1:rows) {
  cat(strrep(" ", rows - i))
  cat(strrep("* ", i))
  cat("\n")
}

# Pattern 4: Diamond
cat("\nPattern 4 — Diamond:\n")
for (i in 1:rows) {
  cat(strrep(" ", rows - i))
  cat(strrep("* ", i))
  cat("\n")
}
for (i in (rows-1):1) {
  cat(strrep(" ", rows - i))
  cat(strrep("* ", i))
  cat("\n")
}

# Pattern 5: Number triangle
cat("\nPattern 5 — Number Triangle:\n")
for (i in 1:rows) {
  for (j in 1:i) cat(j, "")
  cat("\n")
}

# Pattern 6: Pascal's triangle (bonus)
cat("\nBonus — Pascal's Triangle (6 rows):\n")
pascal <- function(n) {
  row <- 1L
  for (i in 1:n) {
    cat(strrep("  ", n - i))
    cat(paste(row, collapse = "  "), "\n")
    row <- c(0L, row) + c(row, 0L)
  }
}
pascal(6L)
