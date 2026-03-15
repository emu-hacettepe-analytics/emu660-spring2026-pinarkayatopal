# 1. Custom summary function
compute_stats <- function(x) {
  if (!is.numeric(x)) return(NULL)
  
  stats <- list(
    mean     = mean(x, na.rm = TRUE),
    median   = median(x, na.rm = TRUE),
    variance = var(x, na.rm = TRUE),
    iqr      = IQR(x, na.rm = TRUE),
    min      = min(x, na.rm = TRUE),
    max      = max(x, na.rm = TRUE)
  )
  return(stats)
}

# 2. For Loop ile Uygulama
cat("--- For Loop Sonuclari ---\n")
for (col_name in names(mtcars)) {
  cat("\nSutun:", col_name, "\n")
  print(compute_stats(mtcars[[col_name]]))
}

# 3. sapply ile Uygulama
cat("\n--- sapply Sonuçları ---\n")
res_sapply <- sapply(mtcars, compute_stats, simplify = FALSE)
print(res_sapply)

# 4. apply ile Uygulama
cat("\n--- apply Sonuçları ---\n")
res_apply <- apply(as.matrix(mtcars), 2, compute_stats)
print(res_apply)

