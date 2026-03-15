#| message: false
library(dslabs)
data("polls_us_election_2016")

# Kişisel Bilgiler
my_first <- "Pinar"
my_birth_year <- 1997

# k Değerinin Hesaplanması
k <- (nchar(my_first) + my_birth_year) %% 15 + 8
cat("Hesaplanan k değeri:", k, "\n")

# k değerine göre ilk veya son satırları göster
if (k %% 2 == 0) {
  cat("\nİlk", k, "satır (k çift):\n")
  print(head(polls_us_election_2016, k))
} else {
  cat("\nSon", k, "satır (k tek):\n")
  print(tail(polls_us_election_2016, k))
}

# NA Değerlerinin Analizi
total_na <- sum(is.na(polls_us_election_2016))
cat("\nToplam NA sayısı:", total_na, "\n")

na_per_col <- sort(colSums(is.na(polls_us_election_2016)), decreasing = TRUE)
cat("\nEn çok NA içeren ilk 8 sütun:\n")
print(head(na_per_col, 8))

# NA Değerlerini Değiştirme
new_data <- polls_us_election_2016

for (i in seq_along(new_data)) {
  if (is.numeric(new_data[[i]])) {
    # Sayısal sütunlar için: doğum yılı + k
    new_data[[i]][is.na(new_data[[i]])] <- my_birth_year + k
  } else {
    # Karakter/Faktör sütunlar için: isim_k
    new_data[[i]] <- as.character(new_data[[i]])
    new_data[[i]][is.na(new_data[[i]])] <- paste0(my_first, "_", k)
  }
}

# Yeni Veri Seti Kontrolleri
cat("\nYeni veri setinden k satır:\n")
if (k %% 2 == 0) {
  print(head(new_data, k))
} else {
  print(tail(new_data, k))
}

cat("\nYeni veri setinde kalan NA sayısı:", sum(is.na(new_data)), "\n")
cat("anyNA sonucu (FALSE olmalı):", anyNA(new_data), "\n")
