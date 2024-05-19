
library(e1071)

# Set seed for reproducibility
set.seed(123)

# Jumlah sampel untuk masing-masing skenario
n1 <- 1000
n2 <- 10000
n3 <- 100000

# Skenario n1
CO_n1 <- round(runif(n1, min = 0, max = 500),0)  # Example range for CO
CO2_n1 <- round(runif(n1, min = 0, max = 500),0)  # Example range for CO2

# Skenario n2
CO_n2 <- round(runif(n2, min = 0, max = 500),0)  # Example range for CO
CO2_n2 <- round(runif(n2, min = 0, max = 500),0)  # Example range for CO2

# Skenario n3
CO_n3 <- round(runif(n3, min = 0, max = 500),0)  # Example range for CO
CO2_n3 <- round(runif(n3, min = 0, max = 500),0)  # Example range for CO2

# Combine data into a list or dataframe for each scenario
data_n1 <- data.frame(CO_n1,CO2_n1)
data_n2 <- data.frame(CO_n2, CO2_n2)
data_n3 <- data.frame(CO_n3, CO2_n3)

# Fungsi fuzzyfikasi untuk mengklasifikasikan nilai ISPU ke dalam kategori kualitas udara
fuzzyfikasi_ispu <- function(ispu_value) {
  if (ispu_value >= 0 && ispu_value <= 50) {
    return("Baik")
  } else if (ispu_value >= 51 && ispu_value <= 100) {
    return("Sedang")
  } else if (ispu_value >= 101 && ispu_value <= 199) {
    return("Tidak Sehat")
  } else if (ispu_value >= 200 && ispu_value <= 299) {
    return("Sangat Tidak Sehat")
  } else if (ispu_value >= 300) {
    return("Berbahaya")
  } else {
    return(NA) # Nilai di luar rentang yang valid
  }
}

# Apply fuzzyfikasi to each column n1
data_n1$Kategori_CO <- sapply(data_n1$CO_n1, fuzzyfikasi_ispu)
data_n1$Kategori_CO2 <- sapply(data_n1$CO2_n1, fuzzyfikasi_ispu)

#Statistik Deskriptif n1
Stat.desk_n1_co<-table(data_n1$Kategori_CO)
Stat.desk_n1_co2<-table(data_n1$Kategori_CO2)

# Apply fuzzyfikasi to each column n2
data_n2$Kategori_CO <- sapply(data_n2$CO_n2, fuzzyfikasi_ispu)
data_n2$Kategori_CO2 <- sapply(data_n2$CO2_n2, fuzzyfikasi_ispu)

#Statistik Deskriptif n2
Stat.desk_n2_co<-table(data_n2$Kategori_CO)
Stat.desk_n2_co2<-table(data_n2$Kategori_CO2)


# Apply fuzzyfikasi to each column n3
data_n3$Kategori_CO <- sapply(data_n3$CO_n3, fuzzyfikasi_ispu)
data_n3$Kategori_CO2 <- sapply(data_n3$CO2_n3, fuzzyfikasi_ispu)

#Statistik Deskriptif n3
Stat.desk_n3_co<-table(data_n3$Kategori_CO)
Stat.desk_n3_co2<-table(data_n3$Kategori_CO2)

#---------------------------------Bayes N1---------------------------------------------------#
# Hitung jumlah sampel untuk setiap kategori kualitas udara (Baik, Sedang, Tidak Sehat, Sangat Tidak Sehat, Berbahaya)
count_Baik <- sum(data_n3$Kategori_CO == "Baik")
count_Sedang <- sum(data_n3$Kategori_CO == "Sedang")
count_Tidak_Sehat <- sum(data_n3$Kategori_CO == "Tidak Sehat")
count_Sangat_Tidak_Sehat <- sum(data_n3$Kategori_CO == "Sangat Tidak Sehat")
count_Berbahaya <- sum(data_n3$Kategori_CO == "Berbahaya")

# Hitung total jumlah sampel
total_samples <- nrow(data_n3)

# Hitung probabilitas prior untuk setiap kategori kualitas udara
prior_Baik <- count_Baik / total_samples
prior_Sedang <- count_Sedang / total_samples
prior_Tidak_Sehat <- count_Tidak_Sehat / total_samples
prior_Sangat_Tidak_Sehat <- count_Sangat_Tidak_Sehat / total_samples
prior_Berbahaya <- count_Berbahaya / total_samples

# Output hasil
cat("Probabilitas prior untuk kategori Baik:", prior_Baik, "\n")
cat("Probabilitas prior untuk kategori Sedang:", prior_Sedang, "\n")
cat("Probabilitas prior untuk kategori Tidak Sehat:", prior_Tidak_Sehat, "\n")
cat("Probabilitas prior untuk kategori Sangat Tidak Sehat:", prior_Sangat_Tidak_Sehat, "\n")
cat("Probabilitas prior untuk kategori Berbahaya:", prior_Berbahaya, "\n")

# Membuat data frame contoh
data1 <- data.frame(
  CO = c(data_n1$Kategori_CO),
  CO2 = c(data_n1$Kategori_CO2)
)

# Menetapkan aturan fuzzy
data1$Output <- with(data, ifelse(CO == "Baik" & CO2 == "Baik", "Baik",
                                 ifelse(CO == "Baik" & CO2 == "Sedang", "Sedang",
                                        ifelse(CO == "Baik" & CO2 == "Tidak Sehat", "Tidak Sehat",
                                               ifelse(CO == "Baik" & CO2 == "Sangat Tidak Sehat", "Sangat Tidak Sehat",
                                                      ifelse(CO == "Baik" & CO2 == "Berbahaya", "Berbahaya",
                                                             ifelse(CO == "Sedang" & CO2 == "Baik", "Sedang",
                                                                    ifelse(CO == "Sedang" & CO2 == "Sedang", "Sedang",
                                                                           ifelse(CO == "Sedang" & CO2 == "Tidak Sehat", "Tidak Sehat",
                                                                                  ifelse(CO == "Sedang" & CO2 == "Sangat Tidak Sehat", "Sangat Tidak Sehat",
                                                                                         ifelse(CO == "Sedang" & CO2 == "Berbahaya", "Berbahaya",
                                                                                                ifelse(CO == "Tidak Sehat" & CO2 == "Baik", "Tidak Sehat",
                                                                                                       ifelse(CO == "Tidak Sehat" & CO2 == "Sedang", "Tidak Sehat",
                                                                                                              ifelse(CO == "Tidak Sehat" & CO2 == "Tidak Sehat", "Tidak Sehat",
                                                                                                                     ifelse(CO == "Tidak Sehat" & CO2 == "Sangat Tidak Sehat", "Sangat Tidak Sehat",
                                                                                                                            ifelse(CO == "Tidak Sehat" & CO2 == "Berbahaya", "Berbahaya",
                                                                                                                                   ifelse(CO == "Sangat Tidak Sehat" & CO2 == "Baik", "Sangat Tidak Sehat",
                                                                                                                                          ifelse(CO == "Sangat Tidak Sehat" & CO2 == "Sedang", "Sangat Tidak Sehat",
                                                                                                                                                 ifelse(CO == "Sangat Tidak Sehat" & CO2 == "Tidak Sehat", "Sangat Tidak Sehat",
                                                                                                                                                        ifelse(CO == "Sangat Tidak Sehat" & CO2 == "Sangat Tidak Sehat", "Sangat Tidak Sehat",
                                                                                                                                                               ifelse(CO == "Sangat Tidak Sehat" & CO2 == "Berbahaya", "Berbahaya",
                                                                                                                                                                      ifelse(CO == "Berbahaya" & CO2 == "Baik", "Berbahaya",
                                                                                                                                                                             ifelse(CO == "Berbahaya" & CO2 == "Sedang", "Berbahaya",
                                                                                                                                                                                    ifelse(CO == "Berbahaya" & CO2 == "Tidak Sehat", "Berbahaya",
                                                                                                                                                                                           ifelse(CO == "Berbahaya" & CO2 == "Sangat Tidak Sehat", "Berbahaya",
                                                                                                                                                                                                  ifelse(CO == "Berbahaya" & CO2 == "Berbahaya", "Berbahaya", NA))))))))))))))))))))))))))

# Menampilkan data dengan output kategori kualitas udara
print(data1)

#Hasil Klasifikasi
table(data1$Output)

# Membuat data frame sesuai dengan tabel yang diberikan
data2 <- data.frame(
  CO = c(data_n2$Kategori_CO),
  CO2 = c(data_n2$Kategori_CO2)
)

# Menetapkan aturan fuzzy
data2$Output <- with(data2, ifelse(CO == "Baik" & CO2 == "Baik", "Baik",
                                 ifelse(CO == "Baik" & CO2 == "Sedang", "Sedang",
                                        ifelse(CO == "Baik" & CO2 == "Tidak Sehat", "Tidak Sehat",
                                               ifelse(CO == "Baik" & CO2 == "Sangat Tidak Sehat", "Sangat Tidak Sehat",
                                                      ifelse(CO == "Baik" & CO2 == "Berbahaya", "Berbahaya",
                                                             ifelse(CO == "Sedang" & CO2 == "Baik", "Sedang",
                                                                    ifelse(CO == "Sedang" & CO2 == "Sedang", "Sedang",
                                                                           ifelse(CO == "Sedang" & CO2 == "Tidak Sehat", "Tidak Sehat",
                                                                                  ifelse(CO == "Sedang" & CO2 == "Sangat Tidak Sehat", "Sangat Tidak Sehat",
                                                                                         ifelse(CO == "Sedang" & CO2 == "Berbahaya", "Berbahaya",
                                                                                                ifelse(CO == "Tidak Sehat" & CO2 == "Baik", "Tidak Sehat",
                                                                                                       ifelse(CO == "Tidak Sehat" & CO2 == "Sedang", "Sedang",
                                                                                                              ifelse(CO == "Tidak Sehat" & CO2 == "Tidak Sehat", "Tidak Sehat",
                                                                                                                     ifelse(CO == "Tidak Sehat" & CO2 == "Sangat Tidak Sehat", "Sangat Tidak Sehat",
                                                                                                                            ifelse(CO == "Tidak Sehat" & CO2 == "Berbahaya", "Berbahaya",
                                                                                                                                   ifelse(CO == "Sangat Tidak Sehat" & CO2 == "Baik", "Sangat Tidak Sehat",
                                                                                                                                          ifelse(CO == "Sangat Tidak Sehat" & CO2 == "Sedang", "Sedang",
                                                                                                                                                 ifelse(CO == "Sangat Tidak Sehat" & CO2 == "Tidak Sehat", "Sangat Tidak Sehat",
                                                                                                                                                        ifelse(CO == "Sangat Tidak Sehat" & CO2 == "Sangat Tidak Sehat", "Sangat Tidak Sehat",
                                                                                                                                                               ifelse(CO == "Sangat Tidak Sehat" & CO2 == "Berbahaya", "Berbahaya",
                                                                                                                                                                      ifelse(CO == "Berbahaya" & CO2 == "Baik", "Berbahaya",
                                                                                                                                                                             ifelse(CO == "Berbahaya" & CO2 == "Sedang", "Berbahaya",
                                                                                                                                                                                    ifelse(CO == "Berbahaya" & CO2 == "Tidak Sehat", "Berbahaya",
                                                                                                                                                                                           ifelse(CO == "Berbahaya" & CO2 == "Sangat Tidak Sehat", "Berbahaya",
                                                                                                                                                                                                  ifelse(CO == "Berbahaya" & CO2 == "Berbahaya", "Berbahaya", NA))))))))))))))))))))))))))

# Menampilkan data dengan kolom Output
print(data2)

#Hasil Klasifikasi
table(data2$Output)

# Membuat data frame sesuai dengan tabel yang diberikan
data3 <- data.frame(
  CO = c(data_n3$Kategori_CO),
  CO2 = c(data_n3$Kategori_CO2)
)

# Menetapkan aturan fuzzy
data3$Output <- with(data3, ifelse(CO == "Baik" & CO2 == "Baik", "Baik",
                                 ifelse(CO == "Baik" & CO2 == "Sedang", "Baik",
                                        ifelse(CO == "Baik" & CO2 == "Tidak Sehat", "Tidak Sehat",
                                               ifelse(CO == "Baik" & CO2 == "Sangat Tidak Sehat", "Sangat Tidak Sehat",
                                                      ifelse(CO == "Baik" & CO2 == "Berbahaya", "Berbahaya",
                                                             ifelse(CO == "Sedang" & CO2 == "Baik", "Baik",
                                                                    ifelse(CO == "Sedang" & CO2 == "Sedang", "Sedang",
                                                                           ifelse(CO == "Sedang" & CO2 == "Tidak Sehat", "Tidak Sehat",
                                                                                  ifelse(CO == "Sedang" & CO2 == "Sangat Tidak Sehat", "Sangat Tidak Sehat",
                                                                                         ifelse(CO == "Sedang" & CO2 == "Berbahaya", "Berbahaya",
                                                                                                ifelse(CO == "Tidak Sehat" & CO2 == "Baik", "Tidak Sehat",
                                                                                                       ifelse(CO == "Tidak Sehat" & CO2 == "Sedang", "Tidak Sehat",
                                                                                                              ifelse(CO == "Tidak Sehat" & CO2 == "Tidak Sehat", "Tidak Sehat",
                                                                                                                     ifelse(CO == "Tidak Sehat" & CO2 == "Sangat Tidak Sehat", "Sangat Tidak Sehat",
                                                                                                                            ifelse(CO == "Tidak Sehat" & CO2 == "Berbahaya", "Berbahaya",
                                                                                                                                   ifelse(CO == "Sangat Tidak Sehat" & CO2 == "Baik", "Sangat Tidak Sehat",
                                                                                                                                          ifelse(CO == "Sangat Tidak Sehat" & CO2 == "Sedang", "Sangat Tidak Sehat",
                                                                                                                                                 ifelse(CO == "Sangat Tidak Sehat" & CO2 == "Tidak Sehat", "Sangat Tidak Sehat",
                                                                                                                                                        ifelse(CO == "Sangat Tidak Sehat" & CO2 == "Sangat Tidak Sehat", "Sangat Tidak Sehat",
                                                                                                                                                               ifelse(CO == "Sangat Tidak Sehat" & CO2 == "Berbahaya", "Berbahaya",
                                                                                                                                                                      ifelse(CO == "Berbahaya" & CO2 == "Baik", "Berbahaya",
                                                                                                                                                                             ifelse(CO == "Berbahaya" & CO2 == "Sedang", "Berbahaya",
                                                                                                                                                                                    ifelse(CO == "Berbahaya" & CO2 == "Tidak Sehat", "Berbahaya",
                                                                                                                                                                                           ifelse(CO == "Berbahaya" & CO2 == "Sangat Tidak Sehat", "Berbahaya",
                                                                                                                                                                                                  ifelse(CO == "Berbahaya" & CO2 == "Berbahaya", "Berbahaya", NA))))))))))))))))))))))))))

# Menampilkan data dengan kolom Output
print(data3)

#Hasil Klasifikasi
table(data3$Output)
