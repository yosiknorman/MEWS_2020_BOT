# Catatan
# 1. 
source("hindcast_warning.R") # Persiapan Download atau Copy data 
# 2. 
source("CORRECTION_BIAS_10_with_ARGorAWS") # Mendapatkan data bias untuk koreksi nilai satelit
# 3.
source("Data_Cluster.R") # Pendeteksian Cluster Objek Awan Convective
# 4.
source("Connected_Cluster.R") # Pengkoneksian Cluster antar waktu
# 5.
source("Coba_Ulang.R") # AWAL MCS
# 6.
source("GET_INFO.R") # Mendapat meta data cluster system
# 7.
source("Nowcasting.R") # Prediksi Pergerakan track Cloud System 1 Jam Kedepan
# 8.
source("Warning_Prediction") # Warning 1 jam kedepan
# 9.
source("POST_Processing_CS") # Pengarsipan MCS 
