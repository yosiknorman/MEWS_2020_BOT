#!/usr/bin/Rscript

#  Pilih Waktu Analisis
DT_CASE = "01-01-2020 00:00"
Kanal = "13"
print(paste0("Analisis Untuk Tanggal : ", DT_CASE))
DMYHH = strsplit(DT_CASE, split = " ")[[1]]
DMY = strsplit(DMYHH[1], split = "-")[[1]]
Y = DMY[3] ; M = DMY[2] ; DMY[1];
HH   = strsplit(DMYHH[2], split = ":")[[1]]
H = HH[1] ; Min = HH[2]

if(DMYHH[2] == "00:00"){
  DC = as.POSIXct(paste0(paste(rev(DMY), collapse = "-"), " ", H,":", Min))+1
}else{
  DC = as.POSIXct(paste0(paste(rev(DMY), collapse = "-"), " ", H,":", Min))
}
DC0 = DC - (24*60*60)
DC1 = DC + (24*60*60)
DCALL = as.character(seq(DC0, DC1, by = "hour"))
DCALL_STR = substr(DCALL, 1, nchar(DCALL)-3)
DCALL_STR = gsub(DCALL_STR, pattern = ":", replacement = "")
DY = substr(DCALL_STR, 1,4) ; DM = substr(DCALL_STR, 6,7) ; DD = substr(DCALL_STR, 9,10)
DH = substr(DCALL_STR, 12,13) ; DMin = substr(DCALL_STR, 14,15)
sat_url="ftp://96733:96733$%^@202.90.199.64/himawari2/HIMAWARI-8/DATA/NC/Indonesia/" # GANTI
U2 = paste0(sat_url, DY, "/", DM, "/", DD ,"/", "H08_B",Kanal,"_Indonesia_", DY, DM, DD, DH, DMin, ".nc" )
# "H08_B16_Indonesia_201912312240
# setwd("../hindcast_warning/")
system("mkdir data/nc")
setwd("data/nc")
for(i in 1:length(U2)){
  system(paste0("curl -O ", U2[i]))
}
setwd("../..")