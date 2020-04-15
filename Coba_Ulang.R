#!/usr/bin/Rscript
library(rgdal)
library(ncdf4)
library(maps)

# AMBIL DATA CONNECTION
rm(list = ls())
CC_LIST = list.files("data/ConnectedCluster/", pattern = ".bin")
CS_LIST = list.files("data/SYSTEM/", pattern = ".bin")

# PANGGIL 2 Data Pertama
PERTAMA = TRUE
DD = "01" ; YY = "2020" ; MM = "01" ; HH = "19" ; Min = "00" 
DMYH0 = as.character(as.POSIXct(paste0(YY, "-", MM,"-", DD,  " ", HH, ":", Min))-(60*60*1))
DMYH0 = gsub(DMYH0, pattern = "-", replacement = "")
DMYH0 = gsub(DMYH0, pattern = ":", replacement = "")
DMYH0 = substr(gsub(DMYH0, pattern = " ", replacement = ""), 1, 12)
DMYH1 = as.character(as.POSIXct(paste0(YY, "-", MM,"-", DD,  " ", HH, ":", Min)))
DMYH1 = gsub(DMYH1, pattern = c("-"), replacement = "")
DMYH1 = gsub(DMYH1, pattern = c( ":"), replacement = "")
DMYH1 = substr(gsub(DMYH1, pattern = c(" "), replacement = ""), 1, 12)
LAST = grep(CC_LIST, pattern = paste0(YY, MM, DD, HH))
if(PERTAMA == TRUE){
  START = LAST-1
  DC = list()
  for(i in START:LAST){
    load(paste0("data/ConnectedCluster/", CC_LIST[i]))
    DC[[i]] = CON
  }
  DC = DC[START:LAST]
  names(DC) = CC_LIST[START:LAST]
}
  # else{
#   START = grep(CS_LIST , pattern = DMYH0)
#   load("data/SYSTEM/", (CS_LIST))
#   DC
# }


# DC[[1]]


is.integer0 <- function(x){
  is.integer(x) && length(x) == 0L
}
is.character0 <- function(x){
  is.character(x) && length(x) == 0L
}

# SINGLE CASE
# KONEKSI ANTAR WAKTU
CON_CC_SINGLE = function(DC){
  SINGLE = grep(DC[[2]][,1], pattern = "&")
  DC2 = DC[[2]][,1]
  DC1 = as.integer(DC[[1]][,2])
  # SINGLE_CASE 
  SINGLE = grep(DC[[2]][,1], pattern = "&")
  DC2 = as.integer(DC[[2]][-SINGLE,1])
  D22 = as.integer(DC[[2]][-SINGLE,2])
  BAY = DC[[2]][c(-SINGLE),]
  # DC[[1]][which(DC1 %in% DC2),]
  MASUK = c()
  HAS = list()
  for(i in 1:length(DC2)){
    if(is.integer0(which(DC1 == DC2[i]))){
      MASUK[i] = 0
      HAS[[i]] = BAY[i,]
    }else{
      MASUK[i] = which(DC1 == DC2[i])
    }
  }
  # if(MASUK)
  MASUK_DC2 = which(MASUK != 0)
  MASUK_DC1 = MASUK[MASUK != 0]
  INI_NAME = names(DC[[2]])[2]
  HAS = data.frame(cbind(0, do.call("rbind", HAS)))
  
  # RET = data.frame(DC[[1]][MASUK_DC1,], INI_NAME = DC[[2]][MASUK_DC2,2], stringsAsFactors = F)
  RET = data.frame(DC[[1]][MASUK_DC1,], INI_NAME = D22[MASUK_DC2], stringsAsFactors = F)
  
  names(RET)[3]  = INI_NAME
  names(HAS) = names(RET)
  RET = rbind(RET, HAS)
  
  return(RET)
}
SINGLE_RESULT = CON_CC_SINGLE(DC = DC)
# --------------------------------------------------------------------------------------------------------
# FUNGSI PERSATUAN_MULTI_CLUSTER

# DC1_1 = CLUSTER 1 jam sebelum yang per array
# ke = 4
The_Multi = function(ke){
  Multi = DC2[ke]
  D2 = D22[ke]
  iMulti = as.integer(unlist(strsplit(Multi, split = "&")))
  SIMPAN_MULTI1 = list()
  MASUK = c()
  HELL = list()
  for(i in 1:length(iMulti)){
    if(is.integer0(which(DC1 == iMulti[i]))){
      SIMPAN_MULTI1[[i]] = matrix(c(0, iMulti[i]), ncol = 2)
      MASUK[i] = 0
      TENGAH = DC[[2]][DOUBLE[ke], 1]
      HELL[[i]] =  data.frame(0,TENGAH = iMulti[i], D2 = D2 , stringsAsFactors = F)
      
    }else{
      joz = (which(DC1 == iMulti[i]))
      print(joz)
      MASUK[i] = which(DC1 == iMulti[i])
      SIMPAN_MULTI1[[i]] = matrix(c(DC1[MASUK[i]], iMulti[i]), ncol = 2)
    }
  }
  SIMPAN_MULTI1 = data.frame(do.call("rbind", SIMPAN_MULTI1), stringsAsFactors = F)
  HELL = do.call("rbind", HELL)
  
  if(all(SIMPAN_MULTI1[,1] == 0)){
    TENGAH = DC[[2]][DOUBLE[ke], 1]
    # SIMP =  data.frame(0,TENGAH = TENGAH, D2 = D2 , stringsAsFactors = F)
    SIMP = HELL
    names(SIMP) = c(names(DC[[1]]), names(DC[[2]])[2])
  }else{
    D1 = D11[MASUK[MASUK != 0]]
    TENGAH = unique(unlist(SIMPAN_MULTI1[MASUK!=0, ]))
    SIMP =  data.frame(D1 =D1,TENGAH = TENGAH, D2 = D2 , stringsAsFactors = F)
    names(SIMP) = c(names(DC[[1]]), names(DC[[2]])[2])
    if(!is.null(HELL)){
      names(HELL) = names(SIMP)
      SIMP = rbind(SIMP, HELL)
    }
  }
  return(SIMP)
}
# MULTI CLUSTER CASE
DC1 = as.integer(DC[[1]][, 2])
D11 = DC[[1]][, 1]
DOUBLE = grep(DC[[2]][, 1], pattern = "&")
DC2 = DC[[2]][DOUBLE, 1]
D22 = DC[[2]][DOUBLE, 2]
CON_CC_DOUBLE = function(DC){
  DC1 = as.integer(DC[[1]][, 2])
  D11 = DC[[1]][, 1]
  
  DOUBLE = grep(DC[[2]][, 1], pattern = "&")
  DC2 = DC[[2]][DOUBLE, 1]
  D22 = DC[[2]][DOUBLE, 2]
  
  # FUNGSI PERSATUAN_MULTI_CLUSTER
  DOB = list()
  for(i in 1:length(DOUBLE)){
    DOB[[i]] = The_Multi(ke = i)
  }
  return(DOB)
}

DOB = CON_CC_DOUBLE(DC = DC)
SIN = CON_CC_SINGLE(DC = DC)
SINGL = list()
for(i in 1:length(SIN[,1])){
  SINGL[[i]] = SIN[i,]
}
ALL = c(DOB, SINGL)
IND = c()
for(i in 1:length(ALL)){
  names(ALL)[i] = paste0("SYSTEM_", ALL[[i]][1,3], "_",names(ALL[[i]])[3])
  IND[i] = ALL[[i]][1,3]
}
WK = data.frame(IND, ambil = 1:length(IND))
sort_system = WK[order(WK$IND), c(1, 2) ]
CONVEVTIVE_SYSTEM = ALL[sort_system[,2]]
TG = substr(strsplit(names(CONVEVTIVE_SYSTEM), split = "_")[[1]][5], 1, 12)
save(CONVEVTIVE_SYSTEM, file = paste0("data/SYSTEM/CS_",TG, ".bin"))