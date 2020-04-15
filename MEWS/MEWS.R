library(ncdf4)
library(raster)
library(shiny)
library(leaflet)
library(rgdal)
library(parallel)
library(doParallel)
library(stringr)


# setwd("MEWS/")
rm(list = ls())
is.character0 <- function(x){
  is.character(x) && length(x) == 0L
}

INPUT_WAKTU = TRUE
NAMA_WAKTU = "01-01-2020 03:00"
LSF = list.files("../data/nc/", pattern = "nc")
if(INPUT_WAKTU){
  PAT = paste0(substr(NAMA_WAKTU, 7, 10),
               substr(NAMA_WAKTU, 4, 5),
               substr(NAMA_WAKTU, 1, 2), 
               substr(NAMA_WAKTU, 12, 13),
               substr(NAMA_WAKTU, 15, 16))
   LAST = grep(LSF, pattern = PAT)
}else{
  LAST = length(LSF)
}
START = LAST-1


IR = raster(paste0("../data/nc/", LSF[LAST]))
IR_st = raster(paste0("../data/nc/", LSF[START]))
IR_int = IR - IR_st
# plot(IR_int)
load("~/Data_riset/kec2.Rda")
UPP = unique(kec@data$Provinsi)
kec = kec[-which(kec$Provinsi == UPP[length(UPP)]),]
UPP = UPP[-length(UPP)]
Per_Prov = function(iProv){
  Prov = UPP[iProv]
  kec_P = kec[kec$Provinsi %in% Prov,]
  BOX = raster()
  extent(BOX) = extent(kec_P)
  BOX@extent@xmin = BOX@extent@xmin-3
  BOX@extent@xmax = BOX@extent@xmax+3
  BOX@extent@ymin = BOX@extent@ymin-3
  BOX@extent@ymax = BOX@extent@ymax+3
  IR_P = crop(IR, BOX)
  IR_int_P = crop(IR_int, BOX)
  RES = list(  kec_P,  IR_P , IR_int_P)
  names(RES) = c(paste(UPP[1]), "IR" , "IR_int")
  return(RES)
}

iProv = 3
PreProc = Per_Prov(iProv)
EXIST = function(PARA){
  res = min(as.matrix(raster::mask(PreProc$IR, PreProc[[1]][PARA,])), na.rm = T)
  return(res)
}
# PreProc = Per_Prov(iProv)
EXIST_Penurunan = function(PARA){
  res = c(as.matrix(raster::mask(PreProc$IR_int, PreProc[[1]][PARA,])))
  res = res[!is.na(res)]
  res = (length(which(res < 0))/length(res))*100
  return(res)
}
Juml_Kec = PreProc[[1]]$Kabupaten
U_KB = as.character(unique(PreProc[[1]]$Kabupaten))
UTB_KB = table(as.character(PreProc[[1]]$Kabupaten))
Kab_DT =  data.frame( Kab = names(UTB_KB), Jumlah = as.numeric(UTB_KB), stringsAsFactors = F)
# for(i in 1:length())
  

# EXIST(7)
# EXIST_Kenaikan(7)
minimal_LAST = c()
Penurunan_T = c()
for(i in 1:length(PreProc[[1]])){
  minimal_LAST[i] = EXIST(i)
  Penurunan_T[i] = EXIST_Penurunan(i)
}
PreProc[[1]] = PreProc[[1]][-which(minimal_LAST == Inf),]
minimal_LAST = round(minimal_LAST[-which(minimal_LAST == Inf)], 1)
Penurunan_T = round(Penurunan_T[-which(is.na(Penurunan_T) )],1)
# which(minimal_LAST <= 210 & Penurunan_T < 0)
hasil = c()
for(i in 1:length(minimal_LAST)){
  if(minimal_LAST[i] <= 210){
    if(Penurunan_T[i] > 50){
      hasil[i] = minimal_LAST[i]
    }else{
      hasil[i] = paste0(minimal_LAST[i], " Naik ", Penurunan_T[i])
    }
  }else{
    hasil[i] = "No Warning"
  }
}

PreProc[[1]]@data = data.frame(PreProc[[1]]@data, WARNING = hasil)

INIT = hasil
# INIT_WARNING = 
if(any(INIT != "No Warning")){
  # Inti
  HL = INIT[-(which(INIT == "No Warning"))]
  HL_BU = HL
  HL = (HL[-grep(HL, pattern = " Naik ")])
  HL_Only = PreProc[[1]][which(PreProc[[1]]@data$WARNING %in% HL),]
  
  # HL_TRACK(HL_Only)
  # Meluas
  HL2 = (HL_BU[grep(HL_BU, pattern = " Naik ")])
  HL2_Only = PreProc[[1]][which(PreProc[[1]]@data$WARNING %in% HL2),]
  # HL1 = HL_TRACK(HL_Only)
  # HL_TRACK2(HL2_Only, Jangan = HL1[[2]])
  # Jangan =
  
  # HL_TRACK(HL_Only )
  # HL_TRACK2(HL2_Only , Jangan)
  HL_TRACK = function(HL_Only){
    JKec = as.character(HL_Only$Kecamatan)
    JKab = as.character(HL_Only$Kabupaten)
    JPrv = as.character(HL_Only$Provinsi)
    D1 = data.frame(Kab = as.character(names(table(JKab))), 
                    Jumlah = as.character((table(JKab))))
    DT_ALL = data.frame(JKec, JKab, JPrv, stringsAsFactors = F)
    D1 =  cbind(D1, Proporsi = as.numeric(D1$Jumlah) /  as.numeric(Kab_DT$Jumlah[Kab_DT$Kab %in% D1$Kab]))
    WARN_ALL_HL = ""
    if(any(D1$Proporsi > 0.5)){
      WARN_ALL_HL = paste(D1$Kab[D1$Proporsi > 0.5], collapse = ", ")
      Jangan = as.character(D1$Kab[D1$Proporsi > 0.5])
      # WARN_ALL_HL_BU = WARN_ALL_HL
      Kurang = which(D1$Proporsi <= 0.5)
      D1 = D1[Kurang, ]
      DT_ALL = DT_ALL[(DT_ALL$JKab %in% as.character(D1$Kab)), ]
      WARN_ALL_HL = gsub(WARN_ALL_HL, pattern = "KAB. ",replacement = "")
      WARN_ALL_HL = gsub(WARN_ALL_HL, pattern = "KOTA ",replacement = "")
      WARN_ALL_HL = str_to_title(WARN_ALL_HL)
      WARN_ALL_HL = paste0("Keseluruh wilayah *", WARN_ALL_HL, "* \n dan ")
    }
    
    KB_er = unique(DT_ALL$JKab)
    Text_Kec_HL = c()
    for(i in 1:length(KB_er)){
      Pil = DT_ALL[DT_ALL$JKab ==  KB_er[i], ]
      Text_Kec_HL[i] =  paste0("*" ,unique(Pil$JKab), "* : (", 
                               paste(unique(Pil$JKec), collapse = ", "), 
                               ")"
      ) 
    }
    Text_Kec_HL = paste(Text_Kec_HL, collapse = ", ")
    Text_Kec_HL = gsub(Text_Kec_HL, pattern = "KAB. ",replacement = "")
    Text_Kec_HL = gsub(Text_Kec_HL, pattern = "KOTA ",replacement = "")
    Text_Kec_HL = str_to_title(Text_Kec_HL)
    TEXT_HL = paste(WARN_ALL_HL, "beberapa wilayah seperti : \n ",
                    Text_Kec_HL )
    return(list(TEXT_HL, Jangan))
  }
  
  HL_TRACK2 = function(HL2_Only,   Jangan){
    # Jangan = HL_Only2[[2]]
    if(!is.character0(Jangan)){
      if(any(HL2_Only$Kabupaten %in% Jangan)){
        HL2_Only = HL2_Only[-(HL2_Only$Kabupaten %in% Jangan),] 
      }
    }
    JKec = as.character(HL2_Only$Kecamatan)
    JKab = as.character(HL2_Only$Kabupaten)
    JPrv = as.character(HL2_Only$Provinsi)
    D1 = data.frame(Kab = as.character(names(table(JKab))), 
                    Jumlah = as.character((table(JKab))))
    DT_ALL = data.frame(JKec, JKab, JPrv, stringsAsFactors = F)
    D1 =  cbind(D1, Proporsi = as.numeric(D1$Jumlah) /  as.numeric(Kab_DT$Jumlah[Kab_DT$Kab %in% D1$Kab]))
    WARN_ALL_HL = ""
    if(any(D1$Proporsi > 0.5)){
      WARN_ALL_HL = paste(D1$Kab[D1$Proporsi > 0.5], collapse = ", ")
      # Jangan = as.character(D1$Kab[D1$Proporsi > 0.5])
      # WARN_ALL_HL_BU = WARN_ALL_HL
      Kurang = which(D1$Proporsi <= 0.5)
      D1 = D1[Kurang, ]
      WARN_ALL_HL = gsub(WARN_ALL_HL, pattern = "KAB. ",replacement = "")
      WARN_ALL_HL = gsub(WARN_ALL_HL, pattern = "KOTA ",replacement = "")
      WARN_ALL_HL = str_to_title(WARN_ALL_HL)
      WARN_ALL_HL = paste0("Keseluruh wilayah *", WARN_ALL_HL, "* \n dan ")
    }
    DT_ALL = DT_ALL[(DT_ALL$JKab %in% as.character(D1$Kab)), ]
    
    KB_er = unique(DT_ALL$JKab)
    Text_Kec_HL = c()
    for(i in 1:length(KB_er)){
      Pil = DT_ALL[DT_ALL$JKab ==  KB_er[i], ]
      Text_Kec_HL[i] =  paste0("*" ,unique(Pil$JKab), "* : (", 
                               paste(unique(Pil$JKec), collapse = ", "), 
                               ") "
      ) 
      
    }
    Text_Kec_HL = paste(Text_Kec_HL, collapse = ", ")
    Text_Kec_HL = gsub(Text_Kec_HL, pattern = "KAB. ",replacement = "")
    Text_Kec_HL = gsub(Text_Kec_HL, pattern = "KOTA ",replacement = "")
    Text_Kec_HL = str_to_title(Text_Kec_HL)
    TEXT_HL = paste0(WARN_ALL_HL, "beberapa wilayah seperti : \n ",
                    Text_Kec_HL )
    return(TEXT_HL)
  }
  HL_Only2 = HL_TRACK(HL_Only)
  Meluas2 = HL_TRACK2(HL2_Only, Jangan = HL_Only2[[2]])
  ALL = paste0(HL_Only2[[1]], " \n dan dapat meluas ke ", Meluas2)
  # system("mkdir ../data/WARNING")
  PP = as.character(UPP[iProv])
  PP = gsub(PP, pattern = " ", replacement = "_")
  write(ALL, file = paste0("../data/WARNING/Warning_",   
                           substr(LSF[LAST], 1, nchar(LSF[LAST])-3), "_",PP, ".txt")
        
        )
  
}else{
  ALL = "No Warning"
}

# setwd("..")

# leaflet(PreProc[[1]])