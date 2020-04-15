# Time-Dimension
# library()

rm(list = ls())
is.integer0 <- function(x){
  is.integer(x) && length(x) == 0L
}
is.character0 <- function(x){
  is.character(x) && length(x) == 0L
}
BATAS_EVENT = as.numeric("7") # JAM
DT_CASE = "02-01-2020 00:00" # PILIH TANGGAL WAKTU
Kanal = "13"
print(paste0("Analisis Untuk Tanggal : ", DT_CASE, " UTC"))
DMYHH = strsplit(DT_CASE, split = " ")[[1]]
DMY = strsplit(DMYHH[1], split = "-")[[1]]
Y = DMY[3] ; M = DMY[2] ; D = DMY[1];
HH   = strsplit(DMYHH[2], split = ":")[[1]]
H = HH[1] ; Min = HH[2]

if(DMYHH[2] == "00:00"){
  DC = as.POSIXct(paste0(paste(rev(DMY), collapse = "-"), " ", H,":", Min))+1
}else{
  DC = as.POSIXct(paste0(paste(rev(DMY), collapse = "-"), " ", H,":", Min))
}
DC0 = DC - (24*60*60)
DC1 = DC 
DCALL = as.character(seq(DC0, DC1, by = "hour"))
DCALL = DCALL[(length(DCALL)-BATAS_EVENT):length(DCALL)]
THIS = substr(gsub(gsub(gsub(DCALL, pattern = "-", replacement = ""),
                        pattern = ":", replacement = ""   ),
                   pattern = " ", replacement = ""), 1 ,12)
# THIS = paste0(paste(rev(DMY), collapse = ""), paste(HH, collapse = ""))
THIS = paste0("data/ConnectedCluster/", "ConnectedCluster_",THIS,".bin")

# THIS = paste0(paste(rev(DMY), collapse = ""), paste(HH, collapse = ""))
# THIS = paste0("data/ConnectedCluster/", "ConnectedCluster_",THIS,".bin")
CON_ALL = list()
for(i in 1:length(THIS)){
  load(THIS[i])
  CON_ALL[[i]] = CON
}
# dim(CON_ALL[[7]])
# CON_ALL[[1]] 
load(THIS[7])
CON1 = CON
load(THIS[length(THIS)])
# cek_
# for(i in 1:length(CON1[,1])){
#   
#   which(CON1[,2] == CON[1,1] )
# }



# SPLIT = list()
# for(i in 1:length(CON[,1])){
#   ADA =grep(CON[i,1], pattern = "&")  
#   if(is.integer0(ADA)){
#     SPLIT[[i]] = CON[i,1]
#   }else{
#     SPLIT[[i]] = strsplit(CON[i,1], split = "&")[[1]]
#   }
# }

# ADA =grep(CON[,1], pattern = "&")  
# GGG = CON1[which(as.character(CON1[,2]) == SPLIT[[ADA[1]]][1]),]
# c(as.character(GGG), CON[ADA[1],2])

# CASE 1 
# POSSIBLE INPUT IS ar = , CON = WAKTU TERAKHIR (ID_OUT), CON1 = WAKTU_TERAKHIR -1

TheAll_Connection = function(ar, CON, CON1){
  # ar = 12
  IN = unlist(strsplit(CON[ar,1], split = "&"))
  
  ID_OUT = list()
  for(i in 1:length(IN)){
    ID_OUT[[i]] = which(CON1[,2] == IN[i])
    if(is.integer0(ID_OUT[[i]])){
      ID_OUT[[i]] = 0
      # ID_OUT[[i]] = paste0(IN[i], "PADA_DATA", CON[ar,], "NEXT")
    }else{
      ID_OUT[[i]] = CON1[unlist(ID_OUT[[i]]),1]
      ID_OUT[[i]] = c(ID_OUT[[i]], "NYAMBUNG-KE", IN[i], "PADA_DATA", CON[ar,], "NEXT")
    }
  }
  
  ID_OUT = unlist(ID_OUT)
  if(all(ID_OUT == 0)){
    ID_OUT = ID_OUT
  }else{
    ID_OUT = as.character(ID_OUT[ID_OUT != 0])
    MAN = which(ID_OUT == "NEXT" )[1]
    ID_OUT = matrix(ID_OUT, ncol = MAN, byrow = T)
    ID_OUT = ID_OUT[, 1:MAN-1]
  }
  
  return(ID_OUT)
}

# function(CON, CON1){
#   
# }
DOUBLE = grep(CON[,1] , pattern = "&" )
ID_OUT = list()

SEMUA_0 = c()
for(i in 1:length(CON[,1])){
  if(any(DOUBLE == i ) ){
    ID_OUT[[i]] = TheAll_Connection(ar = i, CON = CON, CON1 = CON1 )
    if(all(ID_OUT[[i]] == 0)){
      # SEMUA_0[i] = TRUE
      ID_OUT[[i]] =   c("HABIS", CON[i,c(1)], CON[i,c(2)])
    }
  }else{
    PAS   = CON1[which(CON1[, 2] == CON[i,1]), 1]
    ID_OUT[[i]] =   c(PAS, "NYAMBUNG-KE", CON[i,1], "PADA_DATA", CON[i,c(1)], CON[i,c(2)])
  }
}

# SEMUA_0 = c()
# for(i in 1:length(ID_OUT)){
#   if(all(ID_OUT[[i]] == 0)){
#     SEMUA_0[i] = TRUE
#   }else{
#     SEMUA_0[i] = FALSE
#   }
# }

# ID_OUT = CON
# ADA = grep(ID_OUT[,1], pattern = "&")
# if(is.integer0(ADA)){
#   
# }else{
#   ID_OUT = TheAll_Connection(ADA[3], ID_OUT, CON1)
# }
