# Time-Dimension
library(rgdal)
library(ncdf4)
library(maps)


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
# load(THIS[7])
# CON1 = CON
# load(THIS[length(THIS)])
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

CON = CON_ALL[[7]] 
CON1 = CON_ALL[[6]]  
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
LEN_IDOUT = c()
CL_LAST = c()
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
  LEN_IDOUT[i] = length(ID_OUT[[i]])
  CL_LAST[i] = ID_OUT[[i]][length(ID_OUT[[i]])]
  if(LEN_IDOUT[i] == 5){
    ID_OUT[i][[1]][1] = "MULAI"
  }
}
UCL_ = unique(CL_LAST)



# table(CL_LAST)
# for(i in 1:length(UCL_)){
#   ID_OUT[CL_LAST == UCL_[]]
# }
# 

# LEN_IDOUT[102]
UPILIH = 2
if(is.null(dim(ID_OUT[UPILIH][[1]]))){
  print("satu row")
  INIXY = matrix(ID_OUT[UPILIH][[1]], nrow = 1)
}else{
  INIXY = ID_OUT[UPILIH][[1]]
}



JS1 = readOGR("data/Data_Cluster/Data_Cluster_202001020000.geojson")
JS2 = readOGR("data/Data_Cluster/Data_Cluster_202001012300.geojson")
JS3 = readOGR("data/Data_Cluster/Data_Cluster_202001012200.geojson")
library(maps)
library(ncdf4)
NC1= nc_open("data/nc/H08_B13_Indonesia_202001020000.nc")
lon = ncvar_get(NC1, "longitude")
lat = ncvar_get(NC1, "latitude")
VAR =  ncvar_get(NC1, "IR")
image(lon, lat, VAR
      ,
      xlim = c(109, 120), ylim = c(-15, -6)
      )
# plot(JS1, col = "white", border = "white", )
map( 
  # xlim = c(109, 120), ylim = c(-15, -6), 
  add = T)
plot(JS3[unique(INIXY[,1]),], add = T, col = rgb(0.2,0.2, 0.7 , alpha = 0.4))
plot(JS2[unique(INIXY[,3]),], add = T, col = rgb(0.2,0.7, 0.2 , alpha = 0.4))
plot(JS1[unique(INIXY[,6]),], add = T, col = rgb(0.7,0.2, 0.2 , alpha = 0.4))

points(JS2$XPOS[as.numeric(unique(INIXY[,3]))], 
       JS2$YPOS[as.numeric(unique(INIXY[,3]))], 
       col = rgb(0.2,0.7, 0.2 , alpha = 0.9))
points(JS3$XPOS[as.numeric(unique(INIXY[,1]))], 
       JS3$YPOS[as.numeric(unique(INIXY[,1]))], 
       col = rgb(0.2,0.2, 0.7 , alpha = 0.9))
points(JS1$XPOS[as.numeric(unique(INIXY[,6]))], 
       JS1$YPOS[as.numeric(unique(INIXY[,6]))], 
       col = rgb(0.7,0.2, 0.2 , alpha = 0.9))


text(JS3$XPOS[as.numeric(unique(INIXY[,1]))], 
       JS3$YPOS[as.numeric(unique(INIXY[,1]))],
     labels = "T-2Jam", cex = 0.5 )
text(JS2$XPOS[as.numeric(unique(INIXY[,3]))]-0.7, 
       JS2$YPOS[as.numeric(unique(INIXY[,3]))], 
       labels = "T-1Jam", cex = 0.5)
text(JS1$XPOS[as.numeric(unique(INIXY[,6]))], 
       JS1$YPOS[as.numeric(unique(INIXY[,6]))]-0.3, 
       labels = "T-0Jam", cex = 0.5)

  
  