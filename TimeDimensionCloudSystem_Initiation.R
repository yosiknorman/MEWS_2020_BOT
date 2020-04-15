# Time-Dimension
library(rgdal)

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
THIS = paste0("data/ConnectedCluster/", "ConnectedCluster_",THIS,".bin")

CON_ALL = list()
for(i in 1:length(THIS)){
  load(THIS[i])
  CON_ALL[[i]] = CON
}
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
    ID_OUT = as.matrix(ID_OUT, ncol = length(ID_OUT))
  }else{
    ID_OUT = as.character(ID_OUT[ID_OUT != 0])
    MAN = which(ID_OUT == "NEXT" )[1]
    ID_OUT = matrix(ID_OUT, ncol = MAN, byrow = T)
    ID_OUT = ID_OUT[, 1:MAN-1]
  }
  return(ID_OUT)
}
LS_THIS = c(names(CON_ALL[[1]]), names(CON_ALL[[2]][2]))
CON = CON_ALL[[2]]
CON1 = CON_ALL[[1]]
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
      ID_OUT[[i]] =   c("MULAI", CON[i,c(1)], CON[i,c(2)])
    }
  }else{
    PAS   = CON1[which(CON1[, 2] == CON[i,1]), 1]
    ID_OUT[[i]] =   c(PAS, "NYAMBUNG-KE", CON[i,1], "PADA_DATA", CON[i,c(1)], CON[i,c(2)])
  }
  LEN_IDOUT[i] = length(ID_OUT[[i]])
  CL_LAST[i] = ID_OUT[[i]][length(ID_OUT[[i]])]
}
UCL_ = unique(CL_LAST)

UPILIH = 1
if(is.null(dim(ID_OUT[UPILIH][[1]]))){
  print("satu row")
  INIXY = matrix(ID_OUT[UPILIH][[1]], nrow = 1)
}else{
  INIXY = ID_OUT[UPILIH][[1]]
}

SPLITMERGE =which(as.numeric(LEN_IDOUT) > 6 )
LINEAR =which(as.numeric(LEN_IDOUT) == 6 )
SPLITMERGE_CL = ID_OUT[SPLITMERGE]
LINEAR_CL = ID_OUT[LINEAR]




# LS_THIS = list.files("data/Data_Cluster/")
# JS1 = readOGR(paste0("data/Data_Cluster/", LS_THIS[1]))
# JS2 = readOGR(paste0("data/Data_Cluster/", LS_THIS[2]))
# JS3 = readOGR(paste0("data/Data_Cluster/", LS_THIS[3]))
# library(maps)
# library(ncdf4)
# NC1= nc_open("data/nc/H08_B13_Indonesia_202001020000.nc")
# lon = ncvar_get(NC1, "longitude")
# lat = ncvar_get(NC1, "latitude")
# VAR =  ncvar_get(NC1, "IR")
# image(lon, lat, VAR,  xlim = c(109, 120), ylim = c(-15, -6))

# plot(JS3[U3,], add = T, col = rgb(0.2,0.2, 0.7 , alpha = 0.4))

# plot(JS3[U3,], col = rgb(0.2,0.2, 0.7 , alpha = 0.4))
# map(add = T)
# plot(JS2[as.numeric(unique(INIXY[,3])),], add = T, col = rgb(0.2,0.7, 0.2 , alpha = 0.4))
# plot(JS1[as.numeric(unique(INIXY[,6])),], add = T, col = rgb(0.7,0.2, 0.2 , alpha = 0.4))

# plot(JS1[unique(INIXY[,6]),])
# 
# points(JS3$XPOS[U3], 
#        JS3$YPOS[U3], 
#        col = rgb(0.2,0.2, 0.7 , alpha = 0.9))
# 
# points(JS2$XPOS[as.numeric(unique(INIXY[,3]))], 
#        JS2$YPOS[as.numeric(unique(INIXY[,3]))], 
#        col = rgb(0.2,0.7, 0.2 , alpha = 0.9))
# 
# points(JS1$XPOS[as.numeric(unique(INIXY[,6]))], 
#        JS1$YPOS[as.numeric(unique(INIXY[,6]))], 
#        col = rgb(0.7,0.2, 0.2 , alpha = 0.9))
# 
# 
# text(JS3$XPOS[U3], 
#      JS3$YPOS[U3],
#      labels = "T-2Jam", cex = 0.5 )
# text(JS2$XPOS[as.numeric(unique(INIXY[,3]))]-0.7, 
#      JS2$YPOS[as.numeric(unique(INIXY[,3]))], 
#      labels = "T-1Jam", cex = 0.5)
# text(JS1$XPOS[as.numeric(unique(INIXY[,6]))], 
#      JS1$YPOS[as.numeric(unique(INIXY[,6]))]-0.3, 
#      labels = "T-0Jam", cex = 0.5)
# 
# 
# 
# plot(JS3[U3,], col = rgb(0.2,0.2, 0.7 , alpha = 0.4))