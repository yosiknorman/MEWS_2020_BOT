#!/usr/bin/Rscript
library(rgdal)
library(leaflet)
source("Coba_Ulang.R")
is.integer0 <- function(x){
  is.integer(x) && length(x) == 0L
}
is.numeric0 <- function(x){
  is.numeric(x) && length(x) == 0L
}
DT = CONVEVTIVE_SYSTEM$SYSTEM_2_Data_Cluster_202001012300.geojson
function(DT){
  
}
NM_DT = names(DT)
CL_sing = list()
CL_sing_selected = list()
GET_INFO = list()
for(i in 1:length(NM_DT)){
  CL_sing[[i]] = readOGR(paste0("data/Data_Cluster/",NM_DT[i]))
  iDT = paste(unique(DT[,i]), collapse = "&")
  kumpulan = iDT
  iDT = as.integer(unique(unlist(strsplit(iDT, split = "&")[[1]])))
  if(length(iDT) > 1){
    iDT = iDT[iDT != 0] 
  }else{
    # iDT[iDT == 0] = NULL
    if(iDT == 0){
      iDT = NULL
    }
  }
  CL_sing_selected[[i]] = CL_sing[[i]][iDT, ]
  GET_INFO[[i]] = cbind(CL_sing_selected[[i]]@data, No_CL = iDT)
  GET_INFO[[2]]
}
B1 = c(bbox(CL_sing_selected[[1]]))
B2 = c(bbox(CL_sing_selected[[2]]))
B3 = c(bbox(CL_sing_selected[[3]]))
if(is.numeric0(CL_sing_selected[[1]]$XPOS)){
  KOTAK = data.frame (rbind( B2, B3), stringsAsFactors = F)
}else{
  KOTAK = data.frame (rbind(B1, B2, B3), stringsAsFactors = F)
}

# KOTAK = data.frame (rbind(B1, B2, B3), stringsAsFactors = F)
names(KOTAK) = c("xmn", "ymn", "xmx", "ymx")
# load("~/Data_riset/kec2.Rda")
load("~/Data_riset/SHP_Prov.dat")


# plot( SHP_Prov, xlim = c(min(KOTAK$xmn)-1, max(KOTAK$xmx)+1), 
    # ylim = c(min(KOTAK$ymn)-1, max(KOTAK$ymx)+1) )

# plot(CL_sing_selected[[1]], add = T, col = "green")
# plot(CL_sing_selected[[2]], add = T, border = "yellow")
# plot(CL_sing_selected[[3]], add = T, border = "red")


leaflet() %>% addProviderTiles(providers$Esri) %>% 
  addPolygons(data = CL_sing_selected[[1]], color = "green" , weight = 0.5,fillOpacity = 0.3) %>% 
  addPolygons(data = CL_sing_selected[[2]], color = "yellow" , weight = 0.5,fillOpacity = 0.3) %>% 
  addPolygons(data = CL_sing_selected[[3]], color = "red" , weight = 0.5,fillOpacity = 0.7)