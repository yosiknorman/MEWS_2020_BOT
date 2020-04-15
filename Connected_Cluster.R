#!/usr/bin/Rscript

library(rgdal)
library(rgeos)
library(doParallel)

rm(list = ls())

lsc = list.files("data/Data_Cluster/", pattern = ".geojson")
DN = lsc[length(lsc)-1] # GANTI
D0 = lsc[which(lsc == DN)-1]
Connected_Cluster = readOGR(paste0("data/Data_Cluster/",DN))
Last_Cluster = readOGR(paste0("data/Data_Cluster/",D0))
# plot(Last_Cluster[1:10,])
# plot(Connected_Cluster[1:10,], border = "blue", add = T)

# PARA = 1
CEK_CONNECTED_CLUSTER = function(PARA){
  Con_CL = list()
  for(i in 1:length(Last_Cluster)){
    INI = rgeos::gIntersection(Last_Cluster[i,], Connected_Cluster[PARA,])
    if(is.null(INI )){
      Con_CL[[i]] = c(0, PARA)
    }else{
      Con_CL[[i]] = c(i, PARA) 
    }
  }
  Con_CL = do.call("rbind", Con_CL )
  Con_CL = data.frame(Con_CL , stringsAsFactors = F)
  names(Con_CL) = c(D0, DN)
  Con_CL = Con_CL[Con_CL[,1] != 0,]
  return(Con_CL)
}
# rm(PARA)

# CONNECTED_CLUSTER = list()
# strt<-Sys.time()
# for(i in 1:length(Connected_Cluster)){
#   CONNECTED_CLUSTER [[i]]= CEK_CONNECTED_CLUSTER(PARA = i)
# }
# print(Sys.time()-strt)
cl <- makeCluster(4)   # how many worker ?
registerDoParallel(cl)
strt<-Sys.time()
CONNECTED_CLUSTER <- foreach(i=1:length(Connected_Cluster))  %dopar%
  CEK_CONNECTED_CLUSTER(PARA = i)
print(Sys.time()-strt)
stopCluster(cl)
CONNECTED_CLUSTER = do.call("rbind", CONNECTED_CLUSTER)
UCC = unique(CONNECTED_CLUSTER[,2])
A10MINBEFORE = c()
for(i in 1:length(UCC)){
  A10MINBEFORE[i] = paste(CONNECTED_CLUSTER[,1][which(CONNECTED_CLUSTER[,2] == UCC[i])], collapse = "&")
}

CON = data.frame( A10MINBEFORE, UCC , stringsAsFactors = F)
names(CON) = c(D0, DN)
# system("mkdir data/ConnectedCluster")
SFX = substr(DN, 14, nchar(DN)-8)
save(CON ,file = paste0("data/ConnectedCluster/ConnectedCluster_", SFX, ".bin"))