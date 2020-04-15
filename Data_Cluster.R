#!/usr/bin/Rscript

library(uavRst)
library(raster)
library(geojsonio)
library(ncdf4)
library(EBImage)
rm(list = ls())


lsfd = list.files("data/nc/", pattern = "nc")
lsf = lsfd[length(lsfd)-0] # GANTI

sfx = strsplit(lsf, split = "_")[[1]][4]
sfx = gsub(sfx, pattern = ".nc", replacement = "")
R = raster(paste0("data/nc/", lsf))
nc = nc_open(paste0("data/nc/", lsf))
# x11()
# par(mfrow = c(2, 1))
# plot(R)
# image(VAR)
VAR = ncvar_get(nc, "IR")

VAR[VAR > 210]  = 0
lon = ncvar_get(nc, "longitude")
lat = ncvar_get(nc, "latitude")
# BLB[BLB > 210]  = 
BLB = bwlabel(VAR)
CF = computeFeatures.shape(BLB)
CF = data.frame(CF, stringsAsFactors = F)
CM = computeFeatures.moment(BLB)
CM = data.frame(CM, stringsAsFactors = F)

PX = CM$m.cx/length(lon)*(range(lon)[2]-range(lon)[1]); XPOS = PX+range(lon)[1]
PY = CM$m.cy/length(lat)*(range(lat)[2]-range(lat)[1]); YPOS = PY+range(lat)[1]
# length(lat)
# image(BLB)
# plot()

Thresshold_Area = 10
m = BLB 
A = CF$s.area
iB = which(A > Thresshold_Area)
k1 = list()
i_pol = list()
mat_poly = list()
coords = list()
for(i in 1:length(iB)){
  k1[[i]] = m
  k1[[i]][m  !=  iB[i]] = NA
  k2 = k1[[i]]
  k1[[i]] = i
  i_pol[[i]] = which(!is.na(k2),arr.ind = T)
  rm(k2)
  mat_poly[[i]] = cbind(lon[i_pol[[i]][,1]],lat[i_pol[[i]][,2]])
  colnames(mat_poly[[i]]) = c("x","y")
  hpts = chull(x = mat_poly[[i]][,1],y = mat_poly[[i]][,2])
  hpts <- c(hpts, hpts[1])
  coords[[i]] <- mat_poly[[i]][hpts,]
}

sp_poly  = list()
for(i in 1:length(coords)){
  sp_poly[[i]] <- Polygons(list(Polygon(coords[[i]])), ID= i )
}
Spoly=SpatialPolygons(sp_poly)
Data_Cluster=SpatialPolygonsDataFrame(Spoly, data.frame(row.names=paste0(1:length(coords)), Cloud_Cluster=1:length(coords), y=1:length(coords)))
crs(Data_Cluster) = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
# plot(Data_Cluster)
# system("mkdir data/Data_Cluster")
OUT = paste0("data/Data_Cluster/Data_Cluster_", sfx)
AREA = CF$s.area[CF$s.area > Thresshold_Area]
RADIUS_MINIMUM = CF$s.radius.min[CF$s.area > Thresshold_Area]
RADIUS_MAKSIMUM = CF$s.radius.max[CF$s.area > Thresshold_Area]
YPOS = YPOS[CF$s.area > Thresshold_Area]
XPOS = XPOS[CF$s.area > Thresshold_Area]
ISI_TABLE = data.frame(XPOS,YPOS,AREA, RADIUS_MINIMUM,RADIUS_MAKSIMUM,  stringsAsFactors = F)

Data_Cluster@data = ISI_TABLE
# poly_metrics(Data_Cluster)
hasil_per = poly_metrics(Data_Cluster)
Data_Cluster = poly_metrics(Data_Cluster)
# plot(Data_Cluster[hasil_per$elongation < 0.5,])
# plot(Data_Cluster)
geojson_write(Data_Cluster, file = OUT)
# plot(Data_Cluster)
# points(XPOS, YPOS, pch = ".")

