# max_dowy function testing on sagehen block

library(easypackages)
library(BiocManager)
library(rhdf5)
library(tidyverse)
library(rgeos)
library(raster)
library(rgdal)
library(parallel)
library(data.table)
library(gtools)


#######################################################
##### code for bringing block to test functions on
#######################################################

hdf_path <- "/Volumes/jt/projects/margulis/swe/hdf/" #create path
hdf_name <- "SN_SWE_WY1986.h5" #create file name
hdf_file <- paste(hdf_path, hdf_name, sep="") #full path
h5closeAll()
h5readAttributes(hdf_file, name = "SWE") #$units = mm

#read in a block around sagehen, pulled from QGIS using the cell number rasters generated
system.time(sagehen_86 <- h5read(hdf_file, "/SWE", index = list(2523:2621, 2947:3172, 1:365))) #sagehen
class(sagehen_86) #inspect 
dim(sagehen_86) #dimensions

z <-as.data.frame(sagehen_86[70,30,1:365])
names(z)[1] <- "swe_mm"

##### max_dowy function

max_dowy <-function(x){
  if (max(x) < 5.1){
    return(NA)
  } 
  else{
    max_swe<-as.numeric(max(x))
  dowy <-as.numeric(max(which(x == max_swe)))
  return(dowy)
}
}

max_dowy_raster <-function(x){
  
#### top half
top <- h5read(hdf_file, "/SWE", index = list(1:3300,1:5701,1:365))
top_max_dowy_mat <-as.matrix(apply(top, c(1,2), max_dowy))
rm(top)

#### bottomhalf half
bottom <- h5read(hdf_file, "/SWE", index = list(3301:6601,1:5701,1:365))
bottom_max_dowy_mat <-as.matrix(apply(bottom, c(1,2), max_dowy))
rm(bottom)

#bind chunks together
full <-rbind(top_max_dowy_mat, bottom_max_dowy_mat)
rast <-raster(full, xmn=-123.3, xmx=-117.6, ymn=35.4, ymx=42, CRS("+proj=leac +ellps=clrk66"))
plot(rast)
hist(rast)

name <- gsub(".h5", "", hdf_name)
good_name <- gsub("SN_SWE_", "max_dowy_", name)

setwd("/Volumes/jt/projects/margulis/snow_metric_rasters/max_dowy/rasters")
writeRaster(rast, paste0(good_name, ".tif"))
return(rast)
}
