#scf function attempt

library(BiocManager)
library(rhdf5)
library(tidyverse)
library(rgeos)
library(rasterVis) #https://oscarperpinan.github.io/rastervis/#levelplot
library(rgdal)
library(raster)
library(data.table)
library(gtools)
library(rts)


setwd("/Volumes/john_tarricone/UNR_summer_20/margulis/swe/code_test")

#####################################################

#set path and file name for hdf5 SWE file
hdf_path <- "/Volumes/JTARRICONE/UNR_summer_20/margulis_data/swe/hdf/" #create path
hdf_name <- "SN_SWE_WY2016.h5" #create file name
hdf_file <- paste(hdf_path, hdf_name, sep="") #full path
h5ls(hdf_file) #list contains 3 groups. lat, long, and SWE 
h5closeAll()
h5readAttributes(hdf_file, name = "SWE") #$units = mm

#read in a [100*100*366] block in the middle of the sierra that HOPEfully has no NA values 
test_block2<- h5read(hdf_file, "/SWE", index = list(4243:4342, 4175:4274, 1:366)) #read in SWE group
class(test_block2) #inspect 
dim(test_block2) #dimensions

#create dailly rasters to be stacked
for (i in 1:366){
  mat<-as.matrix(test_block2[,,i])
  rast <-raster(mat, xmn=-123.3, xmx=-117.6, ymn=35.4, ymx=42, crs=CRS("+proj=leac +ellps=clrk66"))
  filename <- paste("",i)
  writeRaster(rast, filename, format='GTiff', overwrite=FALSE)
}

#list rasters in directory
yaht<-list.files("/Volumes/john_tarricone/UNR_summer_20/margulis/swe/code_test", pattern = ".tif")
rast_stack<-stack(yaht) #stack them
scf<-function(x){length(which(x==0))/length(x)} #function that computes yearly scf per cell


#https://gis.stackexchange.com/questions/222291/extracting-mean-of-multiple-raster-layers-using-r/222293
#mean test
mean_raster <- stackApply(rast_stack, indices = rep(1,nlayers(rast_stack)), fun = "mean", na.rm = FALSE)

#scf test
scf_raster <- stackApply(rast_stack, indices = rep(1,nlayers(rast_stack)), function(x,...){ length(which(x==0))/length(x)})
crs(scf_raster)<-"+proj=leac +ellps=clrk66"
crs(r) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
writeRaster(scf_raster, filename= "scf_test2.tif", format='GTiff')
