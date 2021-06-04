#calculation test against wresien data for both max and april_1

library(BiocManager)
library(rhdf5)
library(tidyverse)
library(rgeos)
library(rasterVis) #https://oscarperpinan.github.io/rastervis/#levelplot
library(rgdal)
library(raster)
library(data.table)
library(gtools)

#####################################################

###2014
#set path and file name for hdf5 SWE file
hdf_path <- "/Volumes/john_tarricone/UNR_summer_20/margulis/swe/hdf/" #create path
hdf_name <- "SN_SWE_WY2014.h5" #create file name
hdf_file <- paste(hdf_path, hdf_name, sep="") #full path
h5ls(hdf_file) #list contains 3 groups. lat, long, and SWE 
h5closeAll()
h5readAttributes(hdf_file, name = "SWE") #$units = mm

#read in days 1-100 of SWE data
swe_2014_183 <- h5read(hdf_file, "/SWE", index = list(NULL, NULL, 183)) #read in SWE group

###convert to rasters
#2014
swe_2014_183<-as.matrix(swe_2014_183[1:6601, 1:5701,])
swe_2014_183_rast <- raster(swe_2014_183, xmn=-123.3, xmx=-117.6, ymn=35.4, ymx=42, CRS("+proj=leac +ellps=clrk66")) #set lat lon cords
swe_2014_183_rast[ swe_2014_183_rast[] == -32768 ] <- NA #sets no data value to 0 using subseting, https://stackoverflow.com/questions/19680079/r-crop-no-data-of-a-raster
swe_2014_183_rast #inspect raster
image(swe_2014_183_rast) #print raster

###2009
#set path and file name for hdf5 SWE file
hdf_path2 <- "/Volumes/john_tarricone/UNR_summer_20/margulis/swe/hdf/" #create path
hdf_name2 <- "SN_SWE_WY2009.h5" #create file name
hdf_file2 <- paste(hdf_path2, hdf_name2, sep="") #full path
h5ls(hdf_file2) #list contains 3 groups. lat, long, and SWE 
h5closeAll()
h5readAttributes(hdf_file2, name = "SWE") #$units = mm

#read in days 1-100 of SWE data
swe_2009_183 <- h5read(hdf_file2, "/SWE", index = list(NULL, NULL, 183)) #read in SWE group

#2009
swe_2009_183<-as.matrix(swe_2009_183[1:6601, 1:5701,])
swe_2009_183_rast <- raster(swe_2009_183, xmn=-123.3, xmx=-117.6, ymn=35.4, ymx=42, CRS("+proj=leac +ellps=clrk66")) #set lat lon cords
swe_2009_183_rast[ swe_2009_183_rast[] == -32768 ] <- NA #sets no data value to 0 using subseting, https://stackoverflow.com/questions/19680079/r-crop-no-data-of-a-raster
swe_2009_183_rast #inspect raster
image(swe_2009_183_rast) #print raster

vol_swe_new(swe_2009_183_rast)


###2005
#set path and file name for hdf5 SWE file
hdf_path3 <- "/Volumes/john_tarricone/UNR_summer_20/margulis/swe/hdf/" #create path
hdf_name3 <- "SN_SWE_WY2005.h5" #create file name
hdf_file3 <- paste(hdf_path3, hdf_name3, sep="") #full path
h5ls(hdf_file3) #list contains 3 groups. lat, long, and SWE 
h5closeAll()
h5readAttributes(hdf_file3, name = "SWE") #$units = mm

#read in days 1-100 of SWE data
swe_2005_183 <- h5read(hdf_file3, "/SWE", index = list(NULL, NULL, 183)) #read in SWE group

#2005
swe_2005_183<-as.matrix(swe_2005_183[1:6601, 1:5701,])
swe_2005_183_rast <- raster(swe_2005_183, xmn=-123.3, xmx=-117.6, ymn=35.4, ymx=42, CRS("+proj=leac +ellps=clrk66")) #set lat lon cords
swe_2005_183_rast[ swe_2005_183_rast[] == -32768 ] <- NA #sets no data value to 0 using subseting, https://stackoverflow.com/questions/19680079/r-crop-no-data-of-a-raster
swe_2005_183_rast #inspect raster
image(swe_2005_183_rast) #print raster


vol_swe_new(swe_2005_183_rast)


year_max<-vol_swe_time_series %>% group_by(wy) %>%
  filter(swe_vol_km3 == max(swe_vol_km3)) 

april_1<-vol_swe_time_series %>% group_by(wy) %>%
  filter(doWY == 183) 


          