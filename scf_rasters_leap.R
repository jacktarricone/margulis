# nov 7th 2020
# make full sierra SCF rasters
# converted script from my orginal max one

library(BiocManager)
library(rhdf5)
library(tidyverse)
library(rgeos)
library(raster)
library(rgdal)
library(parallel)
library(data.table)
library(gtools)
library(terra)
library(spatialEco)
library(EnvStats)


# oct 12
# keeping data in a matrix instead of raster for processing
# this way we can avoid geolocation issues

tempdir <- function() { "/Volumes/jt/projects/margulis/temp" }
tempdir()

#set path and file name for hdf5 SWE file
hdf_path <- "/Volumes/jt/projects/margulis/swe/hdf" #create path
h5closeAll()
h5readAttributes(hdf_file, name = "SWE") #$units = mm


# create snow cover frequency (scf) function
# number of days the amounts days there is snow on the ground vs total days
# set threshold to 5.1 mm or .2 inches because thats the accuracy a SNTOEL site has
# this will also reduce the long tails seen on the data and unrealistically large scf numbers i saw

scf<-function(x){
  length(which(x > 5.1))/length(x)
  } 

scf_raster <- function( hdf_name ) {
  
  path <-file.path( hdf_path , hdf_name ) 
  
  c1 <-h5read(path, "/SWE", index = list(1:500,1:5701,1:365)) #load in 
  c1[ c1[] == -32768 ] <- NA #remove NA
  scf_c1 <-as.matrix(apply(c1, c(1,2), scf)) #create matrix with scf value on z axis
  rm(c1) 
  
  c2 <-h5read(path, "/SWE", index = list(501:1000,1:5701,1:365))
  c2[ c2[] == -32768 ] <- NA
  scf_c2 <-as.matrix(apply(c2, c(1,2), scf))
  rm(c2)
  h5closeAll()
  
  c3 <-h5read(path, "/SWE", index = list(1001:1500,1:5701,1:365))
  c3[ c3[] == -32768 ] <- NA
  scf_c3 <-as.matrix(apply(c3, c(1,2), scf))
  rm(c3)
  h5closeAll()
  
  c4 <-h5read(path, "/SWE", index = list(1501:2000,1:5701,1:365))
  c4[ c4[] == -32768 ] <- NA
  scf_c4 <-as.matrix(apply(c4, c(1,2), scf))
  rm(c4)
  h5closeAll()
  
  c5 <-h5read(path, "/SWE", index = list(2001:2500,1:5701,1:365))
  c5[ c5[] == -32768 ] <- NA
  scf_c5 <-as.matrix(apply(c5, c(1,2), scf))
  rm(c5)
  h5closeAll()
  
  c6 <-h5read(path, "/SWE", index = list(2501:3000,1:5701,1:365))
  c6[ c6[] == -32768 ] <- NA
  scf_c6 <-as.matrix(apply(c6, c(1,2), scf))
  rm(c6)
  h5closeAll()
  
  c7 <-h5read(path, "/SWE", index = list(3001:3500,1:5701,1:365))
  c7[ c7[] == -32768 ] <- NA
  scf_c7 <-as.matrix(apply(c7, c(1,2), scf))
  rm(c7)
  h5closeAll()
  
  c8 <-h5read(path, "/SWE", index = list(3501:4000,1:5701,1:365)) #load in 
  c8[ c8[] == -32768 ] <- NA #remove NA
  scf_c8 <-as.matrix(apply(c8, c(1,2), scf)) #creat matrix with scf value on z axis
  rm(c8) 
  
  c9 <-h5read(path, "/SWE", index = list(4001:4500,1:5701,1:365)) #load in 
  c9[ c9[] == -32768 ] <- NA #remove NA
  scf_c9 <-as.matrix(apply(c9, c(1,2), scf)) #creat matrix with scf value on z axis
  rm(c9) 
  
  c10 <-h5read(path, "/SWE", index = list(4501:5000,1:5701,1:365)) #load in 
  c10[ c10[] == -32768 ] <- NA #remove NA
  scf_c10 <-as.matrix(apply(c10, c(1,2), scf)) #creat matrix with scf value on z axis
  rm(c10) 
  
  c11 <-h5read(path, "/SWE", index = list(5001:5500,1:5701,1:365)) #load in 
  c11[ c11[] == -32768 ] <- NA #remove NA
  scf_c11 <-as.matrix(apply(c11, c(1,2), scf)) #creat matrix with scf value on z axis
  rm(c11) 
  
  c12 <-h5read(path, "/SWE", index = list(5501:6000,1:5701,1:365)) #load in 
  c12[ c12[] == -32768 ] <- NA #remove NA
  scf_c12 <-as.matrix(apply(c12, c(1,2), scf)) #creat matrix with scf value on z axis
  rm(c12) 
  
  c13 <-h5read(path, "/SWE", index = list(6001:6601,1:5701,1:365)) #load in 
  c13[ c13[] == -32768 ] <- NA #remove NA
  scf_c13 <-as.matrix(apply(c13, c(1,2), scf)) #creat matrix with scf value on z axis
  rm(c13) 
  
  
  
  
  #bind chunks together
  full_scf <-rbind(scf_c1,scf_c2,scf_c3,scf_c4,scf_c5,scf_c6,scf_c7,
                   scf_c8,scf_c9,scf_c10,scf_c11,scf_c12,scf_c13)
  rast <-raster(full_scf, xmn=-123.3, xmx=-117.6, ymn=35.4, ymx=42, CRS("+proj=leac +ellps=clrk66"))
  plot(rast)
  hist(rast)
  
  name <- gsub(".h5", "", hdf_name)
  good_name <- gsub("SN_SWE_", "scf_swe_", name)
  
  setwd("/Volumes/jt/projects/margulis/snow_metric_rasters/scf")
  writeRaster(rast, paste0(good_name, ".tif"))
  return(rast)
}

#### apply to hdf list

setwd("/Volumes/jt/projects/margulis/swe/hdf")
files <- list.files(pattern = ".h5")
hdf_list <-mixedsort(sort(files)) #sort in correct order
print(hdf_list)

system.time(raster_list <-lapply(hdf_list, function(x) scf_raster(x)))

warnings()
