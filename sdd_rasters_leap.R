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
hdf_path <- "/Volumes/jt/projects/margulis/swe/hdf/leap" #create path
h5closeAll()
h5readAttributes(hdf_file, name = "SWE") #$units = mm


# create snow cover frequency (scf) function
# number of days the amounts days there is snow on the ground vs total days
# set threshold to 5.1 mm or .2 inches because thats the accuracy a SNTOEL site has
# this will also reduce the long tails seen on the data and unrealistically large scf numbers i saw

sdd<-function(x){
  
  # return 0 for values that never reach the 5.1 mm threshold
  if (max(x) < 5.1){
    return(0)
  } else {
    # use which function to index the days that meet condition
    # reverse the vector
    days_vector <-as.numeric(rev(which(x > 5.1)))
    
    # take the difference of each vector point and make positive
    # we do this bc we are looking for the first point that meets condition
    # and does this for 14 consecutive days
    diff_vect <-diff(days_vector*-1)
    
    # use rle function to calc the lengths of continuous number sequences
    # if there isnt a 14 days with continuous snow cover, make pixel 0
    # this fixes the code bc accounts for pixels which may have had 10 days of sc
    
    # which therefore was out of the previous condition set and caused the error
    # found this about by testing different areas, and saw was more in east half
    result <- rle(diff_vect)
    if (max(result$lengths) < 14){
      return(0)
    } else{
      #represents place on rle vector where condition is met
      meets <-as.numeric(min(which(result$lengths >= 14)))
      
      #if its the first rle value, this means day 365 so max of days_vecto0r
      if (meets == 1) {
        sdd1 <- max(days_vector)
        return(sdd1)
      } else {
        #calculate legnth needed to be cut off days vector from rle
        values_to_cut <-result$lengths[1:(meets-1)]
        #sum the lengths
        index_values_until_start <-as.numeric(sum(values_to_cut))
        #subtract that many values off to get your SDD
        sdd2 <-max(days_vector[-c(1:index_values_until_start)])
        return(sdd2)
      }
    }
  }
}

sdd_raster <- function( hdf_name ) {
  
  path <-file.path( hdf_path , hdf_name ) 
  
  c1 <-h5read(path, "/SWE", index = list(1:500,1:5701,1:366)) #load in 
  sdd_c1 <-as.matrix(apply(c1, c(1,2), sdd)) #create matrix with sdd value on z axis
  rm(c1) 
  
  c2 <-h5read(path, "/SWE", index = list(501:1000,1:5701,1:366))
  sdd_c2 <-as.matrix(apply(c2, c(1,2), sdd))
  rm(c2)
  h5closeAll()
  
  c3 <-h5read(path, "/SWE", index = list(1001:1500,1:5701,1:366))
  sdd_c3 <-as.matrix(apply(c3, c(1,2), sdd))
  rm(c3)
  h5closeAll()
  
  c4 <-h5read(path, "/SWE", index = list(1501:2000,1:5701,1:366))
  sdd_c4 <-as.matrix(apply(c4, c(1,2), sdd))
  rm(c4)
  h5closeAll()
  
  c5 <-h5read(path, "/SWE", index = list(2001:2500,1:5701,1:366))
  sdd_c5 <-as.matrix(apply(c5, c(1,2), sdd))
  rm(c5)
  h5closeAll()
  
  c6 <-h5read(path, "/SWE", index = list(2501:3000,1:5701,1:366))
  sdd_c6 <-as.matrix(apply(c6, c(1,2), sdd))
  rm(c6)
  h5closeAll()
  
  c7 <-h5read(path, "/SWE", index = list(3001:3500,1:5701,1:366))
  sdd_c7 <-as.matrix(apply(c7, c(1,2), sdd))
  rm(c7)
  h5closeAll()
  
  c8 <-h5read(path, "/SWE", index = list(3501:4000,1:5701,1:366)) #load in 
  sdd_c8 <-as.matrix(apply(c8, c(1,2), sdd)) #creat matrix with sdd value on z axis
  rm(c8) 
  
  c9 <-h5read(path, "/SWE", index = list(4001:4500,1:5701,1:366)) #load in 
  sdd_c9 <-as.matrix(apply(c9, c(1,2), sdd)) #creat matrix with sdd value on z axis
  rm(c9) 
  
  c10 <-h5read(path, "/SWE", index = list(4501:5000,1:5701,1:366)) #load in 
  sdd_c10 <-as.matrix(apply(c10, c(1,2), sdd)) #creat matrix with sdd value on z axis
  rm(c10) 
  
  c11 <-h5read(path, "/SWE", index = list(5001:5500,1:5701,1:366)) #load in 
  sdd_c11 <-as.matrix(apply(c11, c(1,2), sdd)) #creat matrix with sdd value on z axis
  rm(c11) 
  
  c12 <-h5read(path, "/SWE", index = list(5501:6000,1:5701,1:366)) #load in 
  sdd_c12 <-as.matrix(apply(c12, c(1,2), sdd)) #creat matrix with sdd value on z axis
  rm(c12) 
  
  c13 <-h5read(path, "/SWE", index = list(6001:6601,1:5701,1:366)) #load in 
  sdd_c13 <-as.matrix(apply(c13, c(1,2), sdd)) #creat matrix with sdd value on z axis
  rm(c13) 
  
  
  
  
  #bind chunks together
  full_sdd <-rbind(sdd_c1,sdd_c2,sdd_c3,sdd_c4,sdd_c5,sdd_c6,sdd_c7,
                   sdd_c8,sdd_c9,sdd_c10,sdd_c11,sdd_c12,sdd_c13)
  rast <-raster(full_sdd, xmn=-123.3, xmx=-117.6, ymn=35.4, ymx=42, CRS("+proj=leac +ellps=clrk66"))
  plot(rast)
  hist(rast)
  
  name <- gsub(".h5", "", hdf_name)
  good_name <- gsub("SN_SWE_", "sdd_swe_", name)
  
  setwd("/Volumes/jt/projects/margulis/snow_metric_rasters/sdd")
  writeRaster(rast, paste0(good_name, ".tif"))
  return(rast)
}

#sdd_86<-sdd_raster("SN_SWE_WY1986.h5")

#### apply to hdf list

setwd("/Volumes/jt/projects/margulis/swe/hdf/leap")
files <- list.files(pattern = ".h5")
hdf_list <-mixedsort(sort(files)) #sort in correct order
print(hdf_list)

system.time(raster_list <-lapply(hdf_list, function(x) sdd_raster(x)))

warnings()
