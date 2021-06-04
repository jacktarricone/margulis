## testing using apply to iterate volume calculation
## need to look at virtual memory backup
## started 7/21/2020

library(BiocManager)
library(rhdf5)
library(tidyverse)
library(rgeos)
library(rasterVis) #https://oscarperpinan.github.io/rastervis/#levelplot
library(rgdal)
library(raster)
library(data.table)
library(gtools)
library(parallel)

#set working directory
setwd("/Volumes/john_tarricone/UNR_summer_20/margulis/swe/hdf/leap")

# define your file paths here.
hdf_path <- "/Volumes/john_tarricone/UNR_summer_20/margulis/swe/hdf/leap" #create path

files <- list.files(pattern = ".h5") #list files for all 24 non-leap years
hdf_list <-mixedsort(sort(files)) #sort in correct order
print(hdf_list) #test print

## raster temp file settings

rasterOptions(tmpdir="/Volumes/john_tarricone/UNR_summer_20/margulis/temp/")
tmpDir() #check location
removeTmpFiles(.5) #set to delete after 30 min

# bring in pixel area raster, which is in [m^2]
pixel_area <-raster("/Volumes/john_tarricone/UNR_summer_20/margulis/static/rasters/SNSR_pixel_area.tif")

# define the raster max you want to preform 
# in this case, convert both pixel area [m^2] and swe [mm^2]
# to [km^2] and [km], respectively by multiplying by 1e-6
pix_vol_swe <-function(a,b){return((a*1e-6)*(b*1e-6))}

# deine vol_swe function which overlays the rasters and performs to function we just define
# we now have a raster with swe in [km^3], generated using the correct pixel size
# next we sum all the pixels using cellstats to create daily volumetric swe value
vol_swe<-function(x){
  swe_by_pixel <- overlay(x, pixel_area, fun= pix_vol_swe )  
  vol_swe <-cellStats(swe_by_pixel,'sum', digits=9, na.rm=TRUE)
  print(vol_swe)
}

# create 8 functions which read in the hdfs, convert to brick, and then breack into raster list
# it has to be done in chunks bc of how large the files are and how much RAM your CPU has
# this could be done in less steps the more RAM you have on your CPU


# i think you could iterate this but i'm just not sure how do it

open_hdf_as_raster_list_c1 <- function( hdf_name ) {
  
  hdf_name %>%
    file.path( hdf_path , . ) %>%
    h5read(., "/SWE", index = list(NULL,NULL,1:50)) %>%
    brick(., xmn=-123.3, xmx=-117.6, ymn=35.4, ymx=42,CRS("+proj=leac +ellps=clrk66")) %>%
    as.list(.)
  
}

open_hdf_as_raster_list_c2 <- function( hdf_name ) {
  
  hdf_name %>%
    file.path( hdf_path , . ) %>%
    h5read(., "/SWE", index = list(NULL,NULL,51:100)) %>%
    brick(., xmn=-123.3, xmx=-117.6, ymn=35.4, ymx=42,CRS("+proj=leac +ellps=clrk66")) %>%
    as.list(.)
  
}

open_hdf_as_raster_list_c3 <- function( hdf_name ) {
  
  hdf_name %>%
    file.path( hdf_path , . ) %>%
    h5read(., "/SWE", index = list(NULL,NULL,101:150)) %>%
    brick(., xmn=-123.3, xmx=-117.6, ymn=35.4, ymx=42,CRS("+proj=leac +ellps=clrk66")) %>%
    as.list(.)
  
}

open_hdf_as_raster_list_c4 <- function( hdf_name ) {
  
  hdf_name %>%
    file.path( hdf_path , . ) %>%
    h5read(., "/SWE", index = list(NULL,NULL,151:200)) %>%
    brick(., xmn=-123.3, xmx=-117.6, ymn=35.4, ymx=42,CRS("+proj=leac +ellps=clrk66")) %>%
    as.list(.)
  
}

open_hdf_as_raster_list_c5 <- function( hdf_name ) {
  
  hdf_name %>%
    file.path( hdf_path , . ) %>%
    h5read(., "/SWE", index = list(NULL,NULL,201:250)) %>%
    brick(., xmn=-123.3, xmx=-117.6, ymn=35.4, ymx=42,CRS("+proj=leac +ellps=clrk66")) %>%
    as.list(.)
  
}


open_hdf_as_raster_list_c6 <- function( hdf_name ) {
  
  hdf_name %>%
    file.path( hdf_path , . ) %>%
    h5read(., "/SWE", index = list(NULL,NULL,251:300)) %>%
    brick(., xmn=-123.3, xmx=-117.6, ymn=35.4, ymx=42,CRS("+proj=leac +ellps=clrk66")) %>%
    as.list(.)
  
}

open_hdf_as_raster_list_c7 <- function( hdf_name ) {
  
  hdf_name %>%
    file.path( hdf_path , . ) %>%
    h5read(., "/SWE", index = list(NULL,NULL,301:330)) %>%
    brick(., xmn=-123.3, xmx=-117.6, ymn=35.4, ymx=42,CRS("+proj=leac +ellps=clrk66")) %>%
    as.list(.)
  
}

open_hdf_as_raster_list_c8 <- function( hdf_name ) {
  
  hdf_name %>%
    file.path( hdf_path , . ) %>%
    h5read(., "/SWE", index = list(NULL,NULL,331:366)) %>%
    brick(., xmn=-123.3, xmx=-117.6, ymn=35.4, ymx=42,CRS("+proj=leac +ellps=clrk66")) %>%
    as.list(.)
  
}



################################ big func for all years

# we next create a function that uses the 8 chunking functions we just made
# for each chunk it 

big_vol_fun<-function(x){
  
  name <- gsub(".h5", "", x) #file name without .h5 for naming the the future
  colname <- gsub("SN_SWE_", "", name) #just name WYXXX for col name in df
  
  ## c1
  rast_list_c1<-open_hdf_as_raster_list_c1(x) #open hdf has raster list
  system.time(results_c1<-mclapply(rast_list_c1, function(x) vol_swe(x), mc.cores = 3, mc.cleanup = TRUE)) #apply vol_swe to raster list
  results_df_c1 <- data.frame(matrix(unlist(results_c1), nrow=length(results_c1), byrow=T)) #create df with results
  colnames(results_df_c1)[1] <- colname #rename col with correct WY
  print(results_df_c1) #df for this chunk of results
  rm(rast_list_c1)
  rm(results_c1)
  h5closeAll()
  
  ## c2
  rast_list_c2<-open_hdf_as_raster_list_c2(x)
  system.time(results_c2<-mclapply(rast_list_c2, function(x) vol_swe(x), mc.cores = 3, mc.cleanup = TRUE))
  results_df_c2 <- data.frame(matrix(unlist(results_c2), nrow=length(results_c2), byrow=T))
  colnames(results_df_c2)[1] <- colname  
  print(results_df_c2)
  rm(rast_list_c2)
  rm(results_c2)
  h5closeAll()
  
  ## c3
  rast_list_c3<-open_hdf_as_raster_list_c3(x)
  system.time(results_c3<-mclapply(rast_list_c3, function(x) vol_swe(x), mc.cores = 3, mc.cleanup = TRUE))
  results_df_c3 <- data.frame(matrix(unlist(results_c3), nrow=length(results_c3), byrow=T))
  colnames(results_df_c3)[1] <- colname 
  print(results_df_c3)
  rm(rast_list_c3)
  rm(results_c3)
  h5closeAll()
  
  ## c4
  rast_list_c4<-open_hdf_as_raster_list_c4(x)
  system.time(results_c4<-mclapply(rast_list_c4, function(x) vol_swe(x), mc.cores = 3, mc.cleanup = TRUE))
  results_df_c4 <- data.frame(matrix(unlist(results_c4), nrow=length(results_c4), byrow=T))
  colnames(results_df_c4)[1] <- colname 
  print(results_df_c4)
  rm(rast_list_c4)
  rm(results_c4)
  h5closeAll()
  
  ## c5
  rast_list_c5<-open_hdf_as_raster_list_c5(x)
  system.time(results_c5<-mclapply(rast_list_c5, function(x) vol_swe(x), mc.cores = 3, mc.cleanup = TRUE))
  results_df_c5 <- data.frame(matrix(unlist(results_c5), nrow=length(results_c5), byrow=T))
  colnames(results_df_c5)[1] <- colname 
  print(results_df_c5)
  rm(rast_list_c5)
  rm(results_c5)
  h5closeAll()
  
  ## c6
  rast_list_c6<-open_hdf_as_raster_list_c6(x)
  system.time(results_c6<-mclapply(rast_list_c6, function(x) vol_swe(x), mc.cores = 3, mc.cleanup = TRUE))
  results_df_c6 <- data.frame(matrix(unlist(results_c6), nrow=length(results_c6), byrow=T))
  colnames(results_df_c6)[1] <- colname 
  print(results_df_c6)
  rm(rast_list_c6)
  rm(results_c6)
  h5closeAll()
  
  ## c7
  rast_list_c7<-open_hdf_as_raster_list_c7(x)
  system.time(results_c7<-mclapply(rast_list_c7, function(x) vol_swe(x), mc.cores = 3, mc.cleanup = TRUE))
  results_df_c7 <- data.frame(matrix(unlist(results_c7), nrow=length(results_c7), byrow=T))
  colnames(results_df_c7)[1] <- colname 
  print(results_df_c7)
  rm(rast_list_c7)
  rm(results_c7)
  h5closeAll()
  
  ## c8
  rast_list_c8<-open_hdf_as_raster_list_c8(x)
  system.time(results_c8<-mclapply(rast_list_c8, function(x) vol_swe(x), mc.cores = 3, mc.cleanup = TRUE))
  results_df_c8 <- data.frame(matrix(unlist(results_c8), nrow=length(results_c8), byrow=T))
  colnames(results_df_c8)[1] <- colname 
  print(results_df_c8)
  rm(rast_list_c8)
  rm(results_c8)
  h5closeAll()
  
  
  results <- rbind(results_df_c1, results_df_c2, results_df_c3,
                   results_df_c4, results_df_c5, results_df_c6,
                   results_df_c7, results_df_c8)
  
  #stich data together and save
  doWY<-(1:366) #create vector
  doWY_df <- data.frame(matrix(unlist(doWY), nrow=length(doWY), byrow=T)) #convert to df
  colnames(doWY_df)[1] <- "doWY"
  
  #bind on doWY column
  full_year <-cbind(doWY_df, results)
  
  
  setwd("/Volumes/john_tarricone/UNR_summer_20/margulis/volume_results/")
  write.csv(full_year, paste0(name, ".csv"), row.names=FALSE)
  return(full_year)
  
}


####### run the new function on the list of hdf files we previusly defined



system.time(results_list <-lapply(hdf_list, function(x) big_vol_fun(x)))
