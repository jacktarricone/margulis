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


setwd("/Volumes/john_tarricone/UNR_summer_20/margulis/swe/daily_data/WY_2014")

# Define your file paths here.
hdf_path <- "/Volumes/john_tarricone/UNR_summer_20/margulis/swe/hdf" #create path

pixel_area <-raster("/Volumes/john_tarricone/UNR_summer_20/margulis/static/rasters/SNSR_pixel_area.tif")

pix_vol_swe <-function(a,b){return((a*1e-6)*(b*1e-6))}

vol_swe<-function(x){
  swe_by_pixel <- overlay(x, pixel_area, fun= pix_vol_swe )  
  vol_swe <-cellStats(swe_by_pixel,'sum', digits=9, na.rm=TRUE)
  print(vol_swe)
}


## create 8 functions which read in the hdfs, convert to brick, and then breack into raster list

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
    h5read(., "/SWE", index = list(NULL,NULL,331:365)) %>%
    brick(., xmn=-123.3, xmx=-117.6, ymn=35.4, ymx=42,CRS("+proj=leac +ellps=clrk66")) %>%
    as.list(.)
  
}



################################ big fun for all years



big_vol_fun<-function(x){
  
  name <- gsub(".h5", "", x)
  colname <- gsub("SN_SWE_", "", name)
  
  ## c1
  rast_list_c1<-open_hdf_as_raster_list_c1(x)
  system.time(results_c1<-mclapply(rast_list_c1, function(x) vol_swe(x), mc.cores = 3, mc.cleanup = TRUE))
  results_df_c1 <- data.frame(matrix(unlist(results_c1), nrow=length(results_c1), byrow=T))
  colnames(results_df_c1)[1] <- colname
  print(results_df_c1)
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
  
  results <- rbind(results_df_c1, results_df_c2, results_df_c3,
                   results_df_c4, results_df_c5, results_df_c6,
                   results_df_c7, results_df_c8)
  
  #stich data together and save
  doWY<-(1:365) #create vector
  doWY_df <- data.frame(matrix(unlist(doWY), nrow=length(doWY), byrow=T)) #convert to df
  colnames(doWY_df)[1] <- "doWY"
  
  #bind on doWY column
  full_year <-cbind(doWY_df, results)
  
  
  setwd("/Volumes/john_tarricone/UNR_summer_20/margulis/volume_results/")
  write.csv(full_year, paste0(name, ".csv"), row.names=FALSE)
  return(full_year)
  
}

##############################
setwd("/Volumes/john_tarricone/UNR_summer_20/margulis/swe/hdf")
files <- list.files(pattern = ".h5")
hdf_list <-mixedsort(sort(files)) #sort in correct order
print(hdf_list)

system.time(results_list <-mclapply(hdf_list, function(x) test_big_vol_fun(x), mc.cores = 3, mc.cleanup = TRUE))

projInfo(type = "proj")
