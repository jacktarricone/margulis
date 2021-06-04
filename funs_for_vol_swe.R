### funtions for vol swe analysis 

pixel_area <-raster("/Volumes/john_tarricone/UNR_summer_20/margulis/static/rasters/SNSR_pixel_area.tif")

pix_vol_swe <-function(a,b){return((a*1e-6)*(b*1e-6))}

vol_swe<-function(x){
  swe_by_pixel <- overlay(x, pixel_area, fun= pix_vol_swe )  
  vol_swe <-cellStats(swe_by_pixel,'sum', digits=9, na.rm=TRUE)
  print(vol_swe)
}

# seads filename & explicitly opens it as raster list
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

big_vol_func <-function(x){
  
  ## c1
  rast_list_c1<-open_hdf_as_raster_list_c1("SN_SWE_WY2014.h5")
  system.time(results_c1<-mclapply(rast_list_c1, function(x) vol_swe(x), mc.cores = 3, mc.cleanup = TRUE))
  results_df_c1 <- data.frame(matrix(unlist(results_c1), nrow=length(results_c1), byrow=T))
  colnames(results_df_c1)[1] <- "WY_2014"  
  rm(rast_list_c1)
  
  ## c2
  rast_list_c2<-open_hdf_as_raster_list_c2("SN_SWE_WY2014.h5")
  system.time(results_c2<-mclapply(rast_list_c2, function(x) vol_swe(x), mc.cores = 3, mc.cleanup = TRUE))
  results_df_c2 <- data.frame(matrix(unlist(results_c2), nrow=length(results_c2), byrow=T))
  colnames(results_df_c2)[1] <- "WY_2014"  
  rm(rast_list_c2)
  
  ## c3
  rast_list_c3<-open_hdf_as_raster_list_c3("SN_SWE_WY2014.h5")
  system.time(results_c3<-mclapply(rast_list_c3, function(x) vol_swe(x), mc.cores = 3, mc.cleanup = TRUE))
  results_df_c3 <- data.frame(matrix(unlist(results_c3), nrow=length(results_c3), byrow=T))
  colnames(results_df_c3)[1] <- "WY_2014"  
  rm(rast_list_c3)
  
  ## c4
  rast_list_c4<-open_hdf_as_raster_list_c4("SN_SWE_WY2014.h5")
  system.time(results_c4<-mclapply(rast_list_c4, function(x) vol_swe(x), mc.cores = 3, mc.cleanup = TRUE))
  results_df_c4 <- data.frame(matrix(unlist(results_c4), nrow=length(results_c4), byrow=T))
  colnames(results_df_c4)[1] <- "WY_2014"  
  rm(rast_list_c4)
  
  ## c5
  rast_list_c5<-open_hdf_as_raster_list_c5("SN_SWE_WY2014.h5")
  system.time(results_c5<-mclapply(rast_list_c5, function(x) vol_swe(x), mc.cores = 3, mc.cleanup = TRUE))
  results_df_c5 <- data.frame(matrix(unlist(results_c5), nrow=length(results_c5), byrow=T))
  colnames(results_df_c5)[1] <- "WY_2014"  
  rm(rast_list_c5)
  
  ## c6
  rast_list_c6<-open_hdf_as_raster_list_c6("SN_SWE_WY2014.h5")
  system.time(results_c6<-mclapply(rast_list_c6, function(x) vol_swe(x), mc.cores = 3, mc.cleanup = TRUE))
  results_df_c6 <- data.frame(matrix(unlist(results_c6), nrow=length(results_c6), byrow=T))
  colnames(results_df_c6)[1] <- "WY_2014"  
  rm(rast_list_c6)
  
  ## c7
  rast_list_c7<-open_hdf_as_raster_list_c7("SN_SWE_WY2014.h5")
  system.time(results_c7<-mclapply(rast_list_c7, function(x) vol_swe(x), mc.cores = 3, mc.cleanup = TRUE))
  results_df_c7 <- data.frame(matrix(unlist(results_c7), nrow=length(results_c7), byrow=T))
  colnames(results_df_c7)[1] <- "WY_2014"  
  rm(rast_list_c7)
  
  ## c8
  rast_list_c8<-open_hdf_as_raster_list_c8("SN_SWE_WY2014.h5")
  system.time(results_c8<-mclapply(rast_list_c8, function(x) vol_swe(x), mc.cores = 3, mc.cleanup = TRUE))
  results_df_c8 <- data.frame(matrix(unlist(results_c8), nrow=length(results_c8), byrow=T))
  colnames(results_df_c8)[1] <- "WY_2014"  
  rm(rast_list_c8)
  
  WY_2014_results <- rbind(rast_list_c1, rast_list_c2, rast_list_c3,
                           rast_list_c4, rast_list_c5, rast_list_c6,
                           rast_list_c7, rast_list_c8)
  
  return(WY_2014_results)
}


system.time(big_vol_func("SN_SWE_WY2014.h5"))
