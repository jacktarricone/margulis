
#######saving "chunks" of data at a time as text files for memory purposes
#will later be brought back in as a listed and converted to rasters for processing

#set c1
setwd("/Volumes/john_tarricone/UNR_summer_20/margulis/swe/daily_rasters/WY_2014/c1")

########first chunk: days 1-90
seq<-(1:90)

for (i in 1:365){
  mat<-as.matrix(swe_2014_1[,,i])
  rast <-raster(mat, xmn=-123.3, xmx=-117.6, ymn=35.4, ymx=42, crs=CRS("+proj=leac +ellps=clrk66"))
  rast[ rast[] == -32768 ] <- NA
  filename <- paste("swe_2014_doWY_",i)
  writeRaster(rast, filename, format='GTiff', overwrite=FALSE)
}

#remove first chunk before starting second chunk
rm(swe_2014_1)





#######second chunk: days 91 - 180
swe_2014_2 <- h5read(hdf_file, "/SWE", index = list(NULL, NULL, 91:180))

#set c2
setwd("/Volumes/john_tarricone/UNR_summer_20/margulis/swe/daily_rasters/WY_2014/c2")

for (i in 1:365){
  mat<-as.matrix(swe_2014_2[,,i])
  rast <-raster(mat, xmn=-123.3, xmx=-117.6, ymn=35.4, ymx=42, crs=CRS("+proj=leac +ellps=clrk66"))
  rast[ rast[] == -32768 ] <- NA
  filename <- paste("swe_2014_doWY_",(i+90))
  writeRaster(rast, filename, format='GTiff', overwrite=FALSE)
}

#remove second chunk before starting second chunk
rm(swe_2014_2)





#######third chunk: days 181 - 270
swe_2014_3 <- h5read(hdf_file, "/SWE", index = list(NULL, NULL, 91:180))

#set c3
setwd("/Volumes/john_tarricone/UNR_summer_20/margulis/swe/daily_rasters/WY_2014/c3")

for (i in 1:365){
  mat<-as.matrix(swe_2014_3[,,i])
  rast <-raster(mat, xmn=-123.3, xmx=-117.6, ymn=35.4, ymx=42, crs=CRS("+proj=leac +ellps=clrk66"))
  rast[ rast[] == -32768 ] <- NA
  filename <- paste("swe_2014_doWY_",(i+180))
  writeRaster(rast, filename, format='GTiff', overwrite=FALSE)
}

#remove second chunk before starting second chunk
rm(swe_2014_3)





#fourth chunk: days 271 - 366
swe_2014_4 <- h5read(hdf_file, "/SWE", index = list(NULL, NULL, 91:180))

#set c4
setwd("/Volumes/john_tarricone/UNR_summer_20/margulis/swe/daily_rasters/WY_2014/c4")

for (i in 1:365){
  mat<-as.matrix(swe_2014_4[,,i])
  rast <-raster(mat, xmn=-123.3, xmx=-117.6, ymn=35.4, ymx=42, crs=CRS("+proj=leac +ellps=clrk66"))
  rast[ rast[] == -32768 ] <- NA
  filename <- paste("swe_2014_doWY_",(i+270))
  writeRaster(rast, filename, format='GTiff', overwrite=FALSE)
}

#remove second chunk before starting second chunk
rm(swe_2014_4)










########################################### data analysis section


#read in pixel area raster
pixel_area <-raster("/Volumes/john_tarricone/UNR_summer_20/margulis/static/rasters/SNSR_pixel_area.tif")


# define multiplication function
# convert both a, which is swe mm, and b, wich is m^2 to km and km^2
# in this case both are * by 1e-6

pix_vol_swe <-function(a,b){return((a*1e-6)*(b*1e-6))}

vol_swe<-function(x){
  pix_vol_swe <-function(a,b){return((a*1e-6)*(b*1e-6))}
  swe_by_pixel <- overlay(x, pixel_area, fun= pix_vol_swe )  
  vol_swe <-cellStats(swe_by_pixel,'sum', digits=9, na.rm=TRUE)
  print(vol_swe)
}

### c1

#read in raster list c1
setwd("/Volumes/john_tarricone/UNR_summer_20/margulis/swe/daily_rasters/WY_2014/c1")
files_c1 <- list.files(pattern = ".tif") #list files
fileslist_c1 <-mixedsort(sort(files_c1)) #sort in proper order
print(fileslist_c1) #test print
yurt<-stack(fileslist_c1)
raster_list_c1 <-lapply(fileslist_c1, raster) #load list in 

## mcapply function to raster list
system.time(results_c1<-stackApply(yurt, indices=c(1,1,1,2,2,2)function(x) vol_swe(x)))

## mcapply function to raster list
system.time(results_c1<-mclapply(raster_list_c1, function(x) vol_swe(x), mc.cores = 3, mc.cleanup = TRUE))

results_df_c1 <- data.frame(matrix(unlist(results_c1), nrow=length(results_c1), byrow=T))
colnames(results_df_c1)[1] <- "WY_2014"        



### c2

#read in raster list c2
files_c2 <- list.files(pattern = ".tif") #list files
fileslist_c2 <-mixedsort(sort(files_c2)) #sort in proper order
print(fileslist_c2) #test print
raster_list_c2 <-lapply(fileslist_c2, raster) #load list in 


## mcapply function to raster list
system.time(results_c2<-mclapply(raster_list_c2, function(x) vol_swe(x), mc.cores = 3, mc.cleanup = TRUE))

results_df_c2 <- data.frame(matrix(unlist(results_c2), nrow=length(results_c2), byrow=T))
colnames(results_df_c2)[1] <- "WY_2014"     


### c3

#read in raster list c3
files_c3 <- list.files(pattern = ".tif") #list files
fileslist_c3 <-mixedsort(sort(files_c3)) #sort in proper order
print(fileslist_c3) #test print
raster_list_c3 <-lapply(fileslist_c3, raster) #load list in 


## mcapply function to raster list
system.time(results_c3<-mclapply(raster_list_c3, function(x) vol_swe(x), mc.cores = 3, mc.cleanup = TRUE))

results_df_c3 <- data.frame(matrix(unlist(results_c3), nrow=length(results_c3), byrow=T))
colnames(results_df_c3)[1] <- "WY_2014"   

### c4

#read in raster list c4
files_c4 <- list.files(pattern = ".tif") #list files
fileslist_c4 <-mixedsort(sort(files_c4)) #sort in proper order
print(fileslist_c4) #test print
raster_list_c4 <-lapply(fileslist_c4, raster) #load list in 


## mcapply function to raster list
system.time(results_c4<-mclapply(raster_list_c4, function(x) vol_swe(x), mc.cores = 3, mc.cleanup = TRUE))

results_df_c4 <- data.frame(matrix(unlist(results_c4), nrow=length(results_c4), byrow=T))
colnames(results_df_c4)[1] <- "WY_2014"   


#stich data together and save
doWY<-(1:365) #create vector
doWY_df <- data.frame(matrix(unlist(doWY), nrow=length(doWY), byrow=T)) #convert to df
colnames(doWY_df)[1] <- "doWY" #rename column

#bind all four data frames together to create full year time series
full_swe <-rbind(results_df_c1, results_df_c2, results_df_c3, results_df_c4)

#bind on doWY column
full_year<-cbind(doWY_df, full_swe)