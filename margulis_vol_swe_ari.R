## testing using apply to iterate volume calculation
## need to look at virtual memory backup
## started 7/21/2020


## ari update 4/15/2021

library(BiocManager) # huge repo of packages used in the bio/genetics, we need it for our hdf5 reader
library(rhdf5) # hdf5 reader from bioc, which is the format the margulis data is in
library(rgeos) # spatial data driver
library(rasterVis) #https://oscarperpinan.github.io/rastervis/#levelplot
library(rgdal) # geospatial data abstraction library aka GDAL, very important to learn
library(tidyverse) # for the pipe operator
library(sf) # vector data aka shapefiles
library(data.table) # very useful package for working with dataframes and .csv data, much faster than base r
library(gtools) # for mixed sort
library(parallel) # allows for parallel processing, which we will use below
library(raster) # working with raster data


#set working directory
setwd("/Volumes/JT/projects/margulis/swe")


hdf_path <- "/Volumes/JT/projects/margulis/swe/" #create path
hdf_name <- "SN_SWE_WY1986.h5" #create file name
hdf_file <- paste(hdf_path, hdf_name, sep="") #full path

## raster temp file settings
# while doing these calculations theres a lot of garbage created
# creating a place to store it helps with RAM management, would crash if not
rasterOptions(tmpdir = "path_here")
tmpDir() #check location
removeTmpFiles(.5) #set to delete after 30 min



#########################################################################
########################## static data ##################################
#########################################################################




# bring in pixel area raster, which is in [m^2] using raster function
pixel_area <-raster("/Volumes/JT/projects/margulis/static/rasters/SNSR_pixel_area.tif")

### this information from the margulis_readme about the projections of the data

#'Projection': 'Lambert Cylindrical Equal Area' 
#'Standard parallel (deg.)': 38.700000
#'Center longitude (deg.)': -120.450000
#'Ellipsoid': 'Clarke 1866'
#'Semi-major axis of ellipsoid': 6378206.400000
#'Eccentricity of ellipsoid': 0.082272

# to figure out how these are interpreted in R, we use the projInfo command

# ellipsoid
projInfo("ellps")
# Clarke 1866 = +ellps=clrk66

# projection
projInfo("proj")
# Lambert Cylindrical Equal Area = +proj=leac

# datum
# you can either set the bounding coordinates which are 
# xmn=-123.3, xmx=-117.6, ymn=35.4, ymx=42

# or set the datum, both get the same results
# the clrk66 projection uses the north american datum = +datum=NAD27

# using this information we set the object crs
crs(pixel_area) <-crs("+proj=leac +ellps=clrk66 +datum=NAD27 +no_defs")
pixel_area # inspect meta data
# quick check shows the extent is the same as we outline above, data is projected correctly
plot(pixel_area) # view it
hist(pixel_area) # quick view of the data

# import cell number rasters
# i made these and they will be used to figure out the extent of SNSR data we should load in
# could def do this in Q very easily, but good practice here

# x
x_cell_num <-raster("/Volumes/JT/projects/margulis/static/rasters/x_cell_num.tif")
crs(x_cell_num)<-crs(pixel_area) # this uses the pixel_area crs we just defined and assin

# y
y_cell_num <-raster("/Volumes/JT/projects/margulis/static/rasters/y_cell_num.tif")
crs(y_cell_num)<-crs(pixel_area)


#### import the uppper san joaquin shape file
usj_raw <-readOGR("/Volumes/JT/projects/margulis/ari/USJW_Watershed_shape/Watershed_USJ.shp")
plot(usj_raw)
summary(usj_raw) # need to project

# use spTransform to reproject shapefile to SNSR crs
# usj <-spTransform(usj_raw, crs("+proj=leac +datum=NAD27"))
# usj@bbox <- as.matrix(extent(usj_raw)) # change extent back to lat/lon, https://stackoverflow.com/questions/35445095/how-to-extend-a-spatialpolygonsdataframe-to-a-given-extent
# usj

######################## this isn't working for me, shapefile won't plot when i reproject, not sure why
## not a huge deal but something to look into in the future
## for you purposes with the class project it shouldn't matter

###  plot the shapefile on the DEM to make sure it looks good
dem <-raster("/Volumes/JT/projects/margulis/static/rasters/SNSR_dem.tif")
crs(dem) <-crs(pixel_area)

# plot
plot(dem)
plot(usj_raw, add = TRUE)
# looks good

# mask all values outside of the shape file boundary
usj_dem <-mask(dem, usj_raw)
plot(usj_dem)
hist(usj_dem)

# crop to the extent of the shape file
# extent is the retangular boundary the shapefile takes up
extent(usj_raw)

usj_dem <- crop(usj_dem, extent(usj_raw))
plot(usj_dem)
plot(usj_raw, add = TRUE)
hist(usj_dem)



#########################################################################
############################# swe data ##################################
#########################################################################

# first go at manipulating the SWE data

# set path and file name for hdf5 SWE file
hdf_path <- "/Volumes/JT/projects/margulis/swe/" #create path
hdf_name <- "SN_SWE_WY1986.h5" #create file name
hdf_file <- paste(hdf_path, hdf_name, sep="") #full path
h5ls(hdf_file) #list contains 3 groups. lat, long, and SWE 
# h5readAttributes(hdf_file, name = "SWE") #$units = mm

# read in 1 days SWE data which is an array, or stack of matrixes
system.time(swe86_185_raw <- h5read(hdf_file, "/SWE", index = list(1:6601,1:5701,185))) #read in SWE group
class(swe86_185_raw) #inspect 
dim(swe86_185_raw) #dimensions

# convert to matrix and then a raster to visualize
# import to put the dimensions, won't work if you dont
swe86_185_mat <-as.matrix(swe86_185_raw[1:6601, 1:5701, 1]) # convert array to matrix
swe86_185 <-raster(swe86_185_mat) # convert from matrix to raster
crs(swe86_185) <-crs(pixel_area) # set crs
extent(swe86_185) <-extent(pixel_area) # set extent
swe86_185 # check
plot(swe86_185) # looks weird
hist(swe86_185) # need to change NA value
values(swe86_185)[values(swe86_185) == -32768] <- NA # change the value using raster function values
hist(swe86_185) #look again, it worked

# plot with shapefile
plot(swe86_185)
plot(usj_raw, add = TRUE)



####################### what extent of the hdf5 file needs to be pulled in?

#### x
# plot x cell number
plot(x_cell_num)
plot(usj_raw, add = TRUE) # add shape 
x_usj <-crop(x_cell_num, usj_raw)# crop to shape
plot(x_usj) # plot crop
plot(usj_raw, add = TRUE)

# pull out max value
xmax <-cellStats(x_usj, 'max')
xmin <-cellStats(x_usj, 'min')

#### y
# plot y cell number
plot(y_cell_num)
plot(usj_raw, add = TRUE) # add shape 
y_usj <-crop(y_cell_num, usj_raw)# crop to shape
plot(y_usj) # plot crop
plot(usj_raw, add = TRUE)

# pull out may value
ymax <-cellStats(y_usj, 'max')
ymin <-cellStats(y_usj, 'min')


#### test extent and see if works properly with SWE data
test <- h5read(hdf_file, "/SWE", index = list(ymin:ymax, xmin:xmax, 185)) #read in SWE group
test2 <-as.matrix(test[1:756, 1:1081, 1]) #mat
test3 <-raster(test2) # rast
crs(test3) <-crs(pixel_area) # crs
extent(test3) <-extent(usj_raw) # extent
values(test3)[values(test3) == -32768] <- NA # set no data
test4 <-mask(test3, usj_raw)

# plot SWE data and usj shape file
plot(test4)
plot(usj_raw, add = TRUE)


#########################################################################
##################### Volumetrics SWE calculation #######################
#########################################################################



# define the raster math you want to preform 
# in this case, convert both pixel area [m^2] and swe [mm^2]
# to [km^2] and [km], respectively by multiplying by 1e-6

convert_units <-function(a,b){return((a*1e-6)*(b*1e-6))}

# deine vol_swe function which overlays the rasters and performs to function we just defined
# we now have a raster with swe in [km^3], generated using the correct pixel size
# next we sum all the pixels using cellstats to create daily volumetric swe value
vol_swe_full <-function(swe){
  
  # raster::overlay creates new layer from defined function
  swe_by_pixel <- overlay(swe, pixel_area, fun = convert_units)
  
  # raster::cellStats does bascic stats on all the cells, this case we use sum, but could do mean, max, etc..
  vol_swe <-cellStats(swe_by_pixel, 'sum', digits=9, na.rm=TRUE)
  print(vol_swe) # print list
}

vol_swe_full(swe86_185)

vol_swe_usj <-function(swe){
  
  # crop pixel area function
  usj_pixel_area_raw <-crop(pixel_area, usj_raw)
  extent(usj_pixel_area_raw) <-extent(usj_raw)
  masked_usj <-mask(usj_pixel_area_raw, usj_raw)
  
  #mask swe
  masked_swe <-mask(swe, usj_raw)
  
  # raster::overlay creates new layer from defined function
  swe_by_pixel <- overlay(masked_swe, masked_usj, fun = convert_units)
  
  # raster::cellStats does bascic stats on all the cells, this case we use sum, but could do mean, max, etc..
  vol_swe <-cellStats(swe_by_pixel,'sum', digits=9, na.rm=TRUE)
  print(vol_swe) # print list
}



# test to see if function works
vol_swe_usj(test3) #yes!


# create 8 functions which read in the hdfs, convert to brick, and then breack into raster list
# it has to be done in chunks bc of how large the files are and how much RAM your CPU has
# this could be done in less steps the more RAM you have on your CPU

# i think you could iterate this but i'm just not sure how do it

# update funciton to usj workflow


open_hdf_as_raster_list <- function( hdf_name ) {
  
  rast_list <-hdf_name %>%
    file.path( hdf_path , . ) %>%
    h5read(., "/SWE", index = list(ymin:ymax, xmin:xmax,1:365)) %>%
    brick(., xmn = -119.7356, xmx = -118.6544, ymn = 36.98334, ymx = 37.73928, 
          CRS("+proj=leac +ellps=clrk66 +datum=NAD27")) %>%
    mask(., usj_raw)
    values(rast_list)[values(rast_list) == -32768] <- NA 
    as.list(rast_list)
}

create_vol_csv <-function(x){
  
  name <- gsub(".h5", "", x) #file name without .h5 for naming the the future
  colname <- gsub("SN_SWE_", "", name) #just name WYXXXX for col name in df
  
  rast_list <-open_hdf_as_raster_list(x) #open hdf has raster list
  system.time(results <-lapply(rast_list, function(x) vol_swe_usj(x), mc.cores = 10, mc.cleanup = TRUE)) #apply vol_swe to raster list
  results_df <- data.frame(matrix(unlist(results), nrow=length(results), byrow=T)) #create df with results
  colnames(results_df)[1] <- colname #rename col with correct WY
  print(results_df) #df for this chunk of results
  rm(rast_list)
  h5closeAll()

  
  #stich data together and save
  doWY<-(1:365) #create vector
  doWY_df <- data.frame(matrix(unlist(doWY), nrow=length(doWY), byrow=T)) #convert to df
  colnames(doWY_df)[1] <- "doWY"
  
  #bind on doWY column
  full_year <-cbind(doWY_df, results_df)
  
  setwd("/Volumes/JT/projects/margulis/swe/")
  write.csv(full_year, paste0(name, ".csv"), row.names=FALSE)
  return(full_year)
  
}


####### run the new function on the list of hdf files we previusly defined

# define your file paths here.
hdf_path <- "/Volumes/jack_t/projects/margulis/swe/hdf" #create path
files <- list.files(hdf_path, pattern = ".h5") #list files for all years
hdf_list <-mixedsort(sort(files)) #sort in correct order, r won't do this naturally
print(hdf_list) #test print, check they're sorted in numerical order

# single yeasr
system.time(create_vol_csv(hdf_name))

# apply to list
system.time(results_list <-lapply(hdf_list, function(x) create_vol_csv(x)))

#system.time(results_list <-mclapply(hdf_list, create_vol_csv, mc.cores = 15, mc.cleanup = TRUE))












####### old functions 

open_hdf_as_raster_list_c1 <- function( hdf_name ) {
  
  hdf_name %>%
    file.path( hdf_path , . ) %>%
    h5read(., "/SWE", index = list(NULL,NULL,1:50)) %>%
    brick(., xmn=-123.3, xmx=-117.6, ymn=35.4, ymx=42, CRS("+proj=leac +ellps=clrk66")) %>%
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
  doWY<-(1:365) #create vector
  doWY_df <- data.frame(matrix(unlist(doWY), nrow=length(doWY), byrow=T)) #convert to df
  colnames(doWY_df)[1] <- "doWY"
  
  #bind on doWY column
  full_year <-cbind(doWY_df, results)
  
  
  setwd("/Volumes/john_tarricone/UNR_summer_20/margulis/volume_results/")
  write.csv(full_year, paste0(name, ".csv"), row.names=FALSE)
  return(full_year)
  
}

