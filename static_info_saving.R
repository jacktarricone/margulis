#saving the meta/info from the static file

library(BiocManager)
library(rhdf5)
library(tidyverse)
library(rgeos)
library(rasterVis) #https://oscarperpinan.github.io/rastervis/#levelplot
library(rgdal)
library(raster)
library(data.table)
library(gtools)

###static
#set path and file name for hdf5 SWE file
hdf_path3 <- "/Volumes/jt/projects/margulis/static/" #create path
hdf_name3 <- "SN_static_data.h5" #create file name
hdf_file3 <- paste(hdf_path3, hdf_name3, sep="") #full path
h5ls(hdf_file3) #list contains 3 groups. lat, long, and SWE 
h5closeAll()

#read in days 1-100 of SWE data
dem <- h5read(hdf_file3, "/DEM") #read in SWE group
pixel_area <- h5read(hdf_file3, "/PIXEL_AREA") #read in SWE group
lat <- h5read(hdf_file3, "/lat") #read in SWE group
lon <- h5read(hdf_file3, "/lon") #read in SWE group
x_coord <- h5read(hdf_file3, "/x_coordinate") #read in SWE group
y_coord<- h5read(hdf_file3, "/y_coordinate") #read in SWE group

dem<-as.matrix(dem[1:6601, 1:5701,])
pixel_area<-as.matrix(pixel_area[1:6601, 1:5701,])
x_coord<-as.matrix(x_coord[1:6601, 1:5701,])
y_coord<-as.matrix(y_coord[1:6601, 1:5701,])

#copy lat values to create matrix
lat_rast<-do.call(cbind, replicate(5701, lat, simplify=FALSE))

#copy lon values to create matrix
lon_rast<-do.call(rbind, replicate(6601, lon, simplify=FALSE))

# create cell number rasters

y_num <-as.matrix(1:6601,1)
x_num <-t(as.matrix(1:5701))

#copy y values values to create matrix
y_cell_rast<-do.call(cbind, replicate(5701, y_num, simplify=FALSE))

#copy lon values to create matrix
x_cell_rast<-do.call(rbind, replicate(6601, x_num, simplify=FALSE))

#y_cell_num
y_cell_num <- raster(y_cell_rast, xmn=-123.3, xmx=-117.6, ymn=35.4, ymx=42, CRS("+proj=leac +ellps=clrk66")) #set lat lon cords
y_cell_num[ y_cell_num[] == -32768 ] <- NA #sets no data value to 0 using subseting, https://stackoverflow.com/questions/19680079/r-crop-no-data-of-a-raster
y_cell_num #inspect raster
image(y_cell_num) #print raster
writeRaster(y_cell_num, "/Volumes/jt/projects/margulis/static/rasters/y_cell_num.tif")


#x_cell_num
x_cell_num <- raster(x_cell_rast, xmn=-123.3, xmx=-117.6, ymn=35.4, ymx=42, CRS("+proj=leac +ellps=clrk66")) #set lat lon cords
x_cell_num[ x_cell_num[] == -32768 ] <- NA #sets no data value to 0 using subseting, https://stackoverflow.com/questions/19680079/r-crop-no-data-of-a-raster
x_cell_num #inspect raster
image(x_cell_num) #print raster
writeRaster(x_cell_num, "/Volumes/jt/projects/margulis/static/rasters/x_cell_num.tif")

cell_num <-stack(x_cell_num, y_cell_num)
writeRaster(cell_num, "/Volumes/jt/projects/margulis/static/rasters/cell_num.tif")

#dem
dem <- raster(dem, xmn=-123.3, xmx=-117.6, ymn=35.4, ymx=42, CRS("+proj=leac +ellps=clrk66")) #set lat lon cords
dem[ dem[] == -32768 ] <- NA #sets no data value to 0 using subseting, https://stackoverflow.com/questions/19680079/r-crop-no-data-of-a-raster
dem #inspect raster
image(dem) #print raster
writeRaster(dem, "/Volumes/john_tarricone/UNR_summer_20/margulis/static/rasters/SNSR_DEM.tif")


#pixel_area
pixel_area <- raster(pixel_area, xmn=-123.3, xmx=-117.6, ymn=35.4, ymx=42, CRS("+proj=leac +ellps=clrk66")) #set lat lon cords
pixel_area [ pixel_area[] == -32768 ] <- NA #sets no data value to 0 using subseting, https://stackoverflow.com/questions/19680079/r-crop-no-data-of-a-raster
pixel_area #inspect raster
image(pixel_area) #print raster
writeRaster(pixel_area, "/Volumes/john_tarricone/UNR_summer_20/margulis/static/rasters/SNSR_pixel_area.tif")


#x_coord
x_coord <- raster(x_coord, xmn=-123.3, xmx=-117.6, ymn=35.4, ymx=42, CRS("+proj=leac +ellps=clrk66")) #set lat lon cords
x_coord [ x_coord[] == -32768 ] <- NA #sets no data value to 0 using subseting, https://stackoverflow.com/questions/19680079/r-crop-no-data-of-a-raster
x_coord #inspect raster
image(x_coord) #print raster
writeRaster(x_coord, "/Volumes/john_tarricone/UNR_summer_20/margulis/static/rasters/SNSR_x_coord.tif")

#y_coord
y_coord <- raster(y_coord, xmn=-123.3, xmx=-117.6, ymn=35.4, ymx=42, CRS("+proj=leac +ellps=clrk66")) #set lat lon cords
y_coord [ y_coord[] == -32768 ] <- NA #sets no data value to 0 using subseting, https://stackoverflow.com/questions/19680079/r-crop-no-data-of-a-raster
y_coord #inspect raster
image(y_coord) #print raster
writeRaster(y_coord, "/Volumes/john_tarricone/UNR_summer_20/margulis/static/rasters/SNSR_y_coord.tif")

#lat
lat<- raster(lat_rast, xmn=-123.3, xmx=-117.6, ymn=35.4, ymx=42, CRS("+proj=leac +ellps=clrk66")) #set lat lon cords
lat #inspect raster
image(lat) #print raster
writeRaster(lat, "/Volumes/john_tarricone/UNR_summer_20/margulis/static/rasters/lat.tif")

#lon
lon <- raster(lon_rast, xmn=-123.3, xmx=-117.6, ymn=35.4, ymx=42, CRS("+proj=leac +ellps=clrk66")) #set lat lon cords
lon #inspect raster
image(lon) #print raster
writeRaster(lon, "/Volumes/john_tarricone/UNR_summer_20/margulis/static/rasters/lon.tif")

##create raster stack
setwd("/Volumes/john_tarricone/UNR_summer_20/margulis/static/rasters")
files<- list.files(pattern = ".tif")
print(files)
static_stack<-stack(files)
crs(static_stack)<-"+proj=leac +ellps=clrk66"
static_stack
writeRaster(static_stack, "/Volumes/john_tarricone/UNR_summer_20/margulis/static/rasters/static_stack.tif")


static_stack[[1]]
static_stack[[2]]
static_stack[[3]]
static_stack[[4]]
static_stack[[5]]
static_stack[[6]]