# january 13th
# trimming data for american river tests

library(tidyverse)
library(rgeos)
library(raster)
library(rgdal)
library(data.table)
library(gtools)
library(spatialEco)
library(EnvStats)
library(sf)
library(rasterVis)

######## read in sdd raster stack
setwd("/Volumes/JT/projects/margulis/snow_metric_rasters/sdd/rasters")
list <-list.files()
sdd_list <-lapply(list, function(x) raster(x))
sdd_stack <-stack(sdd_list) 
crs(sdd_stack)<-"+proj=leac +ellps=clrk66"
plot(sdd_stack[[9]])
sdd_stack

########## static rasters

# dem
dem <-raster("/Volumes/JT/projects/margulis/static/rasters/SNSR_DEM.tif")
crs(dem)<-"+proj=leac +ellps=clrk66" 
plot(dem)

# aspect
aspect <-raster("/Volumes/JT/projects/margulis/static/rasters/SNSR_aspect.tif")
crs(aspect)<-"+proj=leac +ellps=clrk66" 
plot(aspect)

# lat
lat <-raster("/Volumes/JT/projects/margulis/static/rasters/SNSR_lat.tif")
crs(lat)<-"+proj=leac +ellps=clrk66" 
plot(lat)

# lon
lon <-raster("/Volumes/JT/projects/margulis/static/rasters/SNSR_lon.tif")
crs(lon)<-"+proj=leac +ellps=clrk66" 
plot(lon)

# slope
slope <-raster("/Volumes/JT/projects/margulis/static/rasters/SNSR_slope.tif")
crs(slope)<-"+proj=leac +ellps=clrk66" 
plot(slope)

# load in american river shape file

american <-readOGR("/Volumes/JT/projects/margulis/american_test/american.shp")
proj4string(american) <- crs("+proj=leac +ellps=clrk66")
plot(american)

plot(dem)
plot(american, add = TRUE)
summary(american)
american




