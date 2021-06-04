#6/9 starting to mess with hdf5 margulis data

#pj bartlein hdf class
#https://pjbartlein.github.io/REarthSysSci/hdf5_intro.html

#vignette
#http://127.0.0.1:10363/library/rhdf5/doc/rhdf5.html

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install(version = "3.11")

library(rhdf5)
library(maps)
library(mapproj)
library(tidyverse)
library(maptools)
library(rgeos)
library(raster)
library(sf)
library(rasterVis) #https://oscarperpinan.github.io/rastervis/#levelplot
library(RColorBrewer)
library(rgdal)
library(grid)
library(scales)
library(viridis)  # better colors for everyone
library(ggthemes)
library(sp)

#bring in CA shape file
shp_path <- "/Volumes/JTARRICONE/GIS/sml_wtr_copy/ca-state-boundary/" #create path
shp_name <- "CA_State_TIGER2016.shp" #indentify file name
shp_file <- paste(shp_path, shp_name, sep="") #full path
ca_shp <- read_sf(shp_file) #read in shape file
ca_boundary <-as(st_geometry(ca_shp), Class="Spatial") #convert to spatial data
plot(ca_boundary) #test plot

#set path and file name for hdf5 SWE file
hdf_path <- "/Volumes/JTARRICONE/UNR_summer_20/margulis_data/swe/code_test/" #create path
hdf_name <- "SN_SWE_WY2016.h5" #create file name
hdf_file <- paste(hdf_path, hdf_name, sep="") #full path
h5ls(hdf_file) #list contains 3 groups. lat, long, and SWE 
h5readAttributes(hdf_file, name = "SWE") #$units = mm

#get lons
lon <- h5read(hdf_file, "/lon")
nlon <- length(lon)
nlon
max(lon)
min(lon)
head(lon)
tail(lon)

#get lat
lat <- h5read(hdf_file, "/lat")
nlat <- length(lat)
nlat
head(lat)
min(lat)

#read in SWE data which is an array, or stack of matrixes
swe <- h5read(hdf_file, "/SWE") #read in SWE group
class(swe) #is an array
str(swe) #gives dimensions


#subset single day of data into a matrix
#try for 191th doWY or april 10th
swe_april_10 <-as.matrix(swe_2016[1:6601, 1:5701, 191]) #pull out day 191 from array
swe_april_10_raster <- raster(swe_april_10, xmn=-123.3, xmx=-117.6, ymn=35.4, ymx=42) #set lat lon cords
swe_april_10_raster[ swe_april_10_raster[] == -32768 ] <- NA #sets no data value to 0 using subseting, https://stackoverflow.com/questions/19680079/r-crop-no-data-of-a-raster
swe_april_10_raster #inspect raster
crs(swe_april_10_raster) <- "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs" 
image(swe_april_10_raster) #print raster

#try for 291th doWY or oct 18th
swe_oct_18 <-as.matrix(swe_2016[1:6601, 1:5701, 291]) #pull out day 191 from array
swe_oct_18_raster <- raster(swe_oct_18, xmn=-123.3, xmx=-117.6, ymn=35.4, ymx=42) #set lat lon cords
swe_oct_18_raster[ swe_oct_18_raster[] == -32768 ] <- NA #sets no data value to NA using subseting, https://stackoverflow.com/questions/19680079/r-crop-no-data-of-a-raster
crs(swe_oct_18_raster) <- "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs" 
swe_oct_18_raster #inspect raster
image(swe_oct_18_raster) #print raster


# rasterVis plot
mapTheme <- rasterTheme(region=brewer.pal(9,"YlGnBu"))
plt <- levelplot(swe_oct_18_raster, margin=F, par.settings=mapTheme, 
                 at=seq(0, 3000, length=50),main="2016-04-01 Sierra SWE")
plt + layer(sp.lines(ca_boundary, col="black", lwd=1.0))

#bring in clipped DEM from Qgis
sierra_raster <- raster("/Volumes/JTARRICONE/GIS/DEMs/sierra_clipped_dem.tif")
sierra_raster
image(sierra_raster)

#save rasters
writeRaster(swe_oct_18_raster, "/Volumes/JTARRICONE/UNR_summer_20/project/margulis_data/swe_oct_18_raster.tif")
writeRaster(swe_april_10_raster, "/Volumes/JTARRICONE/UNR_summer_20/project/margulis_data/swe_april_10_raster.tif")

#raster histogram
hist(swe_oct_18_raster,
     breaks = 1000,
     main = "Distribution of SWE values",
     xlab = "SWE (mm)", ylab = "Frequency",
     col = "springgreen")



#volumetic swe caluclations

volumetric_swe <-function(x) {
swe_sum <-cellStats(x, 'sum', digits=0, na.rm=TRUE) #sum of all values swe in [mm]
cell_area <- (.0081) #km^2
n_cells <-ncell(x) #number of total cells in the raster
swe_freq<-as.data.frame(freq(x)) #create df for data values and thier frequency
swe_freq <-swe_freq %>% mutate_all(~replace(., is.na(.), -32768)) %>% filter(value =="-32768") #change NA back to -32768 so can be isolated and manipulated
n_na <- sum(swe_freq$count) #sum the count column, giving you number of na cells
cells_used <- (n_cells-n_na) #number of cells reporting data
avg_cell_swe <-((swe_sum/cells_used)/1000000) #average swe per cell in [km]
volumetric_swe <-(cells_used*cell_area*avg_cell_swe) #volumetric swe, in this case 9.26 [km^3]
print(volumetric_swe)
}

swe_1_raster <- subset(raster_stack, 1)
swe_2_raster <- subset(raster_stack, 2)
swe_3_raster <- subset(raster_stack, 3)
swe_4_raster <- subset(raster_stack, 4)
swe_5_raster <- subset(raster_stack, 5)
swe_6_raster <- subset(raster_stack, 6)
swe_7_raster <- subset(raster_stack, 7)
swe_8_raster <- subset(raster_stack, 8)
swe_9_raster <- subset(raster_stack, 9)
swe_10_raster <- subset(raster_stack, 10)


system.time(volumetric_swe(swe_7_raster))


swe_sum <-cellStats(swe_april_10_raster, 'sum', digits=0, na.rm=TRUE) #sum of all values swe in [mm]
cell_area <- (.0081) #km^2
n_cells <-ncell(swe_april_10_raster) #number of total cells in the raster
swe_freq<-as.data.frame(freq(swe_april_10_raster)) #create df for data values and thier frequency
swe_freq <-swe_freq %>% mutate_all(~replace(., is.na(.), -32768)) %>% filter(value =="-32768") #change NA back to -32768 so can be isolated and manipulated
n_na <- sum(swe_freq$count) #sum the count column, giving you number of na cells
cells_used <- (n_cells-n_na) #number of cells reporting data
avg_cell_swe <-((swe_sum/cells_used)/1000000) #average swe per cell in [km]
volumetric_swe <-(cells_used*cell_area*avg_cell_swe) #volumetric swe, in this case 9.26 [km^3]


volumetric_swe(swe_april_10_raster)



