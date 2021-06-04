#esting -9 instead of NA to see if fixes raster problem

library(BiocManager)
library(rhdf5)
library(tidyverse)
library(rgeos)
library(rasterVis) #https://oscarperpinan.github.io/rastervis/#levelplot
library(rgdal)
library(raster)
library(data.table)
library(gtools)

setwd("/Volumes/john_tarricone/UNR_summer_20/margulis/swe/daily_data/WY_2016")

#####################################################

#set path and file name for hdf5 SWE file
hdf_path <- "/Volumes/john_tarricone/UNR_summer_20/margulis/swe/hdf/" #create path
hdf_name <- "SN_SWE_WY2016.h5" #create file name
hdf_file <- paste(hdf_path, hdf_name, sep="") #full path
h5ls(hdf_file) #list contains 3 groups. lat, long, and SWE 
h5closeAll()
h5readAttributes(hdf_file, name = "SWE") #$units = mm

#read in days 1-100 of SWE data
lat <- h5read(hdf_file, "/lat")
lon <- h5read(hdf_file, "/lon")
swe_2016_1 <- h5read(hdf_file, "/SWE", index = list(NULL, NULL, 1:90)) #read in SWE group

#######saving "chunks" of data at a time as text files for memory purposes
#will later be brought back in as a listed and converted to rasters for processing

#set c1
setwd("/Volumes/john_tarricone/UNR_summer_20/margulis/swe/daily_data/wy_9999/")

########first chunk: days 1-90
seq<-(1:90)

for (i in seq){
  slice<-as.matrix(swe_2016_1[1:6601,1:5701,i]) #day single day
  slice[ slice[] == -32768 ] <- NA
  out <- matrix(slice, nrow=6601, ncol=5701)
  filename <- paste("swe_2016_doWY_test",i,".txt", sep="")
  fwrite(out, filename, sep="\t",col.names = FALSE, row.names = FALSE)
}

#remove first chunk before starting second chunk
rm(swe_2016_1)

setwd("/Volumes/john_tarricone/UNR_summer_20/margulis/swe/daily_data/WY_/")
files_c3 <- list.files(pattern = ".txt")
filelist_c3 <-mixedsort(sort(files_c3)) #sort in correct order
print(filelist_c3)

#read in lists of matrixes   
system.time(test_list <- (lapply(filelist_c3, function(x) as.matrix(fread(x, header=T))))) 

#convert to raster list
rast_list<-lapply(test_list, function(x) raster(x,xmn=-123.3, xmx=-117.6, ymn=35.4, ymx=42,CRS("+proj=leac +ellps=clrk66"))) #create raster list from matrix list
#raster_stack<-stack(raster_list) #stack rasterlist
#writeRaster(raster_stack, "wy2016_test") #save raster stack


neg_nine<-rast_list[[1]]
na<-rast_list[[2]]
neg_nine[ neg_nine[] == -9 ] <- NA
dim_check<-rast_list[[3]]
rm(dim_check)

writeRaster(na, "/Volumes/john_tarricone/UNR_summer_20/margulis/swe/daily_data/wy_9999/na.tif")
writeRaster(neg_nine, "/Volumes/john_tarricone/UNR_summer_20/margulis/swe/daily_data/wy_9999/neg_nine.tif")

fix<-as.matrix(test_list[[2]])
out <- matrix(fix, nrow=6601, ncol=5701)

###2005 test
wy_2005_max_rast<-raster(wy_2005_max,xmn=-123.3, xmx=-117.6, ymn=35.4, ymx=42,CRS("+proj=leac +ellps=clrk66"))
wy_2005_max<-as.matrix(fread("/Volumes/john_tarricone/UNR_summer_20/margulis/swe/daily_data/WY_2005/c3/swe_2005_doWY_181.txt"))
x<-wy_2005_max_rast


setwd("/Volumes/john_tarricone/UNR_summer_20/margulis/swe/daily_data/WY_2005")

#####################################################

#set path and file name for hdf5 SWE file
hdf_path <- "/Volumes/john_tarricone/UNR_summer_20/margulis/swe/hdf/" #create path
hdf_name <- "SN_SWE_WY2005.h5" #create file name
hdf_file <- paste(hdf_path, hdf_name, sep="") #full path
h5ls(hdf_file) #list contains 3 groups. lat, long, and SWE 
h5closeAll()
h5readAttributes(hdf_file, name = "SWE") #$units = mm

#read in days 1-100 of SWE data
swe_2005 <- h5read(hdf_file, "/SWE", index = list(NULL, NULL, 181)) #read in SWE group
max_2005<-swe_2005[1:6601, 1:5701,]
max_2005[ max_2005[] == -32768 ] <- NA
fwrite()

max_2005_rast<-raster(max_2005, xmn=-123.3, xmx=-117.6, ymn=35.4, ymx=42,CRS("+proj=leac +ellps=clrk66"))

write.table(max_2005,"/Volumes/john_tarricone/UNR_summer_20/margulis/swe/daily_data/wy_9999/write_na_test_shit.txt", sep="\t",col.names = FALSE, row.names = FALSE)
write_test<-as.matrix(fread("/Volumes/john_tarricone/UNR_summer_20/margulis/swe/daily_data/wy_9999/write_na_test_shit.txt"))
rast<-raster(write_test, xmn=-123.3, xmx=-117.6, ymn=35.4, ymx=42,CRS("+proj=leac +ellps=clrk66"))

swe_sum <-cellStats(rast,'sum', digits=0, na.rm=FALSE) #sum of swe value in every pixel in [mm]
swe_sum_km <-(swe_sum/1000000) 
freq_table<-freq(max_2005_rast)
n_na<-cellStats(x, function(i, ...) sum(is.na(i))) #number of na values in raster
n_na<-as.numeric(n_na) #properly format
cell_area <- (.0081) # [km^2]
n_cells <-ncell(x) #number of total cells in the raster
cells_test<-6000000
cells_used <- (n_cells-n_na) #number of cells reporting data
avg_cell_swe <-((swe_sum/cells_used)/1000000) #average swe per cell in [km]
volumetric_swe <-(cells_test*cell_area*avg_cell_swe) #volumetric swe [km^3]
print(volumetric_swe)
x<-max_2005_rast

table<-freq(max_2005_rast)

  swe_sum <-cellStats(x,'sum', digits=0, na.rm=TRUE) #sum of swe value in every pixel in [mm]
  swe_sum_km <-(swe_sum/1000000) 
  freq_table<-freq(max_2005_rast)
  n_na<-cellStats(x, function(i, ...) sum(is.na(i))) #number of na values in raster
  n_na<-as.numeric(n_na) #properly format
  cell_area <- (.0081) # [km^2]
  n_cells <-ncell(x) #number of total cells in the raster
  n_0<-784054
  cells_used <- (n_cells-n_na) #number of cells reporting data
  avg_cell_swe <-((swe_sum/cells_used)/1000000) #average swe per cell in [km]
  volumetric_swe <-(cells_used*cell_area*swe_sum_km) #volumetric swe [km^3]
  print(volumetric_swe)
  area_used<-cells_used*cell_area #area in [km^2]

          