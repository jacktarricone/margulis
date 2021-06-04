#breaking arrays into months for leap year which are (2016,2012,2008,2004,2000,1996,1992,1988)

library(BiocManager)
library(rhdf5)
library(tidyverse)
library(rgeos)
library(rasterVis) #https://oscarperpinan.github.io/rastervis/#levelplot
library(rgdal)
library(raster)


setwd("/Volumes/JTARRICONE/UNR_summer_20/margulis_data/swe/code_test")

#####################################################

#set path and file name for hdf5 SWE file
hdf_path <- "/Volumes/JTARRICONE/UNR_summer_20/margulis_data/swe/hdf/" #create path
hdf_name <- "SN_SWE_WY2016.h5" #create file name
hdf_file <- paste(hdf_path, hdf_name, sep="") #full path
h5ls(hdf_file) #list contains 3 groups. lat, long, and SWE 
h5closeAll()
h5readAttributes(hdf_file, name = "SWE") #$units = mm

#read in SWE data which is an array, or stack of matrixes
swe_2016 <- h5read(hdf_file, "/SWE") #read in SWE group
class(swe_2016) #inspect 
dim(swe_2016) #dimensions

seq<-(1:366)
#https://stackoverflow.com/questions/7033699/write-many-files-in-a-for-loop
for (i in seq){
  slice<-as.matrix(swe_2016[1:6601,1:5701,i])
  filename <- paste("2016swe_doWY",i, ".txt", sep="")
  write.table(slice, filename, col.names=FALSE,row.names=FALSE,sep="\t",quote=FALSE)
  }

#list all text files in working directory
filelist <- list.files(pattern = ".*.txt")

#assuming tab separated values with a header    
matrix_list <- lapply(filelist, function(x) as.matrix(read.table(x, header=T))) 

#QA data
add_na_list<-lapply(matrix_list, function(x) na_if(x, -32768)) #add NAs to matrix list
raster_list<-lapply(add_na_list, function(x) raster(x, xmn=-123.3, xmx=-117.6, ymn=35.4, ymx=42)) #create raster list from matrix list
raster_stack<-stack(raster_list) #stack rasterlist
writeRaster(raster_stack, "wy2016_test") #save raster stack

#volumetric swe function
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

#apply to raster list
system.time(swe_stats<-as.data.frame(lapply(raster_list, function(x) volumetric_swe(x)))) 

#format from list to DF
swe_stats_new <-rownames_to_column(swe_stats) %>% 
  gather(var, value, -rowname) %>% 
  spread(rowname, value) 

as.data.frame(swe_stats_new)

#delete unwanted column
swe_stats_new <-dplyr::select(swe_stats_new,-c(var)) %>% rename(swe_km3 = 1)






