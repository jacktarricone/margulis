#6/9 starting to mess with hdf5 margulis data
#6/18 have gotten everything to work in much smaller pieces, need to figure out how to do it with parallel

#pj bartlein hdf class
#https://pjbartlein.github.io/REarthSysSci/hdf5_intro.html

#vignette
#http://127.0.0.1:10363/library/rhdf5/doc/rhdf5.html

library(BiocManager)
library(rhdf5)
#library(maps)
#library(mapproj)
library(tidyverse)
#library(maptools)
library(rgeos)
library(raster)
#library(sf)
library(rasterVis) #https://oscarperpinan.github.io/rastervis/#levelplot
#library(RColorBrewer)
library(rgdal)
#library(grid)
#library(scales)
#library(viridis)  # better colors for everyone
#library(ggthemes)
#library(sp)
#library(plyr)
#library(parallel)
#library(snow)
#library(doParallel)

setwd("/Volumes/JTARRICONE/UNR_summer_20/margulis_data/swe/code_test")
edit_r_environ()

#####################################################

#set path and file name for hdf5 SWE file
hdf_path <- "/Volumes/JTARRICONE/UNR_summer_20/margulis_data/swe/code_test/" #create path
hdf_name <- "SN_SWE_WY2016.h5" #create file name
hdf_file <- paste(hdf_path, hdf_name, sep="") #full path
h5ls(hdf_file) #list contains 3 groups. lat, long, and SWE 
h5closeAll()
h5readAttributes(hdf_file, name = "SWE") #$units = mm

#read in SWE data which is an array, or stack of matrixes
system.time(swe_2016 <- h5read(hdf_file, "/SWE")) #read in SWE group
class(swe_2016) #inspect 
dim(swe_2016) #dimensions

swe_1 <-as.matrix(swe_2016[1:6601, 1:5701, 1:30]) #pull out day 1 from array


#pull out single days
swe_1 <-as.matrix(swe_2016[1:6601, 1:5701, 1]) #pull out day 1 from array
swe_2 <-as.matrix(swe_2016[1:6601, 1:5701, 2]) #pull out day 2 from array
swe_3 <-as.matrix(swe_2016[1:6601, 1:5701, 3]) #pull out day 3 from array
swe_4 <-as.matrix(swe_2016[1:6601, 1:5701, 4]) #pull out day 4 from arra
swe_5 <-as.matrix(swe_2016[1:6601, 1:5701, 5]) #pull out day 5 from array
swe_6 <-as.matrix(swe_2016[1:6601, 1:5701, 6]) #pull out day 6 from array
swe_7 <-as.matrix(swe_2016[1:6601, 1:5701, 7]) #pull out day 7 from array
swe_8 <-as.matrix(swe_2016[1:6601, 1:5701, 8]) #pull out day 8 from array
swe_9 <-as.matrix(swe_2016[1:6601, 1:5701, 9]) #pull out day 9 from array
swe_10 <-as.matrix(swe_2016[1:6601, 1:5701, 10]) #pull out day 10 from array
swe_191 <-as.matrix(swe_2016[1:6601, 1:5701, 191]) #pull out day 191 from array

#save days as rdata
save(swe_1, file = "swe_1.RData")
save(swe_2, file = "swe_2.RData")
save(swe_3, file = "swe_3.RData")
save(swe_4, file = "swe_4.RData")
save(swe_5, file = "swe_5.RData")
save(swe_6, file = "swe_6.RData")
save(swe_7, file = "swe_7.RData")
save(swe_8, file = "swe_8.RData")
save(swe_9, file = "swe_9.RData")
save(swe_10, file = "swe_10.RData")
save(swe_191, file = "swe_191.RData")

#load back in to test that it works
load("swe_1.Rdata")
load("swe_2.Rdata")
load("swe_3.Rdata")
load("swe_4.Rdata")
load("swe_5.Rdata")
load("swe_6.Rdata")
load("swe_7.Rdata")
load("swe_8.Rdata")
load("swe_9.Rdata")
load("swe_10.Rdata")
load("swe_191.Rdata")

#create matrix list
matrix_list <- list(swe_1,swe_2,swe_3,swe_4,swe_5,swe_6,swe_7,swe_8,swe_9,swe_10,swe_191)

add_na_list<-lapply(matrix_list, function(x) na_if(x, -32768)) #add NAs to matrix list
raster_list<-lapply(add_na_list, function(x) raster(x, xmn=-123.3, xmx=-117.6, ymn=35.4, ymx=42)) #create raster list from matrix list
raster_stack<-stack(raster_list) #stack rasterlist
writeRaster(raster_stack, "wy2016_test") #save raster stack

#subset rasters
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
swe_191_raster <-subset(raster_stack, 11)

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

#this works~!!!
volumetric_swe(swe_191_raster)

#apply to raster list
system.time(swe_stats<-as.data.frame(lapply(raster_list, function(x) volumetric_swe(x)))) 

#format from list to DF
swe_stats_new <-rownames_to_column(swe_stats) %>% 
  gather(var, value, -rowname) %>% 
  spread(rowname, value) 

as.data.frame(swe_stats_new)

#delete unwanted column
swe_stats_new <-dplyr::select(swe_stats_new,-c(var))


################## using parallell computation

# Calculate the number of cores
num_cores <- detectCores() - 1

# Initiate cluster
cl <- makeForkCluster(num_cores)
#tell what to export?? not sure what this means
clusterExport(cl, "var",envir=environment())
#parLapply function
system.time(par_swe_stats<-parLapply(cl,raster_list, fun=volumetric_swe)) 
stopCluster(cl) #stop, needed

#format output data
par_swe_stats_new <-rownames_to_column(par_stats) %>% 
  gather(var, value, -rowname) %>% 
  spread(rowname, value) 

as.data.frame(par_swe_stats)

par_swe_stats_new <-dplyr::select(par_swe_stats_new,-c(var))


###### apply tests

data_list <- lapply(seq(from=1, to=991, by=10), function(i) bigDaddy[,i:(i+9)])

Sys.setenv('R_MAX_VSIZE'=32000000000)
A.mean <- apply(swe_2016, 3, function(x) na_if(x, -32768)) 
 
add_na_list<-lapply(swe_2016, function(x) na_if(x, -32768)) #add NAs to matrix list

