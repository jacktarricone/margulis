test<-raster_list_c1[[90]]
volumetric_swe(test)

library(BiocManager)
library(rhdf5)
library(tidyverse)
library(rgeos)
library(rasterVis) #https://oscarperpinan.github.io/rastervis/#levelplot
library(rgdal)
library(raster)
library(data.table)
library(gtools)

vol_swe_new <-function(x){
  swe_sum <-cellStats(x,'sum', digits=0, na.rm=TRUE) #sum of all values swe in [mm]
  n_na<-cellStats(x, function(i, ...) sum(is.na(i))) #number of na values
  n_na<-as.numeric(n_na) #properly format
  cell_area <- (.0081) # [km^2]
  n_cells <-ncell(x) #number of total cells in the raster
  cells_used <- (n_cells-n_na) #number of cells reporting data
  avg_cell_swe <-((swe_sum/cells_used)/1000000) #average swe per cell in [km]
  volumetric_swe <-(cells_used*cell_area*avg_cell_swe) #volumetric swe [km^3]
  print(volumetric_swe)
}

function(x) {
  swe_sum <-cellStats(x, 'sum', digits=0, na.rm=TRUE) #sum of all values swe in [mm]
  cell_area <- (.0081) # [km^2]
  n_cells <-ncell(x) #number of total cells in the raster
  swe_freq<-as.data.frame(freq(x)) #create df for data values and thier frequency
  swe_freq <-swe_freq %>% mutate_all(~replace(., is.na(.), -9)) %>% filter(value =="-9") #change NA back to -32768 so can be isolated and manipulated
  n_na <- sum(swe_freq$count) #sum the count column, giving you number of na cells
  cells_used <- (n_cells-n_na) #number of cells reporting data
  avg_cell_swe <-((swe_sum/cells_used)/1000000) #average swe per cell in [km]
  volumetric_swe <-(cells_used*cell_area*avg_cell_swe) #volumetric swe [km^3]
  print(volumetric_swe)
}

system.time(results_list_c1<-(lapply(raster_list_c1, function(x) volumetric_swe(x)))) 
