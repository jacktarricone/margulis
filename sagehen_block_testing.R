# test on sagehen area 10/22

library(easypackages)
library(BiocManager)
library(rhdf5)
library(tidyverse)
library(rgeos)
library(raster)
library(rgdal)
library(parallel)
library(data.table)
library(gtools)



#######################################################
##### code for bringing block to test functions on
#######################################################

hdf_path <- "/Volumes/jt/projects/margulis/swe/hdf/" #create path
hdf_name <- "SN_SWE_WY1986.h5" #create file name
hdf_file <- paste(hdf_path, hdf_name, sep="") #full path
h5ls(hdf_file) #list contains 3 groups. lat, long, and SWE 
h5closeAll()
h5readAttributes(hdf_file, name = "SWE") #$units = mm

#read in a block around sagehen, pulled from QGIS using the cell number rasters generated
system.time(sagehen_86 <- h5read(hdf_file, "/SWE", index = list(2523:2621, 2947:3172, 1:365))) #sagehen
class(sagehen_block) #inspect 
dim(sagehen_block) #dimensions


####################################################################################################
################ SCF
####################################################################################################


#set path and file name for hdf5 SWE file
hdf_path <- "/Volumes/jt/projects/margulis/swe/hdf/" #create path
path <-file.path( hdf_path , hdf_name ) 
h5closeAll()
h5readAttributes(hdf_file, name = "SWE") #$units = mm

# create snow cover frequency (scf) function
# number of days the amounts days there is snow on the ground vs total days
# set threshold to 2.54 mm or .1 inches beccause thats the accruarcy a SNTOEL site has
# this will also recude the long tails seen on the data and unrealistcally large scf numbers i saw
scf<-function(x){length(which(x>2.54))/length(x)} 

# function for creating annual scf rasters for the area around sagehen
scf_raster <- function( hdf_name ) {
  
  path <-file.path( hdf_path , hdf_name ) 
  
  #for the future change this to leap vs no leap
  sh_block <-h5read(path, "/SWE", index = list(2523:2621, 2947:3172, 1:365)) #load in 
  scf_mat <-as.matrix(apply(sh_block, c(1,2), scf)) #creat matrix with max value on z axis
  
  #convert matrix to raster, assign proper lat/lon bonds and CRS
  scf_rast <-raster(scf_mat, xmn=-120.355, xmx=-120.129, ymn=39.379, ymx=39.478, CRS("+proj=leac +ellps=clrk66"))
  plot(scf_rast)
  hist(scf_rast)
  rm(scf_mat) #remove old matrix from RAM
  
  #change name for saving
  name <- gsub(".h5", "", hdf_name)
  good_name <- gsub("SN_SWE_", "scf_sh_", name)
  
  setwd("/Volumes/jt/projects/margulis/sagehen/scf")
  writeRaster(scf_rast, paste0(good_name, ".tif"))
  return(scf_rast)
  h5closeAll()
}

############### apply to scf function to list of hdf swe files

setwd("/Volumes/jt/projects/margulis/swe/hdf")
files <- list.files(pattern = ".h5")
hdf_list <-mixedsort(sort(files)) #sort in correct order
print(hdf_list)

system.time(scf_list <-lapply(hdf_list, function(x) scf_raster(x)))

## we now have all the 32 annual rasters created
# now we bring load them back in to create a stack and do a pixelwise MK trend te

###### bring in full stack
setwd("/Volumes/jt/projects/margulis/sagehen/sagehen_scf/yearly_rasters")
list <-list.files()
scf_list <-lapply(list, function(x) raster(x))
scf_stack <-stack(scf_list) 
crs(scf_stack)<-"+proj=leac +ellps=clrk66"
plot(scf_stack[[9]])
scf_stack

###### mk test
# this is the full function which generates all 7 different layers 
trend.slope <- function(y, p.value.pass = TRUE, z.pass = TRUE, 
                        tau.pass = TRUE, confidence.pass = TRUE, intercept.pass = TRUE) {
  options(warn = -1)
  fit <- EnvStats::kendallTrendTest(y ~ 1)
  fit.results <- fit$estimate[2]
  if (tau.pass == TRUE) {
    fit.results <- c(fit.results, fit$estimate[1])
  }
  if (intercept.pass == TRUE) {
    fit.results <- c(fit.results, fit$estimate[3])
  }
  if (p.value.pass == TRUE) {
    fit.results <- c(fit.results, fit$p.value)
  }
  if (z.pass == TRUE) {
    fit.results <- c(fit.results, fit$statistic)
  }
  if (confidence.pass == TRUE) {
    ci <- unlist(fit$interval["limits"])
    if (length(ci) == 2) {
      fit.results <- c(fit.results, ci)
    }
    else {
      fit.results <- c(fit.results, c(NA, NA))
    }
  }
  options(warn = 0)
  return(fit.results)
}

# this one only does p-value and slope
trend.slope2 <- function(y, p.value.pass = TRUE, z.pass = FALSE, 
                         tau.pass = FALSE, confidence.pass = FALSE, intercept.pass = FALSE) {
  options(warn = -1)
  fit <- EnvStats::kendallTrendTest(y ~ 1)
  fit.results <- fit$estimate[2]
  if (tau.pass == TRUE) {fit.results <- c(fit.results, fit$estimate[1])}
  if (intercept.pass == TRUE) {fit.results <- c(fit.results, fit$estimate[3])}
  if (p.value.pass == TRUE) {fit.results <- c(fit.results, fit$p.value)}
  if (z.pass == TRUE) {fit.results <- c(fit.results, fit$statistic)}
  if (confidence.pass == TRUE) {ci <- unlist(fit$interval["limits"])
  if (length(ci) == 2) {fit.results <- c(fit.results, ci)}
  else {fit.results <- c(fit.results, c(NA, NA))}
  }
  options(warn = 0)
  return(fit.results)
}


# create a parralell cluster to run pixelwise mk analysis on scf
beginCluster(n=3)

system.time(mk_scf <- clusterR(scf_stack, overlay, args=list(fun=trend.slope2)))

endCluster()

# inspect
mk_scf
plot(mk_scf[[1]]) # plot slope
plot(mk_scf[[1]]) # plot p-value

# save slope
writeRaster(mk_scf[[1]], "/Volumes/jt/projects/margulis/sagehen/sagehen_scf/mk_results/scf_slope.tif") #save slope raster

# save p-value
writeRaster(mk_scf[[2]], "/Volumes/jt/projects/margulis/sagehen/sagehen_scf/mk_results/scf_p_value.tif") #save slope raster






####################################################################################################
################ SDD
####################################################################################################


hdf_path <- "/Volumes/jt/projects/margulis/swe/hdf/" #create path
path <-file.path( hdf_path , hdf_name ) 
h5closeAll()
h5readAttributes(hdf_file, name = "SWE") #$units = mm

#create sdd function which puts out dowy which snow disappears
sdd<-function(x){
  
  # use which function to index the days and give a list of all days that abide conidtion
  days_vector <-as.numeric(which(x > 2.54)) 
  sdd_dowy <-max(days_vector) #filter for max or last day meets conditons
  rm(days_vector) #remove full list
  return(sdd_dowy) #return dowy that snow is gone
  
}

# function for creating annual sdd rasters for the area around sagehen
sdd_raster <- function( hdf_name ) {
  
  path <-file.path( hdf_path , hdf_name ) 
  
  #for the future change this to leap vs no leap
  sh_block <-h5read(path, "/SWE", index = list(2523:2621, 2947:3172, 1:365)) #load in 
  sdd_mat <-as.matrix(apply(sh_block, c(1,2), sdd)) #creat matrix with max value on z axis
  
  #convert matrix to raster, assign proper lat/lon bonds and CRS
  sdd_rast <-raster(sdd_mat, xmn=-120.355, xmx=-120.129, ymn=39.379, ymx=39.478, CRS("+proj=leac +ellps=clrk66"))
  plot(sdd_rast)
  hist(sdd_rast)
  rm(sdd_mat) #remove old matrix from RAM
  
  #change name for saving
  name <- gsub(".h5", "", hdf_name)
  good_name <- gsub("SN_SWE_", "sdd_sh_", name)
  
  setwd("/Volumes/jt/projects/margulis/sagehen/sagehen_sdd/yearly_rasters/")
  writeRaster(sdd_rast, paste0(good_name, ".tif"))
  return(sdd_rast)
  h5closeAll()
}



############### apply to sdd function to list of hdf swe files

setwd("/Volumes/jt/projects/margulis/swe/hdf")
files <- list.files(pattern = ".h5")
hdf_list <-mixedsort(sort(files)) #sort in correct order
print(hdf_list)

system.time(sdd_list <-lapply(hdf_list, function(x) sdd_raster(x)))


###### bring in full stack
setwd("/Volumes/jt/projects/margulis/sagehen/sagehen_sdd/yearly_rasters/")
list <-list.files()
sdd_list <-lapply(list, function(x) raster(x))
sdd_stack <-stack(sdd_list) 
crs(sdd_stack)<-"+proj=leac +ellps=clrk66"
plot(sdd_stack[[9]])
sdd_stack


# create a parralell cluster to run pixelwise mk analysis on sdd
beginCluster(n=3)

system.time(mk_sdd <- clusterR(sdd_stack, overlay, args=list(fun=trend.slope2)))

endCluster()

# inspect
mk_sdd
plot(mk_sdd[[1]]) # plot slope
plot(mk_sdd[[1]]) # plot p-value

# save slope
writeRaster(mk_sdd[[1]], "/Volumes/jt/projects/margulis/sagehen/sagehen_sdd/mk_results/sdd_slope.tif") #save slope raster

# save p-value
writeRaster(mk_sdd[[2]], "/Volumes/jt/projects/margulis/sagehen/sagehen_sdd/mk_results/sdd_p_value.tif") #save slope raster


sdd_mat_93 <-apply(sagehen_93, c(1,2), sdd)

sdd_array <-as.array(sdd_stack)
pixel <-as.data.frame(sdd_array[70,30,1:32])
m <-as.data.frame(sagehen_86[30,30,1:365])
z <-as.data.frame(sagehen_86[70,30,1:365])

x <-as.array(sagehen_86[20,20,1:365])
x

hist(sagehen_86)
match('NA', sagehen_86)
'NA' %in% sagehen_86

x1 <-fourblock[1,1,1:365]
x
x2 <-fourblock[1,2,1:365]
x2
x <-fourblock[2,1,1:365]
x
x4 <-fourblock[2,2,1:365]
x4

sdd(x)


sdd<-function(x){
  
  # return 0 for values that never reach the 5.1 mm threshold
  if (max(x) < 5.1){
    return(0)
  } else {
  # use which function to index the days that meet condition
  # reverse the vector
  days_vector <-as.numeric(rev(which(x > 5.1)))
  
  # take the difference of each vector point and make positive
  # we do this bc we are looking for the first point that meets condition
  # and does this for 14 consecutive days
  diff_vect <-diff(days_vector*-1)

  # use rle function to calc the lengths of continuous number sequences
  # if there isnt a 14 days with continuous snow cover, make pixel 0
  # this fixes the code bc accounts for pixels which may have had 10 days of sc
  
  # which therefore was out of the previous condition set and caused the error
  # found this about by testing different areas, and saw was more in east half
  result <- rle(diff_vect)
  if (max(result$lengths) < 14){
    return(0)
  } else{
  #represents place on rle vector where condition is met
  meets <-as.numeric(min(which(result$lengths >= 14)))

  #if its the first rle value, this means day 365 so max of days_vecto0r
  if (meets == 1) {
    sdd1 <- max(days_vector)
    return(sdd1)
      } else {
        #calculate legnth needed to be cut off days vector from rle
        values_to_cut <-result$lengths[1:(meets-1)]
        #sum the lengths
        index_values_until_start <-as.numeric(sum(values_to_cut))
        #subtract that many values off to get your SDD
        sdd2 <-max(days_vector[-c(1:index_values_until_start)])
        return(sdd2)
    }
  }
 }
}

testing <-as.array(sagehen_86[1:99,1:200,1:365])
sdd_mat_86 <-as.matrix(apply(sagehen_86, c(1,2), sdd))

sdd_test <-raster(sdd_mat_86) 
crs(sdd_test)<-"+proj=leac +ellps=clrk66"
plot(sdd_stack[[9]])
sdd_test
hist(sdd_test)
plot(sdd_test)

hist(testing)
warnings()

length(336:365)
days_vector
sdd_dowy <-max(days_vector) #filter for max or last day meets conditons
sdd_dowy
rm(days_vector) #remove full list
return(sdd_dowy) #return dowy that snow is gone



########################################################################### crop method max sagehen raster


# bring in full stack for max
setwd("/Volumes/jt/projects/margulis/max_rasters/")
list <-list.files()
max_list <-lapply(list, function(x) raster(x))
max_stack <-stack(max_list) 
crs(max_stack)<-"+proj=leac +ellps=clrk66"
plot(max_stack[[9]])
max_stack

# create test block for sagehen
sagehen_brick <-crop(max_stack, extent(-120.355, -120.129, 39.379, 39.478)) # crop to box defined in Qgis
sagehen <-stack(sagehen_brick) # convert from brick to stack
sagehen # inspect
plot(sagehen[[31]]) # plot single year
writeRaster(sagehen, "/Volumes/jt/projects/margulis/sagehen/sagehen_stack/sagehen_stack.tif") #save stack in new foolder 

# load stack in to see if works
sagehen <-stack("/Volumes/jt/projects/margulis/sagehen/sagehen_stack/sagehen_stack.tif")
crs(sagehen)<-"+proj=leac +ellps=clrk66"

###### mk test
# this is the full function which generates all 7 different layers 
trend.slope <- function(y, p.value.pass = TRUE, z.pass = TRUE, 
                        tau.pass = TRUE, confidence.pass = TRUE, intercept.pass = TRUE) {
  options(warn = -1)
  fit <- EnvStats::kendallTrendTest(y ~ 1)
  fit.results <- fit$estimate[2]
  if (tau.pass == TRUE) {
    fit.results <- c(fit.results, fit$estimate[1])
  }
  if (intercept.pass == TRUE) {
    fit.results <- c(fit.results, fit$estimate[3])
  }
  if (p.value.pass == TRUE) {
    fit.results <- c(fit.results, fit$p.value)
  }
  if (z.pass == TRUE) {
    fit.results <- c(fit.results, fit$statistic)
  }
  if (confidence.pass == TRUE) {
    ci <- unlist(fit$interval["limits"])
    if (length(ci) == 2) {
      fit.results <- c(fit.results, ci)
    }
    else {
      fit.results <- c(fit.results, c(NA, NA))
    }
  }
  options(warn = 0)
  return(fit.results)
}

# this one only does p-value and slope
trend.slope2 <- function(y, p.value.pass = TRUE, z.pass = FALSE, 
                           tau.pass = FALSE, confidence.pass = FALSE, intercept.pass = FALSE) {
    options(warn = -1)
    fit <- EnvStats::kendallTrendTest(y ~ 1)
    fit.results <- fit$estimate[2]
    if (tau.pass == TRUE) {fit.results <- c(fit.results, fit$estimate[1])}
    if (intercept.pass == TRUE) {fit.results <- c(fit.results, fit$estimate[3])}
    if (p.value.pass == TRUE) {fit.results <- c(fit.results, fit$p.value)}
    if (z.pass == TRUE) {fit.results <- c(fit.results, fit$statistic)}
    if (confidence.pass == TRUE) {ci <- unlist(fit$interval["limits"])
    if (length(ci) == 2) {fit.results <- c(fit.results, ci)}
    else {fit.results <- c(fit.results, c(NA, NA))}
    }
    options(warn = 0)
    return(fit.results)
  }
  

# create a parralell cluster to run pixelwise mk analysis on max SWE
beginCluster(n=3)

system.time(sagehen_max <- clusterR(sagehen, overlay, args=list(fun=trend.slope2)))

endCluster()

# inspect
sagehen_max
plot(sagehen_max[[1]]) # plot slope
plot(sagehen_max[[1]]) # plot p-value

# save slope
writeRaster(sagehen_max[[1]], "/Volumes/jt/projects/margulis/sagehen/sagehen_max/sagehen_slope.tif") #save slope raster

# save p-value
writeRaster(sagehen_max[[2]], "/Volumes/jt/projects/margulis/sagehen/sagehen_max/sagehen_p_value.tif") #save slope raster


