# oct 7th 2020
# start of pixel wise sens slop analysis 
# need to firgure out the  best way to cut full year vertical columns out for anlaysis and stich them back

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

setwd("/Volumes/JTARRICONE/UNR_summer_20/margulis_data/swe/code_test")
edit_r_environ()

#####################################################

### first steps
# show proof concept for method wiht smaller file size

#set path and file name for hdf5 SWE file
hdf_path <- "/Volumes/jt/projects/margulis/swe/hdf/" #create path
hdf_name <- "SN_SWE_WY2016.h5" #create file name
hdf_file <- paste(hdf_path, hdf_name, sep="") #full path
h5ls(hdf_file) #list contains 3 groups. lat, long, and SWE 
h5closeAll()
h5readAttributes(hdf_file, name = "SWE") #$units = mm

# full raster with just three days
#read in SWE data which is an array, or stack of matrixes
swe_2016 <- h5read(hdf_file, "/SWE", index = list(1:6601, 1:5701, 180:182)) #read in SWE group
class(swe_2016) #inspect 
dim(swe_2016) #dimensions

swe_brick <-brick(swe_2016, xmn=-123.3, xmx=-117.6, ymn=35.4, ymx=42,CRS("+proj=leac +ellps=clrk66"))
swe_brick[ swe_brick[] == -32768 ] <- NA
swe_brick

max_test <-stackApply(swe_brick, max, indices = rep(1,nlayers(swe_brick)), na.rm=TRUE)
plot(max_test)
hist(max_test)

# 100x100x366 test, full year
test_block<- h5read(hdf_file, "/SWE", index = list(4243:4252, 4175:4164, 1:366)) # read in array
test_brick <-brick(test_block, xmn=-123.3, xmx=-117.6, ymn=35.4, ymx=42, CRS("+proj=leac +ellps=clrk66")) # convert to brick
maxi <-stackApply(test_brick, max, indices = rep(1,nlayers(swe_brick)), na.rm=TRUE) # create pixel wise max raster
plot(maxi)
hist(maxi)

# creat row block sequence
seq(0, 6600, by=300 )

b1test <-(3301:3600)
test_block<- h5read(hdf_file, "/SWE", index = list(b1test, NULL, NULL))
test_block[ test_block[] == -32768 ] <- NA
test_brick <-brick(test_block, xmn=-123.3, xmx=-117.6, ymn=41.7, ymx=42, CRS("+proj=leac +ellps=clrk66")) 
maxi <-stackApply(test_brick, max, indices = rep(1,nlayers(swe_brick)), na.rm=TRUE) # create pixel wise max raster
plot(maxi)

#############################################



hdf_path2 <- "/Volumes/jt/projects/margulis/swe/hdf/" #create path
hdf_name2 <- "SN_SWE_WY1993.h5" #create file name
hdf_file2 <- paste(hdf_path2, hdf_name2, sep="") #full path
h5ls(hdf_file2) #list contains 3 groups. lat, long, and SWE 
h5closeAll()
h5readAttributes(hdf_file2, name = "SWE") #$units = mm


b1test <-(3301:3600)
test_block2<- h5read(hdf_file2, "/SWE", index = list(b1test, NULL, NULL))
test_block2[ test_block2[] == -32768 ] <- NA
test_brick2 <-brick(test_block2, xmn=-121.3, xmx=-119.6, ymn=41.7, ymx=42, CRS("+proj=leac +ellps=clrk66")) 
maxi2 <-stackApply(test_brick2, max, indices = rep(1,nlayers(test_brick2)), na.rm=TRUE) # create pixel wise max raster
plot(maxi2)

chunk1 <- h5read(hdf_file2, "/SWE", index = list(1:2000, NULL, 220))
chunk2 <- h5read(hdf_file2, "/SWE", index = list(2001:4000, NULL, 220))
chunk3 <- h5read(hdf_file2, "/SWE", index = list(4001:6000, NULL, 220))
chunk4 <- h5read(hdf_file2, "/SWE", index = list(6001:6601, NULL, 220))

chunk1 <- as.matrix(chunk1[,,1])
chunk2 <- as.matrix(chunk2[,,1])
chunk3 <- as.matrix(chunk3[,,1])
chunk4 <- as.matrix(chunk4[,,1])


rast1 <-raster(chunk1, xmn=-123.3, xmx=-117.6, ymn=41.7, ymx=42, CRS("+proj=leac +ellps=clrk66"))
rast2 <-raster(chunk2, xmn=-123.3, xmx=-117.6, ymn=41.7, ymx=42, CRS("+proj=leac +ellps=clrk66"))
rast3 <-raster(chunk3, xmn=-123.3, xmx=-117.6, ymn=41.7, ymx=42, CRS("+proj=leac +ellps=clrk66"))
rast4 <-raster(chunk4, xmn=-123.3, xmx=-117.6, ymn=41.7, ymx=42, CRS("+proj=leac +ellps=clrk66"))

plot(rast1)
plot(rast2)
plot(rast3)
plot(rast4)

# https://stackoverflow.com/questions/15876591/merging-multiple-rasters-in-r
# https://tengkengvang.com/2018/11/12/mosaic-or-merge-rasters-in-r/

setwd("/Volumes/jt/projects/margulis/swe/code_test")
yut<-merge(rast1,rast2)
plot(yut)

all_my_rasts <- c(rast1, rast2, rast3, rast4)
e <- extent(-123.3, -117.6, 41.7, 42)
template <- raster(e)
projection(template) <- CRS("+proj=leac +ellps=clrk66")
writeRaster(template, file="MyBigNastyRasty.tif", format="GTiff")

mosaic_rasters(gdalfile=all_my_rasts, dst_dataset="MyBigNastyRasty.tif", of="GTiff")
gdalinfo("MyBigNastyRasty.tif")


setwd("D:/Raster")
a <- c('1.tif', '2.tif','3.tif','4.tif')
e <- extent(-85, -83, 39, 41)
template <- raster(e)
proj4string(template) <- CRS("+init=epsg:4269")
writeRaster(template, file="MiamiWatershed.tif", format="GTiff")
mosaic_rasters(gdalfile=a,dst_dataset="MiamiWatershed.tif",of="GTiff")
gdalinfo("MiamiWatershed.tif")


canProcessInMemory(rast1, verbose = TRUE)




b1 <-("1:300")
b2 <-(301:600)
b3 <-(601:900)
b4 <-(901:1200)
b5 <-(1201:1500)
b6 <-(1501:1800)
b7 <-(1801:2100)
b8 <-(2101:2400)
b9 <-(2401:2700)
b10 <-(2701:3000)
b11 <-(3001:3300)
b12 <-(3301:3600)
b13 <-(3601:3900)
b14 <-(3901:4200)
b15 <-(4201:4500)
b16 <-(4501:4800)
b17 <-(4801:5100)
b18 <-(5101:5400)
b19 <-(5401:5700)
b20 <-(5701:6000)
b21 <-(6001:6300)
b22 <-(6301:6601)

blocklist <-list(b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12,b13,b14,b15,b16,b17,b18,b19,b20,b21,b22)

#2100 2400 2700 3000 3300 3600 3900 4200 4500 4800 5100 5400 5700 6000 6300 6600

library(raster)
sca <- brick(nrow=108,ncol=132,nl=365) 
values(sca) <- runif(ncell(sca)*nlayers(sca))

i <- rep(1:ceiling(365/8), each=8)
i <- i[1:nlayers(sca)]

for (j in unique(i)) {
  x <- sca[[which(j==i)]]
  xx <- max(x, na.rm=TRUE)
  # or
  # xx <- calc(x, fun=max, na.rm=TRUE, filename = patste0(i, '.tif'))
}

#After several trials this seems to speed up my task 10 times: 
  rasterOptions(format="CDF",overwrite=TRUE,maxmemory = 1e+09, 
                chunksize=1e+08,progress="text",tmpdir="C:/DATA/mydata") rasterTmpFile("clean_this_after_")

open_hdf_as_raster <- function( hdf_name ) {
  
 hdf_name %>%
    file.path( hdf_path , . ) %>%
    h5read(., "/SWE", index = list(NULL,NULL,1:50)) %>%
    brick(., xmn=-123.3, xmx=-117.6, ymn=35.4, ymx=42,CRS("+proj=leac +ellps=clrk66")) %>%
    as.list(.)
  }


####################################################





# oct 12
# keeping data in a matrix instead of raster for processing
# this way we can avoid geolocation issues

"/Volumes/jt/projects/margulis/temp" <-tempdir()

#set path and file name for hdf5 SWE file
hdf_path <- "/Volumes/jt/projects/margulis/swe/hdf/" #create path
hdf_name <- "SN_SWE_WY2016.h5" #create file name
hdf_file <- paste(hdf_path, hdf_name, sep="") #full path
h5ls(hdf_file) #list contains 3 groups. lat, long, and SWE 
path <-file.path( hdf_path , hdf_name ) 
h5closeAll()
h5readAttributes(hdf_file, name = "SWE") #$units = mm


max_raster <- function( hdf_name ) {
  
    path <-file.path( hdf_path , hdf_name ) 
    
    c1 <-h5read(path, "/SWE", index = list(1:1000,1:5701,1:365)) #load in 
    c1[ c1[] == -32768 ] <- NA #remove NA
    max_c1 <-as.matrix(apply(c1, c(1,2), max)) #creat matrix with max value on z axis
    rm(c1) 
    
    c2 <-h5read(path, "/SWE", index = list(1001:2000,1:5701,1:365))
    c2[ c2[] == -32768 ] <- NA
    max_c2 <-as.matrix(apply(c2, c(1,2), max))
    rm(c2)
    
    c3 <-h5read(path, "/SWE", index = list(2001:3000,1:5701,1:365))
    c3[ c3[] == -32768 ] <- NA
    max_c3 <-as.matrix(apply(c3, c(1,2), max))
    rm(c3)
    
    c4 <-h5read(path, "/SWE", index = list(3001:4000,1:5701,1:365))
    c4[ c4[] == -32768 ] <- NA
    max_c4 <-as.matrix(apply(c4, c(1,2), max))
    rm(c4)
    
    c5 <-h5read(path, "/SWE", index = list(4001:5000,1:5701,1:365))
    c5[ c5[] == -32768 ] <- NA
    max_c5 <-as.matrix(apply(c5, c(1,2), max))
    rm(c5)
    
    c6 <-h5read(path, "/SWE", index = list(5001:6000,1:5701,1:365))
    c6[ c6[] == -32768 ] <- NA
    max_c6 <-as.matrix(apply(c6, c(1,2), max))
    rm(c6)
    
    c7 <-h5read(path, "/SWE", index = list(6001:6601,1:5701,1:365))
    c7[ c7[] == -32768 ] <- NA
    max_c7 <-as.matrix(apply(c7, c(1,2), max))
    rm(c7)

    #bind chunks together
    full_max <-rbind(max_c1,max_c2,max_c3,max_c4,max_c5,max_c6,max_c7)
    rast <-raster(full_max, xmn=-123.3, xmx=-117.6, ymn=35.4, ymx=42, CRS("+proj=leac +ellps=clrk66"))
    plot(rast)
    
    name <- gsub(".h5", "", hdf_name)
    good_name <- gsub("SN_SWE_", "max_swe_", name)
    
    setwd("/Volumes/jt/projects/margulis/max_rasters/")
    writeRaster(rast, paste0(good_name, ".tif"))
    return(rast)
    }

max_raster(hdf_name) 

#### apply to hdf list

setwd("/Volumes/jt/projects/margulis/swe/hdf")
files <- list.files(pattern = ".h5")
hdf_list <-mixedsort(sort(files)) #sort in correct order
print(hdf_list)

system.time(results_list <-mclapply(hdf_list, function(x) max_raster(x), mc.cores = 3, mc.cleanup = TRUE))



data <-open_hdf_as_raster(hdf_name)
chunk[ chunk[] == -32768 ] <- NA
max_value <-as.matrix(apply(data, c(1,2), max))

slice <-(chunk[,,180])
rast1 <-raster(test, xmn=-123.3, xmx=-117.6, ymn=41.7, ymx=42, CRS("+proj=leac +ellps=clrk66"))
plot(rast1)
rast1
