# july 22nd 2021
# getting back into this stuff after meeting with adrian
# doing a whole data set check of variation in amount/magnitude of trends for n vs. s slopes

library(tidyverse)
library(rgeos)
library(terra)
library(rgdal)
library(data.table)

######## scf
## bring in raw pval and slope data
# pval
scf_pval <-rast("/Volumes/jt/projects/margulis/snow_metric_rasters/scf/mk_results/scf_p_value_full.tif")
crs(scf_pval)<-"+proj=latlon +ellps=clrk66"
plot(scf_pval) # test plot, no need to correct
hist(scf_pval)
freq(scf_pval, digits = 1)

# count valid pixels in pval data
p_number_pixels <-scf_pval # create dummy rast
p_number_pixels[!is.na(p_number_pixels)] = 1 # if NOT NA value = 1
plot(p_number_pixels)
freq(p_number_pixels)
p_number_pixels[is.na(p_number_pixels)] = 0 # if NA = 0
global(p_number_pixels, sum) # 4629320 pixels
writeRaster(p_number_pixels, "/Volumes/jt/projects/margulis/snow_metric_rasters/scf/mk_results/new_bin_scf_pval.tiff")

# slope
scf_slope <-rast("/Volumes/jt/projects/margulis/snow_metric_rasters/scf/mk_results/scf_slope_full.tif")
crs(scf_slope)<-"+proj=latlon +ellps=clrk66"
values(scf_slope)[values(scf_slope) == 0.000] = NA # convert 0 to NA
freq(scf_slope, digits = 2)
plot(scf_slope)
hist(scf_slope)

# count valid slope pixels
number_pixels <-scf_slope # create dummy rast
number_pixels[!is.na(number_pixels)] = 1 # if NOT NA value = 1
plot(number_pixels)
global(number_pixels, sum) # 4629320 pixels
writeRaster(number_pixels, "/Volumes/jt/projects/margulis/snow_metric_rasters/scf/mk_results/new_bin_scf_slope.tiff")


# create mask for significant pval pixels
scf_sig_pval <-scf_pval
values(scf_sig_pval)[values(scf_sig_pval) > 0.05] = NA
plot(scf_sig_pval)
writeRaster(scf_sig_pval, "/Volumes/jt/projects/margulis/snow_metric_rasters/scf/mk_results/scf_sig_pval.tiff")

# mask slope of significant pixels
scf_sig_slope <-mask(scf_slope, scf_sig_pval)
plot(scf_sig_slope)
writeRaster(scf_sig_slope, "/Volumes/jt/projects/margulis/snow_metric_rasters/scf/mk_results/scf_sig_slope.tiff")

?terra

full_slope<-as.data.frame(freq(scf_slope, digits = 3))
sig_slope_<-as.data.frame(freq(sig_slope, digits = 3))


scf_mean<-cellStats(sig_slope, stat='mean', na.rm=TRUE)
scf_mean*100

scf_stack <-stack(sig_pval, sig_ele, sig_slope)
scf_stack*100

num_slopes <-sum(full_slope$count) - 33002982
num_sig_slopes <-sum(sig_slope$count) - 32681194
positive_sig_slope <- 62
percent_pos <-(positive_sig_slope/num_sig_slopes)*100
100-percent_pos

plot(scf_stack)


values(sig_ele)[values(sig_ele) <= 0] = NA
hist(sig_ele)


values(sig_slope)[values(sig_slope) == 0.000] = NA
hist(sig_slope)
freq(sig_slope, digits = 3)

plot(values(sig_slope), values(sig_ele))
freq(sig_slope, digits = 3)


####### scf

# convert raster values to dataframe
sig_points <- as.data.frame(rasterToPoints(sig_slope)) 
ele_points <-as.data.frame(rasterToPoints(sig_ele))

# bind together the sig_slope with sig_ele
points <-cbind(sig_points, ele_points$sig_ele)

# rename columns
names(points_no_0)[3] <- "slope"
names(points_no_0)[4] <- "ele"

# filter out 0 ele
points_no_0 <-filter(points, ele >= 1)

# filter out 0 slope
points_no_0 <-filter(points_no_0, slope != 0)

# plot in bins density of points
ggplot(points_no_0, aes(slope, ele))+
  geom_bin2d(bins = 70) +
  scale_fill_continuous(type = "viridis") +
  theme_light()

#same thing but hex plot
ggplot(points_no_0, aes(slope, ele))+
  geom_hex(bins = 27) +
  scale_fill_gradient(low = "gray90", high = "darkred")+
  theme_light()

# contours
ggplot(points_no_0, aes(slope, ele)) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", color = "white")+
  theme_light()

# contours another way
ggplot(points_no_0, aes(slope, ele)) +
  geom_density_2d_filled(alpha = 1)
