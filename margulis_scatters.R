# january 12th 2020
# creating scatter plot/heat maps for signicicant trends vs elevation
# save this script this time jack...

library(tidyverse)
library(rgeos)
library(raster)
library(rgdal)
library(data.table)
library(gtools)
library(spatialEco)
library(EnvStats)
library(viridis)
library(gridExtra)


#######################
## load in static stack and make singles
#######################

# dem
snsr_dem <-raster("/Volumes/jt/projects/margulis/static/rasters/SNSR_DEM.tif")
crs(snsr_dem)<-"+proj=leac +ellps=clrk66" 

# aspect
aspect <-raster("/Volumes/jt/projects/margulis/static/rasters/SNSR_aspect.tif")
crs(aspect)<-"+proj=leac +ellps=clrk66" 
plot(aspect)

#bin the 4 aspects
sdd_aspect_points <-as.data.frame(rasterToPoints(sdd_sig_aspect))
sdd_aspect_points$SNSR_aspect <-replace(sdd_aspect_points$SNSR_aspect, 45 < sdd_aspect_points$SNSR_aspect & 135 > sdd_aspect_points$SNSR_aspect, "E")
sdd_aspect_points$SNSR_aspect <-replace(sdd_aspect_points$SNSR_aspect, 135 < sdd_aspect_points$SNSR_aspect & 225 > sdd_aspect_points$SNSR_aspect, "S")
sdd_aspect_points$SNSR_aspect <-replace(sdd_aspect_points$SNSR_aspect, 225 < sdd_aspect_points$SNSR_aspect & 315 > sdd_aspect_points$SNSR_aspect, "W")
sdd_aspect_points$SNSR_aspect <-replace(sdd_aspect_points$SNSR_aspect, 315 < sdd_aspect_points$SNSR_aspect & 360 > sdd_aspect_points$SNSR_aspect, "N")
sdd_aspect_points$SNSR_aspect <-replace(sdd_aspect_points$SNSR_aspect, 45 > sdd_aspect_points$SNSR_aspect, "N")


###########################################
############# SCF scatter plot creation
###########################################

# bring in full pval data
scf_sig_pval <-raster("/Volumes/jt/projects/margulis/snow_metric_rasters/scf/mk_results/scf_p_value_full.tif")
crs(scf_sig_pval)<-"+proj=leac +ellps=clrk66" 
values(scf_sig_pval)[values(scf_sig_pval) > 0.05] = NA # change anything above .05 to NA
plot(scf_sig_pval)

# bring in slope
scf_sig_slope <-raster("/Volumes/jt/projects/margulis/snow_metric_rasters/scf/mk_results/scf_slope_full.tif")
crs(scf_sig_slope)<-"+proj=leac +ellps=clrk66"

#mask slope with signicant p values
scf_sig_slope <- mask(scf_sig_slope, scf_sig_pval, maskvalue = NA)
plot(scf_sig_ele)

#mask elevtion with signicant p values
scf_sig_ele <- mask(snsr_dem, scf_sig_pval, maskvalue = NA)
plot(scf_sig_ele)

# transform raster into data frames 
scf_ele_points <-as.data.frame(rasterToPoints(scf_sig_ele))
scf_slope_points <-as.data.frame(rasterToPoints(scf_sig_slope))
scf_pval_points <-as.data.frame(rasterToPoints(scf_sig_pval))

# bind columns
scf_points_raw <-cbind(scf_ele_points, scf_slope_points$scf_slope_full, scf_pval_points$scf_p_value_full)
colnames(scf_points_raw)[3] <- "elevation" #change binded col name
colnames(scf_points_raw)[4] <- "slope" #change binded col name
colnames(scf_points_raw)[5] <- "pval" #change binded col name


# filter out 0 elevation values
###### i'm not sure if this is right, ask adrian
###### but it seems that a slope needs a posative or negative value to be significant 

scf_points <-filter(scf_points_raw, slope !=  "0") # one represents a significant value
scf_points$slope <-(scf_points$slope*100)

hist(scf_points$elevation)
hist(scf_points$slope)
zero_test <-filter(scf_points, slope ==  "0")

# plot in a hex heat map
theme_set(theme_light())

scf_ele <-ggplot(scf_points, aes(slope, elevation)) +
  geom_hex() +
  scale_fill_viridis(option="magma")+
  #scale_fill_gradient(low = "grey96", high = "darkred") +
  labs(title = "SCF Significant Pixels vs Elevation",
       x = "Trend Magnitude (%/year)",
       y = "Elevation (m)")
print(scf_ele)

#lat
scf_lat <-ggplot(scf_points, aes(slope, y)) +
  geom_hex() +
  scale_fill_viridis(option="magma")+
  #scale_fill_gradient(low = "grey94", high = "darkred") +
  labs(title = "SCF Significant Pixels vs Latitude",
       x = "Trend Magnitude (%/year)",
       y = "Latitude (degrees)")
print(scf_lat)

#lon
scf_lon <-ggplot(scf_points, aes(x, slope)) +
  geom_hex() +
  scale_fill_viridis(option="magma")+
  #scale_fill_gradient(low = "grey94", high = "darkred") +
  labs(title = "SCF Significant Pixels vs Longitude",
       y = "Trend Magnitude (%/year)",
       x = "Longitude (degrees)")
print(scf_lon)



###########################################
############# SDD scatter plot creation
###########################################

# bring in full pval data
sdd_sig_pval <-raster("/Volumes/jt/projects/margulis/snow_metric_rasters/sdd/mk_results/sdd_p_value_full.tif")
crs(sdd_sig_pval)<-"+proj=leac +ellps=clrk66" 
values(sdd_sig_pval)[values(sdd_sig_pval) > 0.05] = NA # change anything above .05 to NA

# bring in slope
sdd_sig_slope <-raster("/Volumes/jt/projects/margulis/snow_metric_rasters/sdd/mk_results/sdd_sig_slope.tif")
crs(sdd_sig_slope)<-"+proj=leac +ellps=clrk66"

#mask slope with signicant p values
sdd_sig_slope <- mask(sdd_sig_slope, sdd_sig_pval, maskvalue = NA)
plot(sdd_sig_ele)

#mask elevtion with signicant p values
sdd_sig_ele <- mask(snsr_dem, sdd_sig_pval, maskvalue = NA)
plot(sdd_sig_ele)



#mask elevtion with signicant p values
#sdd_sig_aspect <- mask(aspect, sdd_sig_pval, maskvalue = NA)
#plot(sdd_sig_aspect)
#hist(sdd_sig_aspect)
#aspect_freq <-as.data.frame(freq(sdd_sig_aspect, digits = 0))

# assign NA to all zero values for significant elevations
values(sdd_sig_ele)[values(sdd_sig_ele) <= 0] = NA
hist(sdd_sig_ele)
sdd_ele_freq <-as.data.frame(freq(sdd_sig_ele))

# assign NA to all zero values for significant slope
#values(sdd_sig_slope)[values(sdd_sig_slope) == 0.0000] = NA
hist(sdd_sig_slope)
sdd_slope_freq <-as.data.frame(freq(sdd_sig_slope, digits=2))

# assign NA to all zero values for significant slope
#values(sdd_sig_slope)[values(sdd_sig_slope) == 0.0000] = NA
hist(sdd_pval)
plot(sdd_pval)

# turn all not significant values to NA
sdd_pval_freq <-as.data.frame(freq(sdd_sig_pval, digits=6))
plot(sdd_sig_pval)
plot(sdd_sig_slope)

# transform raster into data frames 
# sdd_aspect_points <-as.data.frame(rasterToPoints(sdd_sig_aspect))
sdd_ele_points <-as.data.frame(rasterToPoints(sdd_sig_ele))
sdd_slope_points <-as.data.frame(rasterToPoints(sdd_sig_slope))
sdd_pval_points <-as.data.frame(rasterToPoints(sdd_sig_pval))

# bind columns
sdd_points_raw <-cbind(sdd_ele_points, sdd_slope_points$sdd_sig_slope, 
                       sdd_pval_points$sdd_p_value_full) #, sdd_aspect_points$SNSR_aspect)

colnames(sdd_points_raw)[3] <- "elevation" #change binded col name
colnames(sdd_points_raw)[4] <- "slope" #change binded col name
colnames(sdd_points_raw)[5] <- "pval" #change binded col name
#colnames(sdd_points_raw)[6] <- "aspect" #change binded col name


# filter out 0 elevation values
###### i'm not sure if this is right, ask adrian
###### but it seems that a slope needs a posative or negative value to be significant 

sdd_points <-filter(sdd_points_raw, slope !=  "0") # one represents a significant value
hist(sdd_points$elevation)
hist(sdd_points$slope)

zero_test <-filter(sdd_points, slope ==  "0")

# plot in a hex heat map
theme_set(theme_light())

# elevation
sdd_ele <-ggplot(sdd_points, aes(slope, elevation)) +
  geom_hex() +
  scale_fill_viridis(option="magma")+
  #scale_fill_gradient(low = "grey96", high = "darkred") +
  labs(title = "SDD Significant Pixels vs Elevation",
       x = "Trend Magnitude (days/year)",
       y = "Elevation (m)")
print(sdd_ele)

#lat
sdd_lat <-ggplot(sdd_points, aes(slope, y)) +
  geom_hex() +
  scale_fill_viridis(option="magma")+
  #scale_fill_gradient(low = "grey94", high = "darkred") +
  labs(title = "SDD Significant Pixels vs Latitude",
       x = "Trend Magnitude (days/year)",
       y = "Latitude (degrees)")
print(sdd_lat)

#lon
sdd_lon <-ggplot(sdd_points, aes(x, slope)) +
  geom_hex() +
  scale_fill_viridis(option="magma")+
  #scale_fill_gradient(low = "grey94", high = "darkred") +
  labs(title = "SDD Significant Pixels vs Longitude",
       y = "Trend Magnitude (days/year)",
       x = "Longitude (degrees)")
print(sdd_lon)






###########################################
############# max scatter plot creation
###########################################

#bring in full pval raster and mask it for significance
max_slope <-raster("/Volumes/jt/projects/margulis/snow_metric_rasters/max_swe/mk_results/max_slope_full.tif")
crs(max_slope)<-"+proj=leac +ellps=clrk66"
plot(max_slope)

#bring in full pval raster and mask it for significance
max_sig_pval <-raster("/Volumes/jt/projects/margulis/snow_metric_rasters/max_swe/mk_results/max_p_value_full.tif")
crs(max_sig_pval)<-"+proj=leac +ellps=clrk66"

#bring in full pval raster and mask it for significance
max_sig_pval <-raster("/Volumes/jt/projects/margulis/snow_metric_rasters/max_swe/mk_results/max_p_value_full.tif")
crs(max_sig_pval)<-"+proj=leac +ellps=clrk66" 
values(max_sig_pval)[values(max_sig_pval) > 0.05] = NA # all values > .05 now NA


#mask elevtion with signicant p values
max_sig_ele <- mask(snsr_dem, max_sig_pval, maskvalue = NA)
plot(max_sig_ele)

#mask slope with signicant p values
max_sig_slope <- mask(max_slope, max_sig_pval, maskvalue = NA)
plot(max_sig_slope)

# transform raster into data frames 
max_ele_points <-as.data.frame(rasterToPoints(max_sig_ele))
max_slope_points <-as.data.frame(rasterToPoints(max_sig_slope))
max_pval_points <-as.data.frame(rasterToPoints(max_sig_pval))

# bind columns
max_points_raw <-cbind(max_ele_points, max_slope_points$max_slope_full, 
                       max_pval_points$max_p_value_full)

colnames(max_points_raw)[3] <- "elevation" #change binded col name
colnames(max_points_raw)[4] <- "slope" #change binded col name
colnames(max_points_raw)[5] <- "pval" #change binded col name
#colnames(max_points_raw)[6] <- "aspect" #change binded col name

#test histograms

hist(max_points_raw$elevation)
hist(max_points_raw$slope, breaks = 60)

summary(max_points_raw$slope)

# filter out 0 elevation values
###### i'm not sure if this is right, ask adrian
###### but it seems that a slope needs a posative or negative value to be significant 

max_points <-filter(max_points_raw, slope !=  "0") # one represents a significant value
hist(max_points$elevation)
hist(max_points$slope)

zero_test <-filter(max_points, slope ==  "0")

# plot in a hex heat map
theme_set(theme_light())

# elevation
max_ele <-ggplot(max_points, aes(slope, elevation)) +
  geom_hex() +
  scale_fill_viridis(option="magma")+
  #scale_fill_gradient(low = "grey96", high = "darkred") +
  labs(title = "Max SWE Significant Pixels vs Elevation",
       x = "Trend Magnitude (cm/decade)",
       y = "Elevation (m)")
print(max_ele)

#lat
max_lat <-ggplot(max_points, aes(slope, y)) +
  geom_hex() +
  scale_fill_viridis(option="magma")+
  #scale_fill_gradient(low = "grey94", high = "blue4") +
  labs(title = "Max SWE Significant Pixels vs Latitude",
       x = "Trend Magnitude (cm/decade)",
       y = "Latitude (degrees)")
print(max_lat)

#lon
max_lon <-ggplot(max_points, aes(x, slope)) +
  geom_hex() +
  scale_fill_viridis(option="magma")+
  #scale_fill_gradient(low = "grey94", high = "blue4") +
  labs(title = "Max SWE Significant Pixels vs Longitude",
       y = "Trend Magnitude (cm/decade)",
       x = "Longitude (degrees)")
print(max_lon)

#########create facet grids

# create name col for max
#max_points <-filter(max_points, slope < 5) # clean data so doesn't have high outlier
max_swe <-replicate(length(max_points$slope), "Max SWE")
max_points <-cbind(max_points, max_swe) # bind it
hist(max_points$slope)
colnames(max_points)[6] <- "metric" #change binded col name


#create name col for sdd
sdd <-replicate(length(sdd_points$elevation), "SDD")
sdd_points <-cbind(sdd_points, sdd) # bind it
colnames(sdd_points)[6] <- "metric" #change binded col name

#create name col for scf
scf <-replicate(length(scf_points$elevation), "SCF")
scf_points <-cbind(scf_points, scf) # bind it
colnames(scf_points)[6] <- "metric" #change binded col name

# bind all three metrics with thier newly named columns together

sdd <-replicate(length(sdd_points$elevation), "SDD")
sdd_points <-cbind(sdd_points, sdd) # bind it
colnames(sdd_points)[6] <- "metric" #change binded col name
write.csv(sdd_points, "/Volumes/jt/projects/margulis/scatter_plots/sdd_results.csv")

mk_results <-rbind(sdd_points, scf_points, max_points)
mk_results <-read.csv("/Volumes/jt/projects/margulis/scatter_plots/full_mk_results.csv")
write.csv(mk_results, "/Volumes/jt/projects/margulis/scatter_plots/full_mk_results.csv")

## facet using grid.arrange to keep count scale independent

mk_results <-read.csv("/Volumes/jt/projects/margulis/scatter_plots/full_mk_results.csv")

### elev
ele_list <-list(max_ele, sdd_ele, scf_ele, max_lat, sdd_lat, scf_lat, max_lon, scf_lon, sdd_lon) # create list of all elevation plots
do.call(grid.arrange, c(ele_list, ncol=3, nrow=3)) # grid arrange





# elevation
#ggplot(mk_results, aes(slope, elevation)) + facet_wrap(~metric, scales = "free_x") +
  #stat_density2d(aes(fill=..level..),geom='polygon',colour='black') +
  #geom_hex(bins = 20) +
  #scale_fill_viridis(option="magma")+
  #scale_fill_gradient(low = "grey94", high = "darkred") +
  #scale_x_continuous(breaks = seq(-4, 4, 1)) +
 # labs(title = "Trend Magnitude vs Elevation",
  #     x = "Trend Magnitude",
   #    y = "Elevation (m)")

ggplot(mk_results, aes(slope,elevation)) + facet_wrap(~metric) +
  stat_density2d(aes(fill=..level..),geom='polygon',colour='black') +
  scale_fill_continuous(low="grey94",high="darkred")

theme_set(theme_light(base_size =11))
ggplot(mk_results, aes(slope, elevation)) +
  geom_hex(stat="identity") +
  scale_y_continuous(breaks = seq(-4, 4,1)) +
  facet_grid( ~ metric) +
  labs(title="Sagehen Creek SnowEx 2020 Snow Pit SWE Time Series", y = "SWE (mm)", x = "Date") 

