library(raster)
DEM <- raster("/Volumes/jt/projects/margulis/gis/rasters/SNSR_nlcd_firstdraft.tif")
dem_low <- aggregate(DEM, fact = 3, fun = mean)
writeRaster(dem_low,'/Volumes/jt/projects/margulis/gis/rasters/SNSR_nlcd_lowres.tif',options=c('TFW=YES'))
rm(DEM,dem_low)

plot(DEM)

DEM[ DEM[] == 0 ] <- NA

hist(DEM,
     main = "Distribution of surface elevation values",
     xlab = "Elevation (meters)", ylab = "Frequency",
     col = "springgreen")
