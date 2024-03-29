# scf mk test 
# nov 17 2020

library(BiocManager)
library(rhdf5)
library(tidyverse)
library(rgeos)
library(raster)
library(rgdal)
library(parallel)
library(data.table)
library(gtools)
library(terra)
library(spatialEco)
library(EnvStats)
library(Kendall)
library(remotes)




# read in scf rasters to stack
setwd("/Volumes/jt/projects/margulis/snow_metric_rasters/scf/rasters")
list <-list.files()
scf_list <-lapply(list, function(x) raster(x))
scf_stack <-stack(scf_list) 
crs(scf_stack)<-"+proj=leac +ellps=clrk66"
plot(scf_stack[[9]])
scf_stack


# test mk code by first running it on .5 degrees lat near tahoe
tahoe_crop <-crop(scf_stack, extent(-123.3, -117.6, 39, 39.5))
tahoe_crop
plot(tahoe_crop[[9]])

###### mk test, trend.slope is full stats, 2 is just p-val and slope stats
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

trend.slope2 <- function(y, p.value.pass = TRUE, z.pass = FALSE, 
                         tau.pass = FALSE, confidence.pass = FALSE, intercept.pass = FALSE) {
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


# run it in parallel to see if stripping is gone 
beginCluster(n=7)

system.time(scf_trends_full <- clusterR(scf_stack, overlay, args=list(fun=trend.slope2)))

endCluster()


plot(scf_trends_full[[1]])
plot(scf_trends_full[[2]])

scf_p_value_full <-scf_trends_full[[2]]
scf_slope_full <-scf_trends_full[[1]]
writeRaster(scf_p_value_full,"/Volumes/jt/projects/margulis/snow_metric_rasters/scf/mk_results/scf_p_value_full.tif")
writeRaster(scf_slope_full, "/Volumes/jt/projects/margulis/snow_metric_rasters/scf/mk_results/scf_slope_full.tif")


###########################################
############# calculations testing
###########################################

scf_p_value <-raster("/Volumes/jt/projects/margulis/snow_metric_rasters/scf/mk_results/scf_p_value.tif")
scf_slope <-raster("/Volumes/jt/projects/margulis/snow_metric_rasters/scf/mk_results/scf_slope.tif")


sig_pval <-raster("/Volumes/jt/projects/margulis/snow_metric_rasters/scf/mk_results/sig_pval.tif")
sig_ele <-raster("/Volumes/jt/projects/margulis/snow_metric_rasters/scf/mk_results/sig_ele.tif")
sig_slope <-raster("/Volumes/jt/projects/margulis/snow_metric_rasters/scf/mk_results/sig_slope.tif")
crs(sig_pval)<-"+proj=leac +ellps=clrk66"
crs(sig_ele)<-"+proj=leac +ellps=clrk66"
crs(sig_slope)<-"+proj=leac +ellps=clrk66"

mk_stack <-stack(sig_pval, sig_ele, sig_slope)

plot(mk_stack)


values(sig_ele)[values(sig_ele) <= 0] = NA
hist(sig_ele)


values(sig_slope)[values(sig_slope) == 0.0000] = NA
hist(sig_slope)


plot(values(sig_slope), values(sig_ele))

tahoe_array <-as.array(tahoe_crop)
test_col <-as.data.frame(tahoe_array[250, 2900, 1:32])
wy <-c(1985:2016)
test_col <-cbind(test_col, wy)
colnames(test_col)[1] <- "scf"
hist(test_col)

ggplot(test_col)+
  geom_point(aes(wy, scf)) + 
  theme_light()


# read in static rasters to stack for calculations
static_stack <-stack("/Volumes/jt/projects/margulis/static/rasters/static_stack.tif")
crs(static_stack)<-"+proj=leac +ellps=clrk66"
plot(static_stack)
static_stack

# test mk code by first running it on .5 degrees lat near tahoe
static_crop <-crop(static_stack, extent(-123.3, -117.6, 39, 39.5))
static_crop
plot(static_crop)

hist(sig_pix)
freq(sig_pix)
















?raster.kendall

top1deg_results2
plot(top1deg_results2[[1]])
########

plot(max_results)
max_results_stack <-stack(max_results)
crs(max_results_stack)<-"+proj=leac +ellps=clrk66"

p_value <-max_results[[2]]
slope <-max_results[[1]]
writeRaster(p_value,"/Volumes/jt/projects/margulis/mk_results/p_value.tif")
writeRaster(slope, "/Volumes/jt/projects/margulis/mk_results/slope.tif")
