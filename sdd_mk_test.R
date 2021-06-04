# sdd mk test 
# nov 19 2020

library(BiocManager)
library(rhdf5)
library(tidyverse)
library(rgeos)
library(raster)
library(rgdal)
library(parallel)
library(data.table)
library(gtools)
library(spatialEco)
library(EnvStats)
library(Kendall)
library(remotes)
library(modifiedmk)



# read in scf rasters to stack
setwd("/Volumes/jt/projects/margulis/snow_metric_rasters/sdd/rasters")
list <-list.files()
sdd_list <-lapply(list, function(x) raster(x))
sdd_stack <-stack(sdd_list) 
crs(sdd_stack)<-"+proj=leac +ellps=clrk66"
plot(sdd_stack[[9]])
sdd_stack


# test mk code by first running it on .5 degrees lat near tahoe
#tahoe_crop <-crop(sdd_stack, extent(-120.3, -120.0, 39, 39.3))
#tahoe_crop
#plot(tahoe_crop[[9]])

###### mk test, trend.slope is full stats, 2 is just p-val and slope stats

# modified mk_test taken from git
#mmkh_git <-function(x, ci=0.95) {
  # Initialize the test parameters
  
  # Time series vector
  x = x
  # Modified Z statistic after variance correction by Hamed and Rao (1998) method
  z = NULL
  # Original Z statistic for Mann-Kendall test before variance correction
  z0 = NULL
  # Modified Z statistic after variance correction by Hamed and Rao (1998) method
  pval = NULL
  # Original p-value for Mann-Kendall test before variance correction
  pval0 = NULL
  # Initialize Mann-Kendall S statistic
  S = 0
  # Initialize Mann-Kendall Tau
  Tau = NULL
  # Correction factor n/n* value
  essf = NULL
  # Confidance interval
  ci = ci
  
  # To test whether the data is in vector format
  
  if (is.vector(x) == FALSE) {
    stop("Input data must be a vector")
  }
  
  # To test whether the data values are finite numbers and attempting to eliminate non-finite numbers
  
  if (any(is.finite(x) == FALSE)) {
    x[-c(which(is.finite(x) == FALSE))] -> x
    warning("The input vector contains non-finite numbers. An attempt was made to remove them")
  }
  
  n <- length(x)
  
  #Specify minimum input vector length
  if (n < 3) {
    stop("Input vector must contain at least three values")
  }
  
  # Calculating Sen's slope
  rep(NA, n * (n - 1)/2) -> V
  k = 0
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      k = k+1
      V[k] = (x[j]-x[i])/(j-i)
    }
  }
  median(V,na.rm=TRUE)->slp
  
  # Calculating trend-free Series
  
  t=1:length(x)
  xn<-(x[1:n])-((slp)*(t))
  
  
  # Calculating Mann-Kendall S statistic
  
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      S = S + sign(x[j]-x[i])
    }
  }
  
  # Calculating autocorrelation function of the ranks of observations (ro)
  
  acf(rank(xn), lag.max=(n-1), plot=FALSE)$acf[-1] -> ro
  
  # Calculating significant autocorrelation at given confidance interval (rof)
  
  qnorm((1+ci)/2)/sqrt(n) -> sig
  rep(NA,length(ro)) -> rof
  for (i in 1:(length(ro))) {
    if(ro[i] > sig || ro[i] < -sig) {
      rof[i] <- ro[i]
    } else {
      rof[i] = 0
    }
  }
  
  # Calculating 2/(n*(n-1)*(n-2))
  
  2 / (n*(n-1)*(n-2)) -> cte
  
  # Calculating sum(((n-i)*(n-i-1)*(n-i-2)*rof[i]
  
  ess=0
  for (i in 1:(n-1)) {
    ess = ess + (n-i)*(n-i-1)*(n-i-2)*rof[i]
  }
  
  # Calculating variance correction factor (n/n*) as per Hamed and Rao (1998)
  
  essf = 1 + ess*cte
  
  # Calculating Mann-Kendall Variance before correction (Var(s))
  
  var.S = n*(n-1)*(2*n+5)*(1/18)
  if(length(unique(x)) < n) {
    unique(x) -> aux
    for (i in 1:length(aux)) {
      length(which(x == aux[i])) -> tie
      if (tie > 1) {
        var.S = var.S - tie*(tie-1)*(2*tie+5)*(1/18)
      }
    }
  }
  
  # Calculating new variance Var(s)*=(Var(s))*(n/n*) as per Hamed and Rao (1998)
  
  VS = var.S * essf
  
  # Calculating Z statistic values before and after variance correction
  
  if (S == 0) {
    z = 0
    z0 = 0
  }
  if (S > 0) {
    z = (S-1)/sqrt(VS)
    z0 = (S-1)/sqrt(var.S)
  } else {
    z = (S+1)/sqrt(VS)
    z0 = (S+1)/sqrt(var.S)
  }
  
  # Calculating p-value before and after variance correction
  
  pval = 2*pnorm(-abs(z))
  pval0 = 2*pnorm(-abs(z0))
  
  # Calculating Kendall's Tau
  
  Tau = S/(.5*n*(n-1))
  
  
  return(c("Corrected Zc" = z,
           "new P-value" = pval,
           "N/N*" = essf,
           "Original Z" = z0,
           "old P.value" = pval0,
           "Tau" = Tau,
           "Sen's slope" = slp,
           "old.variance"=var.S,
           "new.variance"= VS))
}

# mmkh sens slope raster generator 
#mmkh_raster_sens <- function(x){
  if(all(is.na(x))){return(NA)}
  if(max(x) == 0){return(NA)}
  fit <- mmkh_git(x, ci = .95)
  sens <- as.numeric(fit[7])
  return(sens)
  }



# mmkh p_val raster generator
#mmkh_raster_p_val <- function(x){
  if(all(is.na(x))){return(NA)}
  if(max(x) == 0){return(NA)}
  fit <- Vectorize(mmkh_git(x, ci = .95))
  p_val <- as.numeric(fit[2])
  return(p_val)
  }



#t <- overlay(tahoe_crop, fun=mmkh_raster_sens )
#t
#plot(t)

#t2 <- overlay(tahoe_crop, fun=mmkh_raster_p_val )
#t2
#plot(t2)

# run it in parallel
# have to split it into two functions bc it's just eazier that way...

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

system.time(sdd_trends_full <- clusterR(sdd_stack, overlay, args=list(fun=trend.slope2)))

endCluster()


plot(sdd_trends_full[[1]])
plot(sdd_trends_full[[2]])

sdd_p_value_full <-sdd_trends_full[[2]]
sdd_slope_full <-sdd_trends_full[[1]]
writeRaster(sdd_p_value_full,"/Volumes/jt/projects/margulis/snow_metric_rasters/sdd/mk_results/sdd_p_value_full.tif")
writeRaster(sdd_slope_full, "/Volumes/jt/projects/margulis/snow_metric_rasters/sdd/mk_results/sdd_slope_full.tif")





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

mmkh_git <-function(x, ci=0.95) {
  # Initialize the test parameters
  
  # Time series vector
  x = x
  # Modified Z statistic after variance correction by Hamed and Rao (1998) method
  z = NULL
  # Original Z statistic for Mann-Kendall test before variance correction
  z0 = NULL
  # Modified Z statistic after variance correction by Hamed and Rao (1998) method
  pval = NULL
  # Original p-value for Mann-Kendall test before variance correction
  pval0 = NULL
  # Initialize Mann-Kendall S statistic
  S = 0
  # Initialize Mann-Kendall Tau
  Tau = NULL
  # Correction factor n/n* value
  essf = NULL
  # Confidance interval
  ci = ci
  
  # To test whether the data is in vector format
  
  if (is.vector(x) == FALSE) {
    stop("Input data must be a vector")
  }
  
  # To test whether the data values are finite numbers and attempting to eliminate non-finite numbers
  
  if (any(is.finite(x) == FALSE)) {
    x[-c(which(is.finite(x) == FALSE))] -> x
    warning("The input vector contains non-finite numbers. An attempt was made to remove them")
  }
  
  n <- length(x)
  
  #Specify minimum input vector length
  if (n < 3) {
    stop("Input vector must contain at least three values")
  }
  
  # Calculating Sen's slope
  rep(NA, n * (n - 1)/2) -> V
  k = 0
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      k = k+1
      V[k] = (x[j]-x[i])/(j-i)
    }
  }
  median(V,na.rm=TRUE)->slp
  
  # Calculating trend-free Series
  
  t=1:length(x)
  xn<-(x[1:n])-((slp)*(t))
  
  
  # Calculating Mann-Kendall S statistic
  
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      S = S + sign(x[j]-x[i])
    }
  }
  
  # Calculating autocorrelation function of the ranks of observations (ro)
  
  acf(rank(xn), lag.max=(n-1), plot=FALSE)$acf[-1] -> ro
  
  # Calculating significant autocorrelation at given confidance interval (rof)
  
  qnorm((1+ci)/2)/sqrt(n) -> sig
  rep(NA,length(ro)) -> rof
  for (i in 1:(length(ro))) {
    if(ro[i] > sig || ro[i] < -sig) {
      rof[i] <- ro[i]
    } else {
      rof[i] = 0
    }
  }
  
  # Calculating 2/(n*(n-1)*(n-2))
  
  2 / (n*(n-1)*(n-2)) -> cte
  
  # Calculating sum(((n-i)*(n-i-1)*(n-i-2)*rof[i]
  
  ess=0
  for (i in 1:(n-1)) {
    ess = ess + (n-i)*(n-i-1)*(n-i-2)*rof[i]
  }
  
  # Calculating variance correction factor (n/n*) as per Hamed and Rao (1998)
  
  essf = 1 + ess*cte
  
  # Calculating Mann-Kendall Variance before correction (Var(s))
  
  var.S = n*(n-1)*(2*n+5)*(1/18)
  if(length(unique(x)) < n) {
    unique(x) -> aux
    for (i in 1:length(aux)) {
      length(which(x == aux[i])) -> tie
      if (tie > 1) {
        var.S = var.S - tie*(tie-1)*(2*tie+5)*(1/18)
      }
    }
  }
  
  # Calculating new variance Var(s)*=(Var(s))*(n/n*) as per Hamed and Rao (1998)
  
  VS = var.S * essf
  
  # Calculating Z statistic values before and after variance correction
  
  if (S == 0) {
    z = 0
    z0 = 0
  }
  if (S > 0) {
    z = (S-1)/sqrt(VS)
    z0 = (S-1)/sqrt(var.S)
  } else {
    z = (S+1)/sqrt(VS)
    z0 = (S+1)/sqrt(var.S)
  }
  
  # Calculating p-value before and after variance correction
  
  pval = 2*pnorm(-abs(z))
  pval0 = 2*pnorm(-abs(z0))
  
  # Calculating Kendall's Tau
  
  Tau = S/(.5*n*(n-1))
  
  
  return(c("Corrected Zc" = z,
           "new P-value" = pval,
           "N/N*" = essf,
           "Original Z" = z0,
           "old P.value" = pval0,
           "Tau" = Tau,
           "Sen's slope" = slp,
           "old.variance"=var.S,
           "new.variance"= VS))
}

