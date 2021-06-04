# stitching solo year files into one large data set with new data
# 7/28/2020

library(lubridate)
library(data.table)

setwd("/Volumes/jt/margulis/volume_results/")
files_c3 <- list.files(pattern = ".csv")
filelist_c3 <-mixedsort(sort(files_c3)) #sort in correct order
print(filelist_c3)

#read in lists of matrixes   
system.time(years_list <- lapply(filelist_c3, function(x) fread(x, header=T))) 


format <-function(x) {  yut<-as.data.frame(x) #convert element of matrix list to dataframe 
                        year_name <-colnames(yut)[2] #pull out name of 2nd column, which is year name
                        list1 <- length(yut) #create vector that is the length of the list
                        wy <- rep(year_name,length(list1)) #apply year name and repeat it over list length
                        yut<-cbind(wy, yut) #bind wy column to data frame
                        colnames(yut)[3] <- "swe_vol_km3" #change 3rd column too vol_swe from orginal year name
                        yut  }

#apply formatting function to list of matrixes
formatted_list<-lapply(years_list, function(x) format(x))

#bind all matrixes in list together
time_series_2 <- do.call("rbind", formatted_list)

#remove WY_ text in column
time_series<-as.data.frame(mutate(time_series_2, wy = str_replace_all(wy, pattern = "WY", replacement = ""))) #remove wy_
time_series$wy <- as.numeric(as.character(time_series$wy))
date<-seq(as.Date("1984-10-01"), as.Date("2016-09-30"), by="days") #create date sequence
time_series<-cbind(date, time_series) #bind date column to data

#save as csv
fwrite(time_series, "/Volumes/jt/margulis/volume_results/SNSR_daily_vol_SWE.csv")
time_series <-fread("/Volumes/jt/margulis/volume_results/SNSR_daily_vol_SWE.csv")

#filter for yearly max
max_year_swe <-time_series %>% group_by(wy) %>%
  filter(swe_vol_km3 == max(swe_vol_km3))

#max time series
ggplot(max_year_swe, aes(wy, swe_vol_km3)) +
  geom_point(color = "firebrick") +
  geom_smooth(method = "lm") +
  scale_x_continuous(breaks = seq(1985,2016,3)) +
  scale_y_continuous(breaks = seq(0,40,2)) +
  labs(title="CA SN SWE vol", y="SWE (km3)", x="Year") +
  theme_light(11)

#save as csv
fwrite(max_year_swe, "/Volumes/jt/margulis/volume_results/SNSR_daily_max_SWE.csv")


sens.slope(max_year_swe$swe_vol_km3, conf.level = 0.95)
Kendall(max_year_swe$swe_vol_km3, max_year_swe$wy)


#max doy time series
ggplot(max_year_swe, aes(wy, doWY)) +
  geom_point(color = "firebrick") +
  geom_smooth(method = "lm") +
  scale_x_continuous(breaks = seq(1985,2016,3)) +
  scale_y_continuous(breaks = seq(100,200,20)) +
  labs(title="CA SN SWE dowy", y="SWE (km3)", x="Year") +
  theme_light(11)

#max doy time series
wy16<-filter(time_series, wy ==2016)
ggplot(wy16, aes(date, swe_vol_km3)) +
  geom_line(color = "coral") +
  scale_x_continuous(breaks = seq(1985,2016,3)) +
  scale_y_continuous(breaks = seq(100,200,20)) +
  labs(title="CA SN SWE", y="SWE (km3)", x="Year") +
  theme_light(11)


