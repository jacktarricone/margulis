library(snotelr)
library(tidyverse)

  
meta <-snotel_info()  
NV <-filter(meta, state == "NV")
elko <-filter(NV, county == "Elko")
CA <-filter(meta, state == "CA")
  
lam <-as.data.frame(snotel_download(site_id = 570, path = tempdir(), internal = TRUE))


lam_wy2018 <-filter(lam, date > "2017-09-30" & date < "2028-10-01")
lam_wy2019 <-filter(lam, date > "2018-09-30" & date < "2029-10-01")
lam_wy2020 <-filter(lam, date > "2019-09-30" & date < "2020-10-01")
lam_wy2021 <-filter(lam, date > "2020-09-30" & date < "2021-10-01")

ggplot()+
  geom_point(lam_wy2020, mapping = aes(x = date, y = snow_water_equivalent,))+
  geom_point(lam_wy2021, mapping = aes(x = date, y = snow_water_equivalent,))+
  geom_point(lam_wy2019, mapping = aes(x = date, y = snow_water_equivalent,))+ 
  geom_point(lam_wy2018, mapping = aes(x = date, y = snow_water_equivalent,))

# downloading snotel data, SWE[mm] and temp[Degrees C]
indy_camp <- snotel_download(site_id = 539, internal = TRUE) 
indy_creek <- snotel_download(site_id = 540, internal = TRUE) 
indy_lake <- snotel_download(site_id = 541, internal = TRUE) 
css_lab <- snotel_download(site_id = 428, internal = TRUE)

### seperating out the data for a specific time
ilake_wy2020 <- filter(indy_lake, date > "2019-09-30", date < "2020-10-30") 
icamp_wy2020 <- filter(indy_camp, date > "2019-09-30", date < "2020-10-30") 
icreek_wy2020 <- filter(indy_creek, date > "2019-09-30", date < "2020-10-30") 
css_wy2020 <- filter(css_lab, date > "2019-09-30", date < "2020-10-30")

theme_set(theme_light(base_size =11))

wy2020<- ggplot() + 
  geom_line(ilake_wy2020, color="blue", mapping =  aes(x = as.Date(date), y = snow_water_equivalent)) +
  geom_line(icamp_wy2020, color="red", mapping = aes(x = as.Date(date), y = snow_water_equivalent)) +
  geom_line(icreek_wy2020, color="purple", mapping = aes(x = as.Date(date), y = snow_water_equivalent)) +
  geom_line(css_wy2020, color="green", mapping = aes(x = as.Date(date),y = snow_water_equivalent)) +
  labs(title="Independence SNOTELs 2019-20 SWE", y="SWE (mm)", x="Date") 
  
print(wy2020)

# so far this year
### seperating out the data for a specific time
ilake_wy2021 <- filter(indy_lake, date > "2020-09-30", date < "2021-10-30") 
icamp_wy2021 <- filter(indy_camp, date > "2020-09-30", date < "2021-10-30") 
icreek_wy2021 <- filter(indy_creek, date > "2020-09-30", date < "2021-10-30") 
css_wy2021 <- filter(css_lab, date > "2020-09-30", date < "2021-10-30")
  
theme_set(theme_light(base_size =11))
wy2021<- ggplot() + 
  geom_line(ilake_wy2021, color="goldenrod", mapping =  aes(x = as.Date(date), y = snow_water_equivalent)) +
  geom_line(icamp_wy2021, color="firebrick", mapping = aes(x = as.Date(date), y = snow_water_equivalent)) +
  geom_line(icreek_wy2021, color="purple", mapping = aes(x = as.Date(date), y = snow_water_equivalent)) +
  geom_line(css_wy2021, color="black", mapping = aes(x = as.Date(date),y = snow_water_equivalent)) +
  labs(title="north lake SNOTELs wy2021 SWE", y="SWE (mm)", x="Date") 

print(wy2021)
