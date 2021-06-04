setwd("/Volumes/JTARRICONE/UNR_summer_20/margulis_data/swe/daily_rdata_files/WY_2016")

list<-"/Volumes/JTARRICONE/UNR_summer_20/margulis_data/swe/daily_rdata_files/WY_2016"
daily_list <- list.files(path=list, pattern=".Rdata", full.names = TRUE)
print(daily_list)

matrix_list<-sapply(daily_list,load,environment())

matrix_list<-list(swe_2016_doWY_1,
                        swe_2016_doWY_2,
                        swe_2016_doWY_3,
                        swe_2016_doWY_4,
                        swe_2016_doWY_5,
                        swe_2016_doWY_6,
                        swe_2016_doWY_7,
                        swe_2016_doWY_8,
                        swe_2016_doWY_9,
                        swe_2016_doWY_10,
                        swe_2016_doWY_11,
                        swe_2016_doWY_12,
                        swe_2016_doWY_13,
                        swe_2016_doWY_14,
                        swe_2016_doWY_15,
                        swe_2016_doWY_16,
                        swe_2016_doWY_17,
                        swe_2016_doWY_18,
                        swe_2016_doWY_19,
                        swe_2016_doWY_20,
                        swe_2016_doWY_22,
                        swe_2016_doWY_23,
                        swe_2016_doWY_24,
                        swe_2016_doWY_25,
                        swe_2016_doWY_26,
                        swe_2016_doWY_27,
                        swe_2016_doWY_28,
                        swe_2016_doWY_29,
                        swe_2016_doWY_30,
                        swe_2016_doWY_31,
                        swe_2016_doWY_32,
                        swe_2016_doWY_33,
                        swe_2016_doWY_34,
                        swe_2016_doWY_35,
                        swe_2016_doWY_36,
                        swe_2016_doWY_37,
                        swe_2016_doWY_38,
                        swe_2016_doWY_39,
                        swe_2016_doWY_40)


add_na_list<-lapply(matrix_list, function(x) na_if(x, -32768)) #add NAs to matrix list
raster_list<-lapply(add_na_list, function(x) raster(x, xmn=-123.3, xmx=-117.6, ymn=35.4, ymx=42)) #create raster list from matrix list
raster_stack<-stack(raster_list) #stack rasterlist

system.time(swe_stats<-as.data.frame(lapply(raster_list, function(x) volumetric_swe(x)))) 

#format from list to DF
swe_stats_new <-rownames_to_column(swe_stats) %>% 
  gather(var, value, -rowname) %>% 
  spread(rowname, value) 

as.data.frame(swe_stats_new)

#delete unwanted column
swe_stats_new <-dplyr::select(swe_stats_new,-c(var))









#1-40 create
swe_2016_doWY_1 <-as.matrix(swe_2016[1:6601, 1:5701, 1]) #pull out day 1 from array	
swe_2016_doWY_2 <-as.matrix(swe_2016[1:6601, 1:5701, 2]) #pull out day 2 from array	
swe_2016_doWY_3 <-as.matrix(swe_2016[1:6601, 1:5701, 3]) #pull out day 3 from array	
swe_2016_doWY_4 <-as.matrix(swe_2016[1:6601, 1:5701, 4]) #pull out day 4 from array	
swe_2016_doWY_5 <-as.matrix(swe_2016[1:6601, 1:5701, 5]) #pull out day 5 from array	
swe_2016_doWY_6 <-as.matrix(swe_2016[1:6601, 1:5701, 6]) #pull out day 6 from array	
swe_2016_doWY_7 <-as.matrix(swe_2016[1:6601, 1:5701, 7]) #pull out day 7 from array	
swe_2016_doWY_8 <-as.matrix(swe_2016[1:6601, 1:5701, 8]) #pull out day 8 from array	
swe_2016_doWY_9 <-as.matrix(swe_2016[1:6601, 1:5701, 9]) #pull out day 9 from array	
swe_2016_doWY_10 <-as.matrix(swe_2016[1:6601, 1:5701, 10]) #pull out day 10 from array	
swe_2016_doWY_11 <-as.matrix(swe_2016[1:6601, 1:5701, 11]) #pull out day 11 from array	
swe_2016_doWY_12 <-as.matrix(swe_2016[1:6601, 1:5701, 12]) #pull out day 12 from array	
swe_2016_doWY_13 <-as.matrix(swe_2016[1:6601, 1:5701, 13]) #pull out day 13 from array	
swe_2016_doWY_14 <-as.matrix(swe_2016[1:6601, 1:5701, 14]) #pull out day 14 from array	
swe_2016_doWY_15 <-as.matrix(swe_2016[1:6601, 1:5701, 15]) #pull out day 15 from array	
swe_2016_doWY_16 <-as.matrix(swe_2016[1:6601, 1:5701, 16]) #pull out day 16 from array	
swe_2016_doWY_17 <-as.matrix(swe_2016[1:6601, 1:5701, 17]) #pull out day 17 from array	
swe_2016_doWY_18 <-as.matrix(swe_2016[1:6601, 1:5701, 18]) #pull out day 18 from array	
swe_2016_doWY_19 <-as.matrix(swe_2016[1:6601, 1:5701, 19]) #pull out day 19 from array	
swe_2016_doWY_20 <-as.matrix(swe_2016[1:6601, 1:5701, 20]) #pull out day 20 from array	
swe_2016_doWY_21 <-as.matrix(swe_2016[1:6601, 1:5701, 21]) #pull out day 21 from array	
swe_2016_doWY_22 <-as.matrix(swe_2016[1:6601, 1:5701, 22]) #pull out day 22 from array	
swe_2016_doWY_23 <-as.matrix(swe_2016[1:6601, 1:5701, 23]) #pull out day 23 from array	
swe_2016_doWY_24 <-as.matrix(swe_2016[1:6601, 1:5701, 24]) #pull out day 24 from array	
swe_2016_doWY_25 <-as.matrix(swe_2016[1:6601, 1:5701, 25]) #pull out day 25 from array	
swe_2016_doWY_26 <-as.matrix(swe_2016[1:6601, 1:5701, 26]) #pull out day 26 from array	
swe_2016_doWY_27 <-as.matrix(swe_2016[1:6601, 1:5701, 27]) #pull out day 27 from array	
swe_2016_doWY_28 <-as.matrix(swe_2016[1:6601, 1:5701, 28]) #pull out day 28 from array	
swe_2016_doWY_29 <-as.matrix(swe_2016[1:6601, 1:5701, 29]) #pull out day 29 from array	
swe_2016_doWY_30 <-as.matrix(swe_2016[1:6601, 1:5701, 30]) #pull out day 30 from array	
swe_2016_doWY_31 <-as.matrix(swe_2016[1:6601, 1:5701, 31]) #pull out day 31 from array	
swe_2016_doWY_32 <-as.matrix(swe_2016[1:6601, 1:5701, 32]) #pull out day 32 from array	
swe_2016_doWY_33 <-as.matrix(swe_2016[1:6601, 1:5701, 33]) #pull out day 33 from array	
swe_2016_doWY_34 <-as.matrix(swe_2016[1:6601, 1:5701, 34]) #pull out day 34 from array	
swe_2016_doWY_35 <-as.matrix(swe_2016[1:6601, 1:5701, 35]) #pull out day 35 from array	
swe_2016_doWY_36 <-as.matrix(swe_2016[1:6601, 1:5701, 36]) #pull out day 36 from array	
swe_2016_doWY_37 <-as.matrix(swe_2016[1:6601, 1:5701, 37]) #pull out day 37 from array	
swe_2016_doWY_38 <-as.matrix(swe_2016[1:6601, 1:5701, 38]) #pull out day 38 from array	
swe_2016_doWY_39 <-as.matrix(swe_2016[1:6601, 1:5701, 39]) #pull out day 39 from array	
swe_2016_doWY_40 <-as.matrix(swe_2016[1:6601, 1:5701, 40]) #pull out day 40 from array	

#save 1-40
save(swe_2016_doWY_1, file = "swe_2016_doWY_1.Rdata")
save(swe_2016_doWY_2, file = "swe_2016_doWY_2.Rdata")
save(swe_2016_doWY_3, file = "swe_2016_doWY_3.Rdata")
save(swe_2016_doWY_4, file = "swe_2016_doWY_4.Rdata")
save(swe_2016_doWY_5, file = "swe_2016_doWY_5.Rdata")
save(swe_2016_doWY_6, file = "swe_2016_doWY_6.Rdata")
save(swe_2016_doWY_7, file = "swe_2016_doWY_7.Rdata")
save(swe_2016_doWY_8, file = "swe_2016_doWY_8.Rdata")
save(swe_2016_doWY_9, file = "swe_2016_doWY_9.Rdata")
save(swe_2016_doWY_10, file = "swe_2016_doWY_10.Rdata")
save(swe_2016_doWY_11, file = "swe_2016_doWY_11.Rdata")
save(swe_2016_doWY_12, file = "swe_2016_doWY_12.Rdata")
save(swe_2016_doWY_13, file = "swe_2016_doWY_13.Rdata")
save(swe_2016_doWY_14, file = "swe_2016_doWY_14.Rdata")
save(swe_2016_doWY_15, file = "swe_2016_doWY_15.Rdata")
save(swe_2016_doWY_16, file = "swe_2016_doWY_16.Rdata")
save(swe_2016_doWY_17, file = "swe_2016_doWY_17.Rdata")
save(swe_2016_doWY_18, file = "swe_2016_doWY_18.Rdata")
save(swe_2016_doWY_19, file = "swe_2016_doWY_19.Rdata")
save(swe_2016_doWY_20, file = "swe_2016_doWY_20.Rdata")
save(swe_2016_doWY_21, file = "swe_2016_doWY_21.Rdata")
save(swe_2016_doWY_22, file = "swe_2016_doWY_22.Rdata")
save(swe_2016_doWY_23, file = "swe_2016_doWY_23.Rdata")
save(swe_2016_doWY_24, file = "swe_2016_doWY_24.Rdata")
save(swe_2016_doWY_25, file = "swe_2016_doWY_25.Rdata")
save(swe_2016_doWY_26, file = "swe_2016_doWY_26.Rdata")
save(swe_2016_doWY_27, file = "swe_2016_doWY_27.Rdata")
save(swe_2016_doWY_28, file = "swe_2016_doWY_28.Rdata")
save(swe_2016_doWY_29, file = "swe_2016_doWY_29.Rdata")
save(swe_2016_doWY_30, file = "swe_2016_doWY_30.Rdata")
save(swe_2016_doWY_31, file = "swe_2016_doWY_31.Rdata")
save(swe_2016_doWY_32, file = "swe_2016_doWY_32.Rdata")
save(swe_2016_doWY_33, file = "swe_2016_doWY_33.Rdata")
save(swe_2016_doWY_34, file = "swe_2016_doWY_34.Rdata")
save(swe_2016_doWY_35, file = "swe_2016_doWY_35.Rdata")
save(swe_2016_doWY_36, file = "swe_2016_doWY_36.Rdata")
save(swe_2016_doWY_37, file = "swe_2016_doWY_37.Rdata")
save(swe_2016_doWY_38, file = "swe_2016_doWY_38.Rdata")
save(swe_2016_doWY_39, file = "swe_2016_doWY_39.Rdata")
save(swe_2016_doWY_40, file = "swe_2016_doWY_40.Rdata")

#create 41-80
swe_2016_doWY_41 <-as.matrix(swe_2016[1:6601, 1:5701, 41]) #pull out day 41 from array	
swe_2016_doWY_42 <-as.matrix(swe_2016[1:6601, 1:5701, 42]) #pull out day 42 from array	
swe_2016_doWY_43 <-as.matrix(swe_2016[1:6601, 1:5701, 43]) #pull out day 43 from array	
swe_2016_doWY_44 <-as.matrix(swe_2016[1:6601, 1:5701, 44]) #pull out day 44 from array	
swe_2016_doWY_45 <-as.matrix(swe_2016[1:6601, 1:5701, 45]) #pull out day 45 from array	
swe_2016_doWY_46 <-as.matrix(swe_2016[1:6601, 1:5701, 46]) #pull out day 46 from array	
swe_2016_doWY_47 <-as.matrix(swe_2016[1:6601, 1:5701, 47]) #pull out day 47 from array	
swe_2016_doWY_48 <-as.matrix(swe_2016[1:6601, 1:5701, 48]) #pull out day 48 from array	
swe_2016_doWY_49 <-as.matrix(swe_2016[1:6601, 1:5701, 49]) #pull out day 49 from array	
swe_2016_doWY_50 <-as.matrix(swe_2016[1:6601, 1:5701, 50]) #pull out day 50 from array	
swe_2016_doWY_51 <-as.matrix(swe_2016[1:6601, 1:5701, 51]) #pull out day 51 from array	
swe_2016_doWY_52 <-as.matrix(swe_2016[1:6601, 1:5701, 52]) #pull out day 52 from array	
swe_2016_doWY_53 <-as.matrix(swe_2016[1:6601, 1:5701, 53]) #pull out day 53 from array	
swe_2016_doWY_54 <-as.matrix(swe_2016[1:6601, 1:5701, 54]) #pull out day 54 from array	
swe_2016_doWY_55 <-as.matrix(swe_2016[1:6601, 1:5701, 55]) #pull out day 55 from array	
swe_2016_doWY_56 <-as.matrix(swe_2016[1:6601, 1:5701, 56]) #pull out day 56 from array	
swe_2016_doWY_57 <-as.matrix(swe_2016[1:6601, 1:5701, 57]) #pull out day 57 from array	
swe_2016_doWY_58 <-as.matrix(swe_2016[1:6601, 1:5701, 58]) #pull out day 58 from array	
swe_2016_doWY_59 <-as.matrix(swe_2016[1:6601, 1:5701, 59]) #pull out day 59 from array	
swe_2016_doWY_60 <-as.matrix(swe_2016[1:6601, 1:5701, 60]) #pull out day 60 from array	
swe_2016_doWY_61 <-as.matrix(swe_2016[1:6601, 1:5701, 61]) #pull out day 61 from array	
swe_2016_doWY_62 <-as.matrix(swe_2016[1:6601, 1:5701, 62]) #pull out day 62 from array	
swe_2016_doWY_63 <-as.matrix(swe_2016[1:6601, 1:5701, 63]) #pull out day 63 from array	
swe_2016_doWY_64 <-as.matrix(swe_2016[1:6601, 1:5701, 64]) #pull out day 64 from array	
swe_2016_doWY_65 <-as.matrix(swe_2016[1:6601, 1:5701, 65]) #pull out day 65 from array	
swe_2016_doWY_66 <-as.matrix(swe_2016[1:6601, 1:5701, 66]) #pull out day 66 from array	
swe_2016_doWY_67 <-as.matrix(swe_2016[1:6601, 1:5701, 67]) #pull out day 67 from array	
swe_2016_doWY_68 <-as.matrix(swe_2016[1:6601, 1:5701, 68]) #pull out day 68 from array	
swe_2016_doWY_69 <-as.matrix(swe_2016[1:6601, 1:5701, 69]) #pull out day 69 from array	
swe_2016_doWY_70 <-as.matrix(swe_2016[1:6601, 1:5701, 70]) #pull out day 70 from array	
swe_2016_doWY_71 <-as.matrix(swe_2016[1:6601, 1:5701, 71]) #pull out day 71 from array	
swe_2016_doWY_72 <-as.matrix(swe_2016[1:6601, 1:5701, 72]) #pull out day 72 from array	
swe_2016_doWY_73 <-as.matrix(swe_2016[1:6601, 1:5701, 73]) #pull out day 73 from array	
swe_2016_doWY_74 <-as.matrix(swe_2016[1:6601, 1:5701, 74]) #pull out day 74 from array	
swe_2016_doWY_75 <-as.matrix(swe_2016[1:6601, 1:5701, 75]) #pull out day 75 from array	
swe_2016_doWY_76 <-as.matrix(swe_2016[1:6601, 1:5701, 76]) #pull out day 76 from array	
swe_2016_doWY_77 <-as.matrix(swe_2016[1:6601, 1:5701, 77]) #pull out day 77 from array	
swe_2016_doWY_78 <-as.matrix(swe_2016[1:6601, 1:5701, 78]) #pull out day 78 from array	
swe_2016_doWY_79 <-as.matrix(swe_2016[1:6601, 1:5701, 79]) #pull out day 79 from array	
swe_2016_doWY_80 <-as.matrix(swe_2016[1:6601, 1:5701, 80]) #pull out day 80 from array	

#save 41-80
save(swe_2016_doWY_41, file = "swe_2016_doWY_41.Rdata")
save(swe_2016_doWY_42, file = "swe_2016_doWY_42.Rdata")
save(swe_2016_doWY_43, file = "swe_2016_doWY_43.Rdata")
save(swe_2016_doWY_44, file = "swe_2016_doWY_44.Rdata")
save(swe_2016_doWY_45, file = "swe_2016_doWY_45.Rdata")
save(swe_2016_doWY_46, file = "swe_2016_doWY_46.Rdata")
save(swe_2016_doWY_47, file = "swe_2016_doWY_47.Rdata")
save(swe_2016_doWY_48, file = "swe_2016_doWY_48.Rdata")
save(swe_2016_doWY_49, file = "swe_2016_doWY_49.Rdata")
save(swe_2016_doWY_50, file = "swe_2016_doWY_50.Rdata")
save(swe_2016_doWY_51, file = "swe_2016_doWY_51.Rdata")
save(swe_2016_doWY_52, file = "swe_2016_doWY_52.Rdata")
save(swe_2016_doWY_53, file = "swe_2016_doWY_53.Rdata")
save(swe_2016_doWY_54, file = "swe_2016_doWY_54.Rdata")
save(swe_2016_doWY_55, file = "swe_2016_doWY_55.Rdata")
save(swe_2016_doWY_56, file = "swe_2016_doWY_56.Rdata")
save(swe_2016_doWY_57, file = "swe_2016_doWY_57.Rdata")
save(swe_2016_doWY_58, file = "swe_2016_doWY_58.Rdata")
save(swe_2016_doWY_59, file = "swe_2016_doWY_59.Rdata")
save(swe_2016_doWY_60, file = "swe_2016_doWY_60.Rdata")
save(swe_2016_doWY_61, file = "swe_2016_doWY_61.Rdata")
save(swe_2016_doWY_62, file = "swe_2016_doWY_62.Rdata")
save(swe_2016_doWY_63, file = "swe_2016_doWY_63.Rdata")
save(swe_2016_doWY_64, file = "swe_2016_doWY_64.Rdata")
save(swe_2016_doWY_65, file = "swe_2016_doWY_65.Rdata")
save(swe_2016_doWY_66, file = "swe_2016_doWY_66.Rdata")
save(swe_2016_doWY_67, file = "swe_2016_doWY_67.Rdata")
save(swe_2016_doWY_68, file = "swe_2016_doWY_68.Rdata")
save(swe_2016_doWY_69, file = "swe_2016_doWY_69.Rdata")
save(swe_2016_doWY_70, file = "swe_2016_doWY_70.Rdata")
save(swe_2016_doWY_71, file = "swe_2016_doWY_71.Rdata")
save(swe_2016_doWY_72, file = "swe_2016_doWY_72.Rdata")
save(swe_2016_doWY_73, file = "swe_2016_doWY_73.Rdata")
save(swe_2016_doWY_74, file = "swe_2016_doWY_74.Rdata")
save(swe_2016_doWY_75, file = "swe_2016_doWY_75.Rdata")
save(swe_2016_doWY_76, file = "swe_2016_doWY_76.Rdata")
save(swe_2016_doWY_77, file = "swe_2016_doWY_77.Rdata")
save(swe_2016_doWY_78, file = "swe_2016_doWY_78.Rdata")
save(swe_2016_doWY_79, file = "swe_2016_doWY_79.Rdata")
save(swe_2016_doWY_80, file = "swe_2016_doWY_80.Rdata")

#create 81-120
swe_2016_doWY_81 <-as.matrix(swe_2016[1:6601, 1:5701, 81]) #pull out day 81 from array	
swe_2016_doWY_82 <-as.matrix(swe_2016[1:6601, 1:5701, 82]) #pull out day 82 from array	
swe_2016_doWY_83 <-as.matrix(swe_2016[1:6601, 1:5701, 83]) #pull out day 83 from array	
swe_2016_doWY_84 <-as.matrix(swe_2016[1:6601, 1:5701, 84]) #pull out day 84 from array	
swe_2016_doWY_85 <-as.matrix(swe_2016[1:6601, 1:5701, 85]) #pull out day 85 from array	
swe_2016_doWY_86 <-as.matrix(swe_2016[1:6601, 1:5701, 86]) #pull out day 86 from array	
swe_2016_doWY_87 <-as.matrix(swe_2016[1:6601, 1:5701, 87]) #pull out day 87 from array	
swe_2016_doWY_88 <-as.matrix(swe_2016[1:6601, 1:5701, 88]) #pull out day 88 from array	
swe_2016_doWY_89 <-as.matrix(swe_2016[1:6601, 1:5701, 89]) #pull out day 89 from array	
swe_2016_doWY_90 <-as.matrix(swe_2016[1:6601, 1:5701, 90]) #pull out day 90 from array	
swe_2016_doWY_91 <-as.matrix(swe_2016[1:6601, 1:5701, 91]) #pull out day 91 from array	
swe_2016_doWY_92 <-as.matrix(swe_2016[1:6601, 1:5701, 92]) #pull out day 92 from array	
swe_2016_doWY_93 <-as.matrix(swe_2016[1:6601, 1:5701, 93]) #pull out day 93 from array	
swe_2016_doWY_94 <-as.matrix(swe_2016[1:6601, 1:5701, 94]) #pull out day 94 from array	
swe_2016_doWY_95 <-as.matrix(swe_2016[1:6601, 1:5701, 95]) #pull out day 95 from array	
swe_2016_doWY_96 <-as.matrix(swe_2016[1:6601, 1:5701, 96]) #pull out day 96 from array	
swe_2016_doWY_97 <-as.matrix(swe_2016[1:6601, 1:5701, 97]) #pull out day 97 from array	
swe_2016_doWY_98 <-as.matrix(swe_2016[1:6601, 1:5701, 98]) #pull out day 98 from array	
swe_2016_doWY_99 <-as.matrix(swe_2016[1:6601, 1:5701, 99]) #pull out day 99 from array	
swe_2016_doWY_100 <-as.matrix(swe_2016[1:6601, 1:5701, 100]) #pull out day 100 from array	
swe_2016_doWY_101 <-as.matrix(swe_2016[1:6601, 1:5701, 101]) #pull out day 101 from array	
swe_2016_doWY_102 <-as.matrix(swe_2016[1:6601, 1:5701, 102]) #pull out day 102 from array	
swe_2016_doWY_103 <-as.matrix(swe_2016[1:6601, 1:5701, 103]) #pull out day 103 from array	
swe_2016_doWY_104 <-as.matrix(swe_2016[1:6601, 1:5701, 104]) #pull out day 104 from array	
swe_2016_doWY_105 <-as.matrix(swe_2016[1:6601, 1:5701, 105]) #pull out day 105 from array	
swe_2016_doWY_106 <-as.matrix(swe_2016[1:6601, 1:5701, 106]) #pull out day 106 from array	
swe_2016_doWY_107 <-as.matrix(swe_2016[1:6601, 1:5701, 107]) #pull out day 107 from array	
swe_2016_doWY_108 <-as.matrix(swe_2016[1:6601, 1:5701, 108]) #pull out day 108 from array	
swe_2016_doWY_109 <-as.matrix(swe_2016[1:6601, 1:5701, 109]) #pull out day 109 from array	
swe_2016_doWY_110 <-as.matrix(swe_2016[1:6601, 1:5701, 110]) #pull out day 110 from array	
swe_2016_doWY_111 <-as.matrix(swe_2016[1:6601, 1:5701, 111]) #pull out day 111 from array	
swe_2016_doWY_112 <-as.matrix(swe_2016[1:6601, 1:5701, 112]) #pull out day 112 from array	
swe_2016_doWY_113 <-as.matrix(swe_2016[1:6601, 1:5701, 113]) #pull out day 113 from array	
swe_2016_doWY_114 <-as.matrix(swe_2016[1:6601, 1:5701, 114]) #pull out day 114 from array	
swe_2016_doWY_115 <-as.matrix(swe_2016[1:6601, 1:5701, 115]) #pull out day 115 from array	
swe_2016_doWY_116 <-as.matrix(swe_2016[1:6601, 1:5701, 116]) #pull out day 116 from array	
swe_2016_doWY_117 <-as.matrix(swe_2016[1:6601, 1:5701, 117]) #pull out day 117 from array	
swe_2016_doWY_118 <-as.matrix(swe_2016[1:6601, 1:5701, 118]) #pull out day 118 from array	
swe_2016_doWY_119 <-as.matrix(swe_2016[1:6601, 1:5701, 119]) #pull out day 119 from array	
swe_2016_doWY_120 <-as.matrix(swe_2016[1:6601, 1:5701, 120]) #pull out day 120 from array	
swe_2016_doWY_121 <-as.matrix(swe_2016[1:6601, 1:5701, 121]) #pull out day 121 from array	
swe_2016_doWY_122 <-as.matrix(swe_2016[1:6601, 1:5701, 122]) #pull out day 122 from array	
swe_2016_doWY_123 <-as.matrix(swe_2016[1:6601, 1:5701, 123]) #pull out day 123 from array	
swe_2016_doWY_124 <-as.matrix(swe_2016[1:6601, 1:5701, 124]) #pull out day 124 from array	
swe_2016_doWY_125 <-as.matrix(swe_2016[1:6601, 1:5701, 125]) #pull out day 125 from array	
swe_2016_doWY_126 <-as.matrix(swe_2016[1:6601, 1:5701, 126]) #pull out day 126 from array	
swe_2016_doWY_127 <-as.matrix(swe_2016[1:6601, 1:5701, 127]) #pull out day 127 from array	
swe_2016_doWY_128 <-as.matrix(swe_2016[1:6601, 1:5701, 128]) #pull out day 128 from array	
swe_2016_doWY_129 <-as.matrix(swe_2016[1:6601, 1:5701, 129]) #pull out day 129 from array	
swe_2016_doWY_130 <-as.matrix(swe_2016[1:6601, 1:5701, 130]) #pull out day 130 from array	
swe_2016_doWY_131 <-as.matrix(swe_2016[1:6601, 1:5701, 131]) #pull out day 131 from array	
swe_2016_doWY_132 <-as.matrix(swe_2016[1:6601, 1:5701, 132]) #pull out day 132 from array	
swe_2016_doWY_133 <-as.matrix(swe_2016[1:6601, 1:5701, 133]) #pull out day 133 from array	
swe_2016_doWY_134 <-as.matrix(swe_2016[1:6601, 1:5701, 134]) #pull out day 134 from array	
swe_2016_doWY_135 <-as.matrix(swe_2016[1:6601, 1:5701, 135]) #pull out day 135 from array	
swe_2016_doWY_136 <-as.matrix(swe_2016[1:6601, 1:5701, 136]) #pull out day 136 from array	
swe_2016_doWY_137 <-as.matrix(swe_2016[1:6601, 1:5701, 137]) #pull out day 137 from array	
swe_2016_doWY_138 <-as.matrix(swe_2016[1:6601, 1:5701, 138]) #pull out day 138 from array	
swe_2016_doWY_139 <-as.matrix(swe_2016[1:6601, 1:5701, 139]) #pull out day 139 from array	
swe_2016_doWY_140 <-as.matrix(swe_2016[1:6601, 1:5701, 140]) #pull out day 140 from array	
swe_2016_doWY_141 <-as.matrix(swe_2016[1:6601, 1:5701, 141]) #pull out day 141 from array	
swe_2016_doWY_142 <-as.matrix(swe_2016[1:6601, 1:5701, 142]) #pull out day 142 from array	
swe_2016_doWY_143 <-as.matrix(swe_2016[1:6601, 1:5701, 143]) #pull out day 143 from array	
swe_2016_doWY_144 <-as.matrix(swe_2016[1:6601, 1:5701, 144]) #pull out day 144 from array	
swe_2016_doWY_145 <-as.matrix(swe_2016[1:6601, 1:5701, 145]) #pull out day 145 from array	
swe_2016_doWY_146 <-as.matrix(swe_2016[1:6601, 1:5701, 146]) #pull out day 146 from array	
swe_2016_doWY_147 <-as.matrix(swe_2016[1:6601, 1:5701, 147]) #pull out day 147 from array	
swe_2016_doWY_148 <-as.matrix(swe_2016[1:6601, 1:5701, 148]) #pull out day 148 from array	
swe_2016_doWY_149 <-as.matrix(swe_2016[1:6601, 1:5701, 149]) #pull out day 149 from array	
swe_2016_doWY_150 <-as.matrix(swe_2016[1:6601, 1:5701, 150]) #pull out day 150 from array	
swe_2016_doWY_151 <-as.matrix(swe_2016[1:6601, 1:5701, 151]) #pull out day 151 from array	
swe_2016_doWY_152 <-as.matrix(swe_2016[1:6601, 1:5701, 152]) #pull out day 152 from array	
swe_2016_doWY_153 <-as.matrix(swe_2016[1:6601, 1:5701, 153]) #pull out day 153 from array	
swe_2016_doWY_154 <-as.matrix(swe_2016[1:6601, 1:5701, 154]) #pull out day 154 from array	
swe_2016_doWY_155 <-as.matrix(swe_2016[1:6601, 1:5701, 155]) #pull out day 155 from array	
swe_2016_doWY_156 <-as.matrix(swe_2016[1:6601, 1:5701, 156]) #pull out day 156 from array	
swe_2016_doWY_157 <-as.matrix(swe_2016[1:6601, 1:5701, 157]) #pull out day 157 from array	
swe_2016_doWY_158 <-as.matrix(swe_2016[1:6601, 1:5701, 158]) #pull out day 158 from array	
swe_2016_doWY_159 <-as.matrix(swe_2016[1:6601, 1:5701, 159]) #pull out day 159 from array	
swe_2016_doWY_160 <-as.matrix(swe_2016[1:6601, 1:5701, 160]) #pull out day 160 from array	
swe_2016_doWY_161 <-as.matrix(swe_2016[1:6601, 1:5701, 161]) #pull out day 161 from array	
swe_2016_doWY_162 <-as.matrix(swe_2016[1:6601, 1:5701, 162]) #pull out day 162 from array	
swe_2016_doWY_163 <-as.matrix(swe_2016[1:6601, 1:5701, 163]) #pull out day 163 from array	
swe_2016_doWY_164 <-as.matrix(swe_2016[1:6601, 1:5701, 164]) #pull out day 164 from array	
swe_2016_doWY_165 <-as.matrix(swe_2016[1:6601, 1:5701, 165]) #pull out day 165 from array	
swe_2016_doWY_166 <-as.matrix(swe_2016[1:6601, 1:5701, 166]) #pull out day 166 from array	
swe_2016_doWY_167 <-as.matrix(swe_2016[1:6601, 1:5701, 167]) #pull out day 167 from array	
swe_2016_doWY_168 <-as.matrix(swe_2016[1:6601, 1:5701, 168]) #pull out day 168 from array	
swe_2016_doWY_169 <-as.matrix(swe_2016[1:6601, 1:5701, 169]) #pull out day 169 from array	
swe_2016_doWY_170 <-as.matrix(swe_2016[1:6601, 1:5701, 170]) #pull out day 170 from array	
swe_2016_doWY_171 <-as.matrix(swe_2016[1:6601, 1:5701, 171]) #pull out day 171 from array	
swe_2016_doWY_172 <-as.matrix(swe_2016[1:6601, 1:5701, 172]) #pull out day 172 from array	
swe_2016_doWY_173 <-as.matrix(swe_2016[1:6601, 1:5701, 173]) #pull out day 173 from array	
swe_2016_doWY_174 <-as.matrix(swe_2016[1:6601, 1:5701, 174]) #pull out day 174 from array	
swe_2016_doWY_175 <-as.matrix(swe_2016[1:6601, 1:5701, 175]) #pull out day 175 from array	
swe_2016_doWY_176 <-as.matrix(swe_2016[1:6601, 1:5701, 176]) #pull out day 176 from array	
swe_2016_doWY_177 <-as.matrix(swe_2016[1:6601, 1:5701, 177]) #pull out day 177 from array	
swe_2016_doWY_178 <-as.matrix(swe_2016[1:6601, 1:5701, 178]) #pull out day 178 from array	
swe_2016_doWY_179 <-as.matrix(swe_2016[1:6601, 1:5701, 179]) #pull out day 179 from array	
swe_2016_doWY_180 <-as.matrix(swe_2016[1:6601, 1:5701, 180]) #pull out day 180 from array	
swe_2016_doWY_181 <-as.matrix(swe_2016[1:6601, 1:5701, 181]) #pull out day 181 from array	
swe_2016_doWY_182 <-as.matrix(swe_2016[1:6601, 1:5701, 182]) #pull out day 182 from array	
swe_2016_doWY_183 <-as.matrix(swe_2016[1:6601, 1:5701, 183]) #pull out day 183 from array	
swe_2016_doWY_184 <-as.matrix(swe_2016[1:6601, 1:5701, 184]) #pull out day 184 from array	
swe_2016_doWY_185 <-as.matrix(swe_2016[1:6601, 1:5701, 185]) #pull out day 185 from array	
swe_2016_doWY_186 <-as.matrix(swe_2016[1:6601, 1:5701, 186]) #pull out day 186 from array	
swe_2016_doWY_187 <-as.matrix(swe_2016[1:6601, 1:5701, 187]) #pull out day 187 from array	
swe_2016_doWY_188 <-as.matrix(swe_2016[1:6601, 1:5701, 188]) #pull out day 188 from array	
swe_2016_doWY_189 <-as.matrix(swe_2016[1:6601, 1:5701, 189]) #pull out day 189 from array	
swe_2016_doWY_190 <-as.matrix(swe_2016[1:6601, 1:5701, 190]) #pull out day 190 from array	
swe_2016_doWY_191 <-as.matrix(swe_2016[1:6601, 1:5701, 191]) #pull out day 191 from array	
swe_2016_doWY_192 <-as.matrix(swe_2016[1:6601, 1:5701, 192]) #pull out day 192 from array	
swe_2016_doWY_193 <-as.matrix(swe_2016[1:6601, 1:5701, 193]) #pull out day 193 from array	
swe_2016_doWY_194 <-as.matrix(swe_2016[1:6601, 1:5701, 194]) #pull out day 194 from array	
swe_2016_doWY_195 <-as.matrix(swe_2016[1:6601, 1:5701, 195]) #pull out day 195 from array	
swe_2016_doWY_196 <-as.matrix(swe_2016[1:6601, 1:5701, 196]) #pull out day 196 from array	
swe_2016_doWY_197 <-as.matrix(swe_2016[1:6601, 1:5701, 197]) #pull out day 197 from array	
swe_2016_doWY_198 <-as.matrix(swe_2016[1:6601, 1:5701, 198]) #pull out day 198 from array	
swe_2016_doWY_199 <-as.matrix(swe_2016[1:6601, 1:5701, 199]) #pull out day 199 from array	
swe_2016_doWY_200 <-as.matrix(swe_2016[1:6601, 1:5701, 200]) #pull out day 200 from array	
swe_2016_doWY_201 <-as.matrix(swe_2016[1:6601, 1:5701, 201]) #pull out day 201 from array	
swe_2016_doWY_202 <-as.matrix(swe_2016[1:6601, 1:5701, 202]) #pull out day 202 from array	
swe_2016_doWY_203 <-as.matrix(swe_2016[1:6601, 1:5701, 203]) #pull out day 203 from array	
swe_2016_doWY_204 <-as.matrix(swe_2016[1:6601, 1:5701, 204]) #pull out day 204 from array	
swe_2016_doWY_205 <-as.matrix(swe_2016[1:6601, 1:5701, 205]) #pull out day 205 from array	
swe_2016_doWY_206 <-as.matrix(swe_2016[1:6601, 1:5701, 206]) #pull out day 206 from array	
swe_2016_doWY_207 <-as.matrix(swe_2016[1:6601, 1:5701, 207]) #pull out day 207 from array	
swe_2016_doWY_208 <-as.matrix(swe_2016[1:6601, 1:5701, 208]) #pull out day 208 from array	
swe_2016_doWY_209 <-as.matrix(swe_2016[1:6601, 1:5701, 209]) #pull out day 209 from array	
swe_2016_doWY_210 <-as.matrix(swe_2016[1:6601, 1:5701, 210]) #pull out day 210 from array	
swe_2016_doWY_211 <-as.matrix(swe_2016[1:6601, 1:5701, 211]) #pull out day 211 from array	
swe_2016_doWY_212 <-as.matrix(swe_2016[1:6601, 1:5701, 212]) #pull out day 212 from array	
swe_2016_doWY_213 <-as.matrix(swe_2016[1:6601, 1:5701, 213]) #pull out day 213 from array	
swe_2016_doWY_214 <-as.matrix(swe_2016[1:6601, 1:5701, 214]) #pull out day 214 from array	
swe_2016_doWY_215 <-as.matrix(swe_2016[1:6601, 1:5701, 215]) #pull out day 215 from array	
swe_2016_doWY_216 <-as.matrix(swe_2016[1:6601, 1:5701, 216]) #pull out day 216 from array	
swe_2016_doWY_217 <-as.matrix(swe_2016[1:6601, 1:5701, 217]) #pull out day 217 from array	
swe_2016_doWY_218 <-as.matrix(swe_2016[1:6601, 1:5701, 218]) #pull out day 218 from array	
swe_2016_doWY_219 <-as.matrix(swe_2016[1:6601, 1:5701, 219]) #pull out day 219 from array	
swe_2016_doWY_220 <-as.matrix(swe_2016[1:6601, 1:5701, 220]) #pull out day 220 from array	
swe_2016_doWY_221 <-as.matrix(swe_2016[1:6601, 1:5701, 221]) #pull out day 221 from array	
swe_2016_doWY_222 <-as.matrix(swe_2016[1:6601, 1:5701, 222]) #pull out day 222 from array	
swe_2016_doWY_223 <-as.matrix(swe_2016[1:6601, 1:5701, 223]) #pull out day 223 from array	
swe_2016_doWY_224 <-as.matrix(swe_2016[1:6601, 1:5701, 224]) #pull out day 224 from array	
swe_2016_doWY_225 <-as.matrix(swe_2016[1:6601, 1:5701, 225]) #pull out day 225 from array	
swe_2016_doWY_226 <-as.matrix(swe_2016[1:6601, 1:5701, 226]) #pull out day 226 from array	
swe_2016_doWY_227 <-as.matrix(swe_2016[1:6601, 1:5701, 227]) #pull out day 227 from array	
swe_2016_doWY_228 <-as.matrix(swe_2016[1:6601, 1:5701, 228]) #pull out day 228 from array	
swe_2016_doWY_229 <-as.matrix(swe_2016[1:6601, 1:5701, 229]) #pull out day 229 from array	
swe_2016_doWY_230 <-as.matrix(swe_2016[1:6601, 1:5701, 230]) #pull out day 230 from array	
swe_2016_doWY_231 <-as.matrix(swe_2016[1:6601, 1:5701, 231]) #pull out day 231 from array	
swe_2016_doWY_232 <-as.matrix(swe_2016[1:6601, 1:5701, 232]) #pull out day 232 from array	
swe_2016_doWY_233 <-as.matrix(swe_2016[1:6601, 1:5701, 233]) #pull out day 233 from array	
swe_2016_doWY_234 <-as.matrix(swe_2016[1:6601, 1:5701, 234]) #pull out day 234 from array	
swe_2016_doWY_235 <-as.matrix(swe_2016[1:6601, 1:5701, 235]) #pull out day 235 from array	
swe_2016_doWY_236 <-as.matrix(swe_2016[1:6601, 1:5701, 236]) #pull out day 236 from array	
swe_2016_doWY_237 <-as.matrix(swe_2016[1:6601, 1:5701, 237]) #pull out day 237 from array	
swe_2016_doWY_238 <-as.matrix(swe_2016[1:6601, 1:5701, 238]) #pull out day 238 from array	
swe_2016_doWY_239 <-as.matrix(swe_2016[1:6601, 1:5701, 239]) #pull out day 239 from array	
swe_2016_doWY_240 <-as.matrix(swe_2016[1:6601, 1:5701, 240]) #pull out day 240 from array	
swe_2016_doWY_241 <-as.matrix(swe_2016[1:6601, 1:5701, 241]) #pull out day 241 from array	
swe_2016_doWY_242 <-as.matrix(swe_2016[1:6601, 1:5701, 242]) #pull out day 242 from array	
swe_2016_doWY_243 <-as.matrix(swe_2016[1:6601, 1:5701, 243]) #pull out day 243 from array	
swe_2016_doWY_244 <-as.matrix(swe_2016[1:6601, 1:5701, 244]) #pull out day 244 from array	
swe_2016_doWY_245 <-as.matrix(swe_2016[1:6601, 1:5701, 245]) #pull out day 245 from array	
swe_2016_doWY_246 <-as.matrix(swe_2016[1:6601, 1:5701, 246]) #pull out day 246 from array	
swe_2016_doWY_247 <-as.matrix(swe_2016[1:6601, 1:5701, 247]) #pull out day 247 from array	
swe_2016_doWY_248 <-as.matrix(swe_2016[1:6601, 1:5701, 248]) #pull out day 248 from array	
swe_2016_doWY_249 <-as.matrix(swe_2016[1:6601, 1:5701, 249]) #pull out day 249 from array	
swe_2016_doWY_250 <-as.matrix(swe_2016[1:6601, 1:5701, 250]) #pull out day 250 from array	
swe_2016_doWY_251 <-as.matrix(swe_2016[1:6601, 1:5701, 251]) #pull out day 251 from array	
swe_2016_doWY_252 <-as.matrix(swe_2016[1:6601, 1:5701, 252]) #pull out day 252 from array	
swe_2016_doWY_253 <-as.matrix(swe_2016[1:6601, 1:5701, 253]) #pull out day 253 from array	
swe_2016_doWY_254 <-as.matrix(swe_2016[1:6601, 1:5701, 254]) #pull out day 254 from array	
swe_2016_doWY_255 <-as.matrix(swe_2016[1:6601, 1:5701, 255]) #pull out day 255 from array	
swe_2016_doWY_256 <-as.matrix(swe_2016[1:6601, 1:5701, 256]) #pull out day 256 from array	
swe_2016_doWY_257 <-as.matrix(swe_2016[1:6601, 1:5701, 257]) #pull out day 257 from array	
swe_2016_doWY_258 <-as.matrix(swe_2016[1:6601, 1:5701, 258]) #pull out day 258 from array	
swe_2016_doWY_259 <-as.matrix(swe_2016[1:6601, 1:5701, 259]) #pull out day 259 from array	
swe_2016_doWY_260 <-as.matrix(swe_2016[1:6601, 1:5701, 260]) #pull out day 260 from array	
swe_2016_doWY_261 <-as.matrix(swe_2016[1:6601, 1:5701, 261]) #pull out day 261 from array	
swe_2016_doWY_262 <-as.matrix(swe_2016[1:6601, 1:5701, 262]) #pull out day 262 from array	
swe_2016_doWY_263 <-as.matrix(swe_2016[1:6601, 1:5701, 263]) #pull out day 263 from array	
swe_2016_doWY_264 <-as.matrix(swe_2016[1:6601, 1:5701, 264]) #pull out day 264 from array	
swe_2016_doWY_265 <-as.matrix(swe_2016[1:6601, 1:5701, 265]) #pull out day 265 from array	
swe_2016_doWY_266 <-as.matrix(swe_2016[1:6601, 1:5701, 266]) #pull out day 266 from array	
swe_2016_doWY_267 <-as.matrix(swe_2016[1:6601, 1:5701, 267]) #pull out day 267 from array	
swe_2016_doWY_268 <-as.matrix(swe_2016[1:6601, 1:5701, 268]) #pull out day 268 from array	
swe_2016_doWY_269 <-as.matrix(swe_2016[1:6601, 1:5701, 269]) #pull out day 269 from array	
swe_2016_doWY_270 <-as.matrix(swe_2016[1:6601, 1:5701, 270]) #pull out day 270 from array	
swe_2016_doWY_271 <-as.matrix(swe_2016[1:6601, 1:5701, 271]) #pull out day 271 from array	
swe_2016_doWY_272 <-as.matrix(swe_2016[1:6601, 1:5701, 272]) #pull out day 272 from array	
swe_2016_doWY_273 <-as.matrix(swe_2016[1:6601, 1:5701, 273]) #pull out day 273 from array	
swe_2016_doWY_274 <-as.matrix(swe_2016[1:6601, 1:5701, 274]) #pull out day 274 from array	
swe_2016_doWY_275 <-as.matrix(swe_2016[1:6601, 1:5701, 275]) #pull out day 275 from array	
swe_2016_doWY_276 <-as.matrix(swe_2016[1:6601, 1:5701, 276]) #pull out day 276 from array	
swe_2016_doWY_277 <-as.matrix(swe_2016[1:6601, 1:5701, 277]) #pull out day 277 from array	
swe_2016_doWY_278 <-as.matrix(swe_2016[1:6601, 1:5701, 278]) #pull out day 278 from array	
swe_2016_doWY_279 <-as.matrix(swe_2016[1:6601, 1:5701, 279]) #pull out day 279 from array	
swe_2016_doWY_280 <-as.matrix(swe_2016[1:6601, 1:5701, 280]) #pull out day 280 from array	
swe_2016_doWY_281 <-as.matrix(swe_2016[1:6601, 1:5701, 281]) #pull out day 281 from array	
swe_2016_doWY_282 <-as.matrix(swe_2016[1:6601, 1:5701, 282]) #pull out day 282 from array	
swe_2016_doWY_283 <-as.matrix(swe_2016[1:6601, 1:5701, 283]) #pull out day 283 from array	
swe_2016_doWY_284 <-as.matrix(swe_2016[1:6601, 1:5701, 284]) #pull out day 284 from array	
swe_2016_doWY_285 <-as.matrix(swe_2016[1:6601, 1:5701, 285]) #pull out day 285 from array	
swe_2016_doWY_286 <-as.matrix(swe_2016[1:6601, 1:5701, 286]) #pull out day 286 from array	
swe_2016_doWY_287 <-as.matrix(swe_2016[1:6601, 1:5701, 287]) #pull out day 287 from array	
swe_2016_doWY_288 <-as.matrix(swe_2016[1:6601, 1:5701, 288]) #pull out day 288 from array	
swe_2016_doWY_289 <-as.matrix(swe_2016[1:6601, 1:5701, 289]) #pull out day 289 from array	
swe_2016_doWY_290 <-as.matrix(swe_2016[1:6601, 1:5701, 290]) #pull out day 290 from array	
swe_2016_doWY_291 <-as.matrix(swe_2016[1:6601, 1:5701, 291]) #pull out day 291 from array	
swe_2016_doWY_292 <-as.matrix(swe_2016[1:6601, 1:5701, 292]) #pull out day 292 from array	
swe_2016_doWY_293 <-as.matrix(swe_2016[1:6601, 1:5701, 293]) #pull out day 293 from array	
swe_2016_doWY_294 <-as.matrix(swe_2016[1:6601, 1:5701, 294]) #pull out day 294 from array	
swe_2016_doWY_295 <-as.matrix(swe_2016[1:6601, 1:5701, 295]) #pull out day 295 from array	
swe_2016_doWY_296 <-as.matrix(swe_2016[1:6601, 1:5701, 296]) #pull out day 296 from array	
swe_2016_doWY_297 <-as.matrix(swe_2016[1:6601, 1:5701, 297]) #pull out day 297 from array	
swe_2016_doWY_298 <-as.matrix(swe_2016[1:6601, 1:5701, 298]) #pull out day 298 from array	
swe_2016_doWY_299 <-as.matrix(swe_2016[1:6601, 1:5701, 299]) #pull out day 299 from array	
swe_2016_doWY_300 <-as.matrix(swe_2016[1:6601, 1:5701, 300]) #pull out day 300 from array	
swe_2016_doWY_301 <-as.matrix(swe_2016[1:6601, 1:5701, 301]) #pull out day 301 from array	
swe_2016_doWY_302 <-as.matrix(swe_2016[1:6601, 1:5701, 302]) #pull out day 302 from array	
swe_2016_doWY_303 <-as.matrix(swe_2016[1:6601, 1:5701, 303]) #pull out day 303 from array	
swe_2016_doWY_304 <-as.matrix(swe_2016[1:6601, 1:5701, 304]) #pull out day 304 from array	
swe_2016_doWY_305 <-as.matrix(swe_2016[1:6601, 1:5701, 305]) #pull out day 305 from array	
swe_2016_doWY_306 <-as.matrix(swe_2016[1:6601, 1:5701, 306]) #pull out day 306 from array	
swe_2016_doWY_307 <-as.matrix(swe_2016[1:6601, 1:5701, 307]) #pull out day 307 from array	
swe_2016_doWY_308 <-as.matrix(swe_2016[1:6601, 1:5701, 308]) #pull out day 308 from array	
swe_2016_doWY_309 <-as.matrix(swe_2016[1:6601, 1:5701, 309]) #pull out day 309 from array	
swe_2016_doWY_310 <-as.matrix(swe_2016[1:6601, 1:5701, 310]) #pull out day 310 from array	
swe_2016_doWY_311 <-as.matrix(swe_2016[1:6601, 1:5701, 311]) #pull out day 311 from array	
swe_2016_doWY_312 <-as.matrix(swe_2016[1:6601, 1:5701, 312]) #pull out day 312 from array	
swe_2016_doWY_313 <-as.matrix(swe_2016[1:6601, 1:5701, 313]) #pull out day 313 from array	
swe_2016_doWY_314 <-as.matrix(swe_2016[1:6601, 1:5701, 314]) #pull out day 314 from array	
swe_2016_doWY_315 <-as.matrix(swe_2016[1:6601, 1:5701, 315]) #pull out day 315 from array	
swe_2016_doWY_316 <-as.matrix(swe_2016[1:6601, 1:5701, 316]) #pull out day 316 from array	
swe_2016_doWY_317 <-as.matrix(swe_2016[1:6601, 1:5701, 317]) #pull out day 317 from array	
swe_2016_doWY_318 <-as.matrix(swe_2016[1:6601, 1:5701, 318]) #pull out day 318 from array	
swe_2016_doWY_319 <-as.matrix(swe_2016[1:6601, 1:5701, 319]) #pull out day 319 from array	
swe_2016_doWY_320 <-as.matrix(swe_2016[1:6601, 1:5701, 320]) #pull out day 320 from array	
swe_2016_doWY_321 <-as.matrix(swe_2016[1:6601, 1:5701, 321]) #pull out day 321 from array	
swe_2016_doWY_322 <-as.matrix(swe_2016[1:6601, 1:5701, 322]) #pull out day 322 from array	
swe_2016_doWY_323 <-as.matrix(swe_2016[1:6601, 1:5701, 323]) #pull out day 323 from array	
swe_2016_doWY_324 <-as.matrix(swe_2016[1:6601, 1:5701, 324]) #pull out day 324 from array	
swe_2016_doWY_325 <-as.matrix(swe_2016[1:6601, 1:5701, 325]) #pull out day 325 from array	
swe_2016_doWY_326 <-as.matrix(swe_2016[1:6601, 1:5701, 326]) #pull out day 326 from array	
swe_2016_doWY_327 <-as.matrix(swe_2016[1:6601, 1:5701, 327]) #pull out day 327 from array	
swe_2016_doWY_328 <-as.matrix(swe_2016[1:6601, 1:5701, 328]) #pull out day 328 from array	
swe_2016_doWY_329 <-as.matrix(swe_2016[1:6601, 1:5701, 329]) #pull out day 329 from array	
swe_2016_doWY_330 <-as.matrix(swe_2016[1:6601, 1:5701, 330]) #pull out day 330 from array	
swe_2016_doWY_331 <-as.matrix(swe_2016[1:6601, 1:5701, 331]) #pull out day 331 from array	
swe_2016_doWY_332 <-as.matrix(swe_2016[1:6601, 1:5701, 332]) #pull out day 332 from array	
swe_2016_doWY_333 <-as.matrix(swe_2016[1:6601, 1:5701, 333]) #pull out day 333 from array	
swe_2016_doWY_334 <-as.matrix(swe_2016[1:6601, 1:5701, 334]) #pull out day 334 from array	
swe_2016_doWY_335 <-as.matrix(swe_2016[1:6601, 1:5701, 335]) #pull out day 335 from array	
swe_2016_doWY_336 <-as.matrix(swe_2016[1:6601, 1:5701, 336]) #pull out day 336 from array	
swe_2016_doWY_337 <-as.matrix(swe_2016[1:6601, 1:5701, 337]) #pull out day 337 from array	
swe_2016_doWY_338 <-as.matrix(swe_2016[1:6601, 1:5701, 338]) #pull out day 338 from array	
swe_2016_doWY_339 <-as.matrix(swe_2016[1:6601, 1:5701, 339]) #pull out day 339 from array	
swe_2016_doWY_340 <-as.matrix(swe_2016[1:6601, 1:5701, 340]) #pull out day 340 from array	
swe_2016_doWY_341 <-as.matrix(swe_2016[1:6601, 1:5701, 341]) #pull out day 341 from array	
swe_2016_doWY_342 <-as.matrix(swe_2016[1:6601, 1:5701, 342]) #pull out day 342 from array	
swe_2016_doWY_343 <-as.matrix(swe_2016[1:6601, 1:5701, 343]) #pull out day 343 from array	
swe_2016_doWY_344 <-as.matrix(swe_2016[1:6601, 1:5701, 344]) #pull out day 344 from array	
swe_2016_doWY_345 <-as.matrix(swe_2016[1:6601, 1:5701, 345]) #pull out day 345 from array	
swe_2016_doWY_346 <-as.matrix(swe_2016[1:6601, 1:5701, 346]) #pull out day 346 from array	
swe_2016_doWY_347 <-as.matrix(swe_2016[1:6601, 1:5701, 347]) #pull out day 347 from array	
swe_2016_doWY_348 <-as.matrix(swe_2016[1:6601, 1:5701, 348]) #pull out day 348 from array	
swe_2016_doWY_349 <-as.matrix(swe_2016[1:6601, 1:5701, 349]) #pull out day 349 from array	
swe_2016_doWY_350 <-as.matrix(swe_2016[1:6601, 1:5701, 350]) #pull out day 350 from array	
swe_2016_doWY_351 <-as.matrix(swe_2016[1:6601, 1:5701, 351]) #pull out day 351 from array	
swe_2016_doWY_352 <-as.matrix(swe_2016[1:6601, 1:5701, 352]) #pull out day 352 from array	
swe_2016_doWY_353 <-as.matrix(swe_2016[1:6601, 1:5701, 353]) #pull out day 353 from array	
swe_2016_doWY_354 <-as.matrix(swe_2016[1:6601, 1:5701, 354]) #pull out day 354 from array	
swe_2016_doWY_355 <-as.matrix(swe_2016[1:6601, 1:5701, 355]) #pull out day 355 from array	
swe_2016_doWY_356 <-as.matrix(swe_2016[1:6601, 1:5701, 356]) #pull out day 356 from array	
swe_2016_doWY_357 <-as.matrix(swe_2016[1:6601, 1:5701, 357]) #pull out day 357 from array	
swe_2016_doWY_358 <-as.matrix(swe_2016[1:6601, 1:5701, 358]) #pull out day 358 from array	
swe_2016_doWY_359 <-as.matrix(swe_2016[1:6601, 1:5701, 359]) #pull out day 359 from array	
swe_2016_doWY_360 <-as.matrix(swe_2016[1:6601, 1:5701, 360]) #pull out day 360 from array	
swe_2016_doWY_361 <-as.matrix(swe_2016[1:6601, 1:5701, 361]) #pull out day 361 from array	
swe_2016_doWY_362 <-as.matrix(swe_2016[1:6601, 1:5701, 362]) #pull out day 362 from array	
swe_2016_doWY_363 <-as.matrix(swe_2016[1:6601, 1:5701, 363]) #pull out day 363 from array	
swe_2016_doWY_364 <-as.matrix(swe_2016[1:6601, 1:5701, 364]) #pull out day 364 from array	
swe_2016_doWY_365 <-as.matrix(swe_2016[1:6601, 1:5701, 365]) #pull out day 365 from array	
swe_2016_doWY_366 <-as.matrix(swe_2016[1:6601, 1:5701, 366]) #pull out day 366 from array	