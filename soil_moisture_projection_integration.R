library(raster)
library(plyr)
library(dplyr)
library(reshape2)

########### region-identifying rasters  #############
sites <- "G:/My Drive/range-resilience/Sensitivity/CONUS_rangelands_NPP_Sensitivity/climate_data_for_import/RasterbySiteID3.tif" 
raster_sites<-raster(sites)
plot(raster_sites)

#raster math
raster_sites_rounded <- round(raster_sites/1000000)
plot(raster_sites_rounded) #see what it looks like

#Create a raster for the different regions, I guess this makes the colors more clear?
AFRI_Site_raster <- raster_sites_rounded - raster_sites*1000000
plot(AFRI_Site_raster)

dir.AFRI_Historical <- "G:/My Drive/range-resilience/Sensitivity/Preliminary_work/SoilMoisture_Data" #set working directory
load(file.path(dir.AFRI_Historical, "aggTABLE_allregions.Rdata")) 
head(aggTABLE_allregions)

###ecoegion raster#####

regions <-  c( "CaliforniaAnnual", "ColdDeserts", "HotDeserts", "NorthernMixedSubset", "SGS")

#sites raster as made in step 1
raster_sites<-raster(sites)
plot(raster_sites)

#create a dataframe with values from the raster (so we can link data from the response variables to it)
rastvals <- as.data.frame(values(raster_sites))
names(rastvals) <- "RegionSite"

#View just the values associated with each cell
#Note that the value in the raster includes both the region (in the millions digit; 1 to 5), and the siteID (in the other digits, range 1 to ~20,000 within each region)
values(raster_sites)

#Plot the raster
plot(raster_sites)

#Create a raster for the different regions
raster_sites <- round(raster_sites/1000000)
plot(raster_sites)

#Create a raster for the different regions
raster_sites <- raster_sites - raster_sites*1000000
plot(raster_sites)

####### future water year total precipitation########
#Specify directory where you have the raster
dir.AFRI <- "G:/My Drive/range-resilience/Sensitivity/CONUS_rangelands_NPP_Sensitivity/NPP_Soil_Moisture/SGS_projections_practice/SGS_output_9vars_unzipped"

#future precipdatan
future_precip<-load((file.path(dir.AFRI, "FUT_SWAAprJun_SGS")))
str(future_precip)
summary(future_precip)
data.frame(future_precip)
new.precip<-as.data.frame(dALLsites)

#check values
head(new.precip[,45:85])
str(new.precip)

#make sure values are stored as numeric
for(i in 2:ncol(new.precip)){
  new.precip[,i] <- as.numeric(as.character(new.precip[,i]))
}

#check this didn't alter the data
head(new.precip[,1:10])

#Creating unique IDs
sitenumENDpos = as.integer(regexpr('_', new.precip$site) )
Site <- as.integer(substr(new.precip$site, 1, sitenumENDpos-1) )
Regionname <- substr(new.precip$site, 8, 9)
Regionnum <- unlist(sapply(Regionname, FUN= function(x) grep(x, regions)) )
new.precip$RegionSite <- Regionnum*1000000 + Site

#joining with the ID raster
new.precip_joindat <- join(rastvals, new.precip, by="RegionSite")
dim(new.precip_joindat)
summary()

#historical precip
new.precip_joindat_2<-new.precip_joindat[,1:86]
head(new.precip_joindat_2)
new.precip_joindat_3<-new.precip_joindat_2[-c(2)]
head(new.precip_joindat_3)
dim(new.precip_joindat_3)

future_precip_sc2<-list()

for(i in 2:ncol(new.precip_joindat_3)){
  new.precip_joindat_3[,i] <- as.numeric(as.character(new.precip_joindat_3[,i]))
  test <- raster_sites
  values(test) <- new.precip_joindat_3[,i]
  future_precip_sc2[[i]] <- test
}

#practice
stack_test<-stack(future_precip_sc2[45:85])
plot(stack_test)

#stopped here
# next step is to  focus on future projection scenario 2020 to 2050, put that in a workable data frame, and run through global model

#change to dataframe
future_precip_sgs = rasterToPoints(stack_test); future_precip_sgs_df = data.frame(future_precip_sgs)
head(future_precip_sgs_df)
dim(future_precip_sgs_df)
colnames(future_precip_sgs_df)[3:43] <-paste(2020:2060) #rename columns to years
future_precip_sgs_df_melted_april_june <- melt(future_precip_sgs_df, 
                                     id.vars = c("x", "y"),
                                     variable.name = "year")
head(future_precip_sgs_df_melted)

#future soil moisture july-September

#future precipdatan
future_swc_jul_sep<-load((file.path(dir.AFRI, "FUT_SWAJulSep_SGS")))
str(future_swc_jul_sep)
summary(future_swc_jul_sep)
data.frame(future_swc_jul_sep)
new.swc_jul_sep<-as.data.frame(dALLsites)

#check values
head(new.swc_jul_sep[,45:85])
str(new.swc_jul_sep)

#make sure values are stored as numeric
for(i in 2:ncol(new.swc_jul_sep)){
  new.swc_jul_sep[,i] <- as.numeric(as.character(new.swc_jul_sep[,i]))
}

#check this didn't alter the data
head(new.swc_jul_sep[,1:10])

#Creating unique IDs
sitenumENDpos = as.integer(regexpr('_', new.swc_jul_sep$site) )
Site <- as.integer(substr(new.swc_jul_sep$site, 1, sitenumENDpos-1) )
Regionname <- substr(new.swc_jul_sep$site, 8, 9)
Regionnum <- unlist(sapply(Regionname, FUN= function(x) grep(x, regions)) )
new.swc_jul_sep$RegionSite <- Regionnum*1000000 + Site

#joining with the ID raster
new.swc_jul_sep_joindat <- join(rastvals, new.swc_jul_sep, by="RegionSite")
dim(new.swc_jul_sep_joindat)
summary()

#historical swc_jul_sep
new.swc_jul_sep_joindat_2<-new.swc_jul_sep_joindat[,1:86]
head(new.swc_jul_sep_joindat_2)
new.swc_jul_sep_joindat_3<-new.swc_jul_sep_joindat_2[-c(2)]
head(new.swc_jul_sep_joindat_3)
dim(new.swc_jul_sep_joindat_3)

future_swc_jul_sep_sc2<-list()

for(i in 2:ncol(new.swc_jul_sep_joindat_3)){
  new.swc_jul_sep_joindat_3[,i] <- as.numeric(as.character(new.swc_jul_sep_joindat_3[,i]))
  test <- raster_sites
  values(test) <- new.swc_jul_sep_joindat_3[,i]
  future_swc_jul_sep_sc2[[i]] <- test
}

#practice
stack_test<-stack(future_swc_jul_sep_sc2[45:85])
plot(stack_test)

#stopped here
# next step is to  focus on future projection scenario 2020 to 2050, put that in a workable data frame, and run through global model

#change to dataframe
future_swc_jul_sep_sgs = rasterToPoints(stack_test); future_swc_jul_sep_sgs_df = data.frame(future_swc_jul_sep_sgs)
head(future_swc_jul_sep_sgs_df)
dim(future_swc_jul_sep_sgs_df)
colnames(future_swc_jul_sep_sgs_df)[3:43] <-paste(2020:2060) #rename columns to years
future_swc_jul_sep_sgs_df_melted_april_june <- melt(future_swc_jul_sep_sgs_df, 
                                               id.vars = c("x", "y"),
                                               variable.name = "year")
#sc1 2020-2060 july-September
head(future_swc_jul_sep_sgs_df_melted_april_june)
colnames(future_swc_jul_sep_sgs_df_melted_april_june) <-c('x','y','year','value.2')

merge_future_swc_scenario_1<-merge(future_swc_jul_sep_sgs_df_melted_april_june,future_precip_sgs_df_melted,by=c('x','y','year'))
head(merge_future_swc_scenario_1)
merge_future_swc_scenario_1$future_swc_sc_1<-merge_future_swc_scenario_1$value + merge_future_swc_scenario_1$value.2

sgs_swc_2020_2060<-merge_future_swc_scenario_1[c(1,2,3,6)]
head(sgs_swc_2020_2060)

#future means if needed
future_mean_swc_sc_1<-aggregate(future_swc_sc_1~x + y,mean,data=sgs_swc_2020_2060)
head(future_mean_swc_sc_1)

#merge historical and future projections

merge_future_hist_sc_1<-merge(sgs_swc_2020_2060,sgs_mean_moisture,by=c('x','y'))
head(merge_future_hist_sc_1)
merge_future_hist_sc_1$swc.dev<-merge_future_hist_sc_1$future_swc_sc_1 - merge_future_hist_sc_1$sgs_moisture

#final touch up
merge_future_hist_sc_2<-merge_future_hist_sc_1[c(1,2,3,5,6)]
colnames(merge_future_hist_sc_2) <-c('x','y','year','swc.mean','swc.dev')
head(merge_future_hist_sc_2)
summary(merge_future_hist_sc_2)

#region-wide predictions
test.cbind<-cbind(merge_future_hist_sc_2, predict(sgs_global,merge_future_hist_sc_2))
head(test.cbind)
test.cbind$year <- as.numeric(as.character(test.cbind$year))
colnames(test.cbind)<-c('x','y','year','swc.mean','dev','NPP')
str(test.cbind)

#get rid of excess columns
test.cbind.future<-test.cbind[c(1,2,3,6)]
head(test.cbind.future)
as.data.frame(test.cbind.future)
str(test.cbind.future)

#yearly npp historical data
head(sgs_merged_3)
sgs_merged_4<-sgs_merged_3[c(1,2,5,6)]
head(sgs_merged_4)
colnames(sgs_merged_4) <- c('x','y','year','NPP')

#yearly npp for future projections
yearly.NPP<-aggregate(NPP~year,mean,data=test.cbind.future)
head(yearly.NPP)
plot(NPP~year,data=yearly.NPP)

#dry sites
dry_sites_prediction_sgs_sc_1<-test.cbind %>% filter(swc.mean < 4.5)
summary(dry_sites_prediction_sgs_sc_1)
head(dry_sites_prediction_sgs_sc_1)
dry_sites_prediction_sgs_sc_2<-dry_sites_prediction_sgs_sc_1[c(1,2,3,6)]
head(dry_sites_prediction_sgs_sc_2)

#merge historical and future together
dry_sites_prediction_sgs_sc_3<-rbind(dry_sites_prediction_sgs_sc_2,sgs_merged_4)
yearly.NPP.dry<-aggregate(NPP~year,mean,data=dry_sites_prediction_sgs_sc_2)
head(yearly.NPP.dry)
plot(NPP~year,data=yearly.NPP.dry)
sd(yearly.NPP.dry$NPP)
yearly.NPP.dry$site<-'Dry'

#wetsites
wet_sites_prediction_sgs_sc_1<-test.cbind %>% filter(swc.mean > 4.5)
summary(wet_sites_prediction_sgs_sc_1)
head(wet_sites_prediction_sgs_sc_1)
yearly.NPP.wet<-aggregate(NPP~year,mean,data=wet_sites_prediction_sgs_sc_1)
head(yearly.NPP.wet)
plot(NPP~year,data=yearly.NPP.wet)
sd(yearly.NPP.wet$NPP)
yearly.NPP.wet$site<-'Wet'

#getting decadal averages for whole region
sgs_swc_sc_1_future_2020_2030 <- test.cbind.future %>% dplyr::filter(test.cbind.future$year < 2030)
head(sgs_swc_sc_1_future_2020_2030)

yearly.NPP<-aggregate(NPP~year,mean,data=test.cbind.future)
head(yearly.NPP)
plot(NPP~year,data=yearly.NPP)

#get mean for past 30 years
mean.npp.1986.2015.sgs<-aggregate(npp.x~x+y,mean,data=sgs_merged)
head(mean.npp.1986.2015.sgs)
summary(mean.npp.1986.2015.sgs)
mean.npp.1986.2015.sgs.raster<-rasterFromXYZ(mean.npp.1986.2015.sgs)
plot(mean.npp.1986.2015.sgs)
mean.npp.1986.2015.sgs$time <- 'Historical'
mean.npp.1986.2015.sgs$NPP<- mean.npp.1986.2015.sgs$npp.x
mean.npp.1986.2015.sgs.2<-mean.npp.1986.2015.sgs[-3]
head(mean.npp.1986.2015.sgs.2)

#get mean for next 30 years
mean.npp.2020.2060.sgs<-aggregate(NPP~x+y,mean,data=test.cbind.future)
head(mean.npp.2020.2060.sgs)
summary(mean.npp.2020.2060.sgs)
mean.npp.2020.2060.sgs.raster<-rasterFromXYZ(mean.npp.2020.2060.sgs)
plot(mean.npp.2020.2060.sgs)
mean.npp.2020.2060.sgs$time<- 'Future'

merge.past.future<-rbind(mean.npp.2020.2060.sgs,mean.npp.1986.2015.sgs.2)
head(merge.past.future)
summary(merge.past.future)

par(mfrow=c(1,1))

#2020-2030 mean NPP
mean.npp.2020.2030<-aggregate(NPP~x + y,mean,data=sgs_swc_sc_1_future_2020_2030)
head(mean.npp.2020.2030)
mean.npp.2020.2030.raster<-rasterFromXYZ(mean.npp.2020.2030)
plot(mean.npp.2020.2030.raster)

#graphs
break_mean_sgs_npp<-quantile(mean.npp.1986.2015.sgs$npp.x,seq(from=0.01, to = .99,by=0.01),na.rm=TRUE)

one<-spplot(mean.npp.2020.2060.sgs.raster,#scales = list(draw = TRUE),
       at=break_mean_sgs_npp,
       asp=1,
       col.regions =
         rev(terrain.colors(length(break_mean_sgs_npp)-1)),
       main="mean NPP 2020-2060") +
  latticeExtra::layer(sp.polygons(states_all_sites, lwd = 0.1))

two<-spplot(mean.npp.1986.2015.sgs.raster,#scales = list(draw = TRUE),
            at=break_mean_sgs_npp,
            asp=1,
            col.regions =
              rev(terrain.colors(length(break_mean_sgs_npp)-1)),
            main="mean NPP 1986-2015") +
  latticeExtra::layer(sp.polygons(states_all_sites, lwd = 0.1))

grid.arrange(two,one,ncol=2)

library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)

#2020-2030 mean NPP
mean.npp.2020.2030<-aggregate(NPP~x + y,mean,data=sgs_swc_sc_1_future_2020_2030)
head(mean.npp.2020.2030)
mean.npp.2020.2030.raster<-rasterFromXYZ(mean.npp.2020.2030)
plot(mean.npp.2020.2030.raster)

twenty_thirty<-spplot(mean.npp.2020.2030.raster,#scales = list(draw = TRUE),
            at=break_mean_sgs_npp,
            asp=1,
            col.regions =
              rev(terrain.colors(length(break_mean_sgs_npp)-1)),
            main="mean NPP 2020-2029") +
  latticeExtra::layer(sp.polygons(states_all_sites, lwd = 0.1))

#2030-2040 mean NPP
sgs_swc_sc_1_future_2030_2040 <- test.cbind.future %>% dplyr::filter(2029 < year & year < 2040)
head(sgs_swc_sc_1_future_2030_2040)
mean.npp.2030.2040<-aggregate(NPP~x + y,mean,data=sgs_swc_sc_1_future_2030_2040)
head(mean.npp.2030.2040)
mean.npp.2030.2040.raster<-rasterFromXYZ(mean.npp.2030.2040)
plot(mean.npp.2030.2040.raster)

thirty_forty<-spplot(mean.npp.2030.2040.raster,#scales = list(draw = TRUE),
                      at=break_mean_sgs_npp,
                      asp=1,
                      col.regions =
                        rev(terrain.colors(length(break_mean_sgs_npp)-1)),
                      main="mean NPP 2030-2039") +
  latticeExtra::layer(sp.polygons(states_all_sites, lwd = 0.1))

#2040-2050 mean NPP
sgs_swc_sc_1_future_2040_2050 <- test.cbind.future %>% dplyr::filter(2039 < year & year < 2050)
head(sgs_swc_sc_1_future_2040_2050)
mean.npp.2040.2050<-aggregate(NPP~x + y,mean,data=sgs_swc_sc_1_future_2040_2050)
head(mean.npp.2040.2050)
mean.npp.2040.2050.raster<-rasterFromXYZ(mean.npp.2040.2050)
plot(mean.npp.2040.2050.raster)

forty_fifty<-spplot(mean.npp.2040.2050.raster,#scales = list(draw = TRUE),
                     at=break_mean_sgs_npp,
                     asp=1,
                     col.regions =
                       rev(terrain.colors(length(break_mean_sgs_npp)-1)),
                     main="mean NPP 2040-2049") +
  latticeExtra::layer(sp.polygons(states_all_sites, lwd = 0.1))

#2050-2060 mean NPP
sgs_swc_sc_1_future_2050_2060 <- test.cbind.future %>% dplyr::filter(year > 2049)
head(sgs_swc_sc_1_future_2050_2060)
mean.npp.2050.2060<-aggregate(NPP~x + y,mean,data=sgs_swc_sc_1_future_2050_2060)
head(mean.npp.2050.2060)
mean.npp.2050.2060.raster<-rasterFromXYZ(mean.npp.2050.2060)
plot(mean.npp.2050.2060.raster)


fifty_sixty<-spplot(mean.npp.2050.2060.raster,#scales = list(draw = TRUE),
                    at=break_mean_sgs_npp,
                    asp=1,
                    col.regions =
                      rev(terrain.colors(length(break_mean_sgs_npp)-1)),
                    main="mean NPP 2050-2060") +
  latticeExtra::layer(sp.polygons(states_all_sites, lwd = 0.1))

grid.arrange(twenty_thirty,thirty_forty,forty_fifty,fifty_sixty,nrow=2)


