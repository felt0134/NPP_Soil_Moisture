library(raster)
library(plyr)
library(dplyr)

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
dir.AFRI <- "G:/My Drive/range-resilience/Sensitivity/CONUS_rangelands_NPP_Sensitivity/NPP_Soil_Moisture/SGS_projections_practice/unzipped"

#future precipdatan
future_precip<-load((file.path(dir.AFRI, "FUTWatYrPRECIP_SGS")))
str(future_precip)
summary(future_precip)
data.frame(future_precip)
new.precip<-as.data.frame(dALLsites)

#check values
head(new.precip[,1:10])
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

#tests
new.precip_joindat_2<-new.precip_joindat[,1:10]
head(new.precip_joindat_2)
new.precip_joindat_3<-new.precip_joindat_2[-c(2)]
head(new.precip_joindat_3)
dim(new.precip_joindat_3)

future_precip<-list()

for(i in 2:ncol(new.precip_joindat_3)){
  new.precip_joindat_3[,i] <- as.numeric(as.character(new.precip_joindat_3[,i]))
  test <- raster_sites
  values(test) <- new.precip_joindat_3[,i]
  future_precip[[i]] <- test
}

#practice
stack_test<-stack(future_precip[2:9])
plot(stack_test)

WatYrprecip_joindat_3<-WatYrprecip_joindat_2[,1:10]

#stopped here
# next step is to  focus on future projection scenario 2020 to 2050, put that in a workable data frame, and run through global model

#change to dataframe
water_yearprecip_cali_p = rasterToPoints(stack_test); df_water_yearprecip_cali = data.frame(water_yearprecip_cali_p)
head(df_water_yearprecip_cali)
colnames(df_water_yearprecip_cali)[3:32] <-paste(1986:2015) #rename columns to years
cali_water_yearprecip_melted <- melt(df_water_yearprecip_cali, 
                                     id.vars = c("x", "y"),
                                     variable.name = "year")