library(raster)
library(plyr)
library(dplyr)

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


#######water year total precipitation########
#Specify directory where you have the raster
dir.AFRI <- "G:/My Drive/range-resilience/Sensitivity/CONUS_rangelands_NPP_Sensitivity/NPP_Soil_Moisture/SGS_projections_practice/unzipped"

#future precipdata
future_precip<-load((file.path(dir.AFRI, "FUTWatYrPRECIP_SGS")))
str(future_precip)
summary(future_precip)
data.frame(future_precip)
new.precip<-as.data.frame(dALLsites)
head(new.precip)

#add label to dataset based on row names
new.precip<- new.precip[,-102]
new.precip$label <- row.names(new.precip)

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

#
new.precip_joindat_2<-new.precip_joindat[,c(1:71)]
head(new.precip_joindat_2)
View(new.precip_joindat_2)

#
plot(raster_sites)
#use joined data to populate values for a raster
WatYrprecip_done <- list()

#1986
WatYrprecip_1986<- raster_sites
values(WatYrprecip_1986) <- WatYrprecip_joindat_2[,"1986"]
WatYrprecip_done[["WatYrprecip_1986"]] <-WatYrprecip_1986

#
x <- raster_sites