#geo-locate
#region-identifying rasters
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

#import data

#Specify directory where you have the raster
dir.AFRI <- "G:/My Drive/range-resilience/Sensitivity/CONUS_rangelands_NPP_Sensitivity/NPP_Soil_Moisture/SGS_projections_practice/unzipped"

#Load raster (can name this whatever you want)
AFRI_RegionSite_Raster <- raster(file.path(dir.AFRI , "FUTWatYrPRECIP_SGS"))

#precipdata
future_precip<-load((file.path(dir.AFRI, "FUTWatYrPRECIP_SGS")))
str(future_precip)
summary(future_precip)
head(annualprecip)
data.frame(future_precip)
new.precip<-as.data.frame(dALLsites)
head(new.precip)

#add row labels
new.precip$label <- row.names(new.precip)