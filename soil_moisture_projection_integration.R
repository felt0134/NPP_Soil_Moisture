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
#both changes in mean and variance
merge_future_hist_sc_1<-merge(sgs_swc_2020_2060,future_mean_swc_sc_1,by=c('x','y'))
head(merge_future_hist_sc_1)
merge_future_hist_sc_1$swc.dev<-merge_future_hist_sc_1$future_swc_sc_1.x - merge_future_hist_sc_1$future_swc_sc_1.y

#final touch up
merge_future_hist_sc_2<-merge_future_hist_sc_1[c(1,2,3,5,6)]
colnames(merge_future_hist_sc_2) <-c('x','y','year','swc.mean','swc.dev')
head(merge_future_hist_sc_2)
summary(merge_future_hist_sc_2)

#run historical data through global model
sgs_swc_historical<-sgs_merged_3[c(1,2,3,5,6,8)]
head(sgs_swc_historical)
colnames(sgs_swc_historical) <- c('x','y','swc.mean','year','npp.obs','swc.dev')
str(sgs_swc_historical)
hist.cbind<-cbind(sgs_swc_historical, predict(sgs_global,sgs_swc_historical))
head(hist.cbind)
colnames(hist.cbind) <- c('x','y','swc.mean','year','npp.obs','swc.dev','npp.pred')

#get yearly residuals
hist.cbind$resid<-hist.cbind$npp.obs - hist.cbind$npp.pred

#get npp mean
mean.npp.hist<-aggregate(npp.obs ~ x + y,mean,data=hist.cbind)
mean(mean.npp.hist$npp.obs)
#get npp sd
sd.npp.hist<-aggregate(npp.obs~ x + y,sd,data=hist.cbind)
head(sd.npp.hist)
mean(sd.npp.hist$npp.obs)
#get mean resid
mean.resid.hist<-aggregate(resid~ x + y,mean,data=hist.cbind)
#get sd resid
var.resid.hist<-aggregate(resid~ x + y,var,data=hist.cbind)

head(var.resid.hist)

#region-wide predictions
test.cbind<-cbind(merge_future_hist_sc_2, predict(sgs_global,merge_future_hist_sc_2))
head(test.cbind)
test.cbind$year <- as.numeric(as.character(test.cbind$year))
colnames(test.cbind)<-c('x','y','year','swc.mean','dev','NPP')
test.cbind.rsds<-merge(test.cbind,median_dev,by=c('x','y'))

#get future mean NPP
#get npp mean
mean.npp.future<-aggregate(NPP ~ x + y,mean,data=test.cbind)
#get npp sd
var.npp.future<-aggregate(NPP ~ x + y,var,data=test.cbind)
head(var.npp.future)

#merge relvent datasets

#means
merge.means<-merge(mean.npp.future,mean.resid.hist,by=c('x','y'))
head(merge.means)
merge.means$npp.future<-merge.means$NPP + merge.means$resid
summary(merge.means)
merge.means.2<-merge(merge.means,mean.npp.hist,by=c('x','y'))
head(merge.means.2)
merge.means.2$npp.change<-merge.means.2$npp.future - merge.means.2$npp.obs
hist(merge.means.2$npp.change)
merge.means.3<-merge.means.2[c(1,2,7)]
mean.change.raster<-rasterFromXYZ(merge.means.3)
plot(mean.change.raster)

#mean changes
break_mean_sgs_npp_change<-quantile(merge.means.3$npp.change,seq(from=0.01, to = 0.99,by=0.01),na.rm=TRUE)
par(mfrow=c(1,1))
one<-spplot(mean.change.raster,#scales = list(draw = TRUE),
            at=break_mean_sgs_npp_change,
            asp=1,
            col.regions =
              rev(terrain.colors(length(break_mean_sgs_npp_change)-1)),
            main="Change in mean rangeland productivity") +
  latticeExtra::layer(sp.polygons(states_all_sites, lwd = 0.1))

#standard deviations
merge.var<-merge(var.npp.future,var.resid.hist,by=c('x','y'))
head(merge.var)
merge.var$future.sd<-sqrt(merge.var$NPP + merge.var$resid)
mean(merge.var$future.sd)
#merge to compare with historical sds

merge.var.2<-merge(merge.var,sd.npp.hist,by=c('x','y'))
head(merge.var.2)
merge.var.2$future.sd.change <- merge.var.2$future.sd - merge.var.2$npp.obs
hist(merge.var.2$future.sd.change)
merge.var.3<-merge.var.2[c(1,2,7)]
var.change.raster<-rasterFromXYZ(merge.var.3)
plot(var.change.raster)
summary(merge.var.2)

# changes on variation
break_sd_sgs_npp_change<-quantile(merge.var.3$future.sd.change,seq(from=0.01, to = 0.99,by=0.01),na.rm=TRUE)

one<-spplot(var.change.raster,#scales = list(draw = TRUE),
            at=break_sd_sgs_npp_change,
            asp=1,
            col.regions =
              rev(heat_hcl(length(break_sd_sgs_npp_change)-1)),
            main="Change in variability of rangeland productivity") +
  latticeExtra::layer(sp.polygons(states_all_sites, lwd = 0.1))


h1<-density(rnorm(1000,mean=182.85,sd=39.84))
h2<-density(rnorm(1000,mean=187.82,sd=41.45))
plot(h1)
plot(h2)

#stopped here

#graphs




#tests

#region-wide future estimates
mean.future<- mean(test.cbind$NPP) + mean(hist.cbind$resid)
sd.future <-sqrt(var(test.cbind$NPP) + var(hist.cbind$resid))
xx <- seq(min(merge.means.2$npp.future),max(merge.means.2$npp.future),1)

plot(dnorm(xx,mean=mean.future,sd = sd.future)),
     xlab="NPP",ylab="Density",ylim=c(0,0.03),type="l",lwd=2,col=my_colors[1],
     main="Total variation"))

lines(density(merge.means.2$npp.future),
      lwd=2,col=my_colors[2])

#historical distributions
mean.past<-mean(hist.cbind$npp.obs)
sd.past<-sd(hist.cbind$npp.obs)

x <- seq(60, 700, length.out=100)
mean.future<-mean(merge.means$npp.future)
sd.future<-mean(merge.var$future.sd)
lines(x, y, col = "red")
pnorm()
par(mfrow=c(1,1))
plot(density(bootstrap_historic),
     xlab="NPP",ylab="Density", xlim=c(-70,250), ylim=c(0,0.02),
     type="l",lwd=2,col=my_colors[1],
     main="Total variation")
lines(density(bootstrap_future),
      lwd=2,col=my_colors[2])
legend("topleft",c("Historical","Future"),
       lty="solid",lwd=2, col=my_colors,cex=0.7,bty="n")

# Calculate shifts in means and variances of the response
mean_past = mean(y)
sd_past = sd(y)
q10_past = quantile(y,0.1)

mean_future = mean(preds_xnew) + mean(resids_x)
sd_future = sqrt(var(preds_xnew) + var(resids_x))
prob_q10_future = pnorm(q10_past,mean_future,sd_future)

#stopped here

hist(test.cbind$NPP)
hist(test.cbind$total)
head(yearly.NPP)
sd(df.residuals.sgs.3$resid)
#get rid of excess columns
test.cbind.future<-test.cbind[c(1,2,3,6,7,8)]
head(test.cbind.future)
as.data.frame(test.cbind.future)
str(test.cbind.future)
summary(test.cbind.future)


#yearly npp historical data
head(sgs_merged_3)
sgs_merged_4<-sgs_merged_3[c(1,2,5,6)]
head(sgs_merged_4)
colnames(sgs_merged_4) <- c('x','y','year','NPP')

#yearly npp for future projections
#purely soil moisture prediction
yearly.NPP<-aggregate(NPP~year,mean,data=test.cbind.rsds)
head(yearly.NPP)
plot(NPP~year,data=yearly.NPP)
summary(yearly.NPP)
yearly.total<-aggregate(total~year,mean,data=test.cbind.rsds)
head(yearly.total)
plot(total~year,data=yearly.total)
summary(yearly.total)
View(test.cbind.future)
hist.cbind
#peters plotting code
# figures based on histograms
my_breaks=seq(0,400,5)
my_colors=c(rgb(0,0,1,1/4),rgb(1,0,0,1/4))
h1 <- hist(mean_npp_obs_pred$npp.pred,breaks=my_breaks)
h2 <- hist(mean_npp_obs$npp.obs,breaks=my_breaks)  # this is the histogram of observed values
h3 <- hist(mean.npp.pred$NPP,breaks=my_breaks) 
h4 <- hist(mean.npp.total$total,breaks=my_breaks)
par(mfrow=c(2,1),mar=c(4,4,1,1))
plot(h1,col=my_colors[1],xlim=c(0,400),ylim=c(0,500),xlab="NPP",
     main="Variation caused by soil moisture")
plot(h3,col=my_colors[2],xlim=c(0,400),ylim=c(0,500),add=T)
plot(h2,col=my_colors[1],xlim=c(0,400),ylim=c(0,500),xlab="NPP",
     main="Total variation")
plot(h4,col=my_colors[2],xlim=c(0,400),ylim=c(0,500),add=T)
legend("topleft",c("Historical","Future"),
       fill=my_colors,cex=0.7,bty="n")


h1 <- hist(yearly.NPP$NPP,breaks=my_breaks)
h2 <- hist(yearly.total$total,breaks=my_breaks) 

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

#combine wet and dry
wet_dry_futue<-rbind(yearly.NPP.wet,yearly.NPP.dry)

#getting decadal averages for whole region
sgs_swc_sc_1_future_2020_2030 <- test.cbind.future %>% dplyr::filter(test.cbind.future$year < 2030)
head(sgs_swc_sc_1_future_2020_2030)

yearly.NPP<-aggregate(NPP~year,mean,data=test.cbind.future)
head(yearly.NPP)
plot(NPP~year,data=yearly.NPP)


library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)
