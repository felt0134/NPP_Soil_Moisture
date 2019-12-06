library(dplyr)

######produce sgs dataframe for regularization#############################################
#want: mean ppt, ppt deviation, tranpiration deviation
head(rangeland_npp_covariates_1)
sgs<-subset(rangeland_npp_covariates_1,region.x=='semi_arid_steppe')

#get precip deviation
sgs$mm.dev<-sgs$mm.x - sgs$mm.y

#get mean transp
sgs_transp_mean<-aggregate(day_of_50_total_transp~ x + y,mean,data=sgs)
head(sgs_transp_mean)

#merge mean transp with full dataframe
sgs_2<-merge(sgs,sgs_transp_mean,by=c('x','y'))
sgs_2$transp.dev<-sgs_2$day_of_50_total_transp.x - sgs_2$day_of_50_total_transp.y
head(sgs_2)

#for regularization test
sgs_3<-sgs_2[c(1,2,3,4,6,18,19,20,21)]
head(sgs_3)
plot(npp.x~transp.dev,data=sgs_3)

saveRDS(sgs_3, file = "sgs_covariates_for_regularization_1.rds")

#add to sgs_2 for additional exploratory analysis

#get swc mean and deviation
head(sgs_2)
sgs_2$june_september_swc <- sgs_2$july_september_swc + sgs_2$april_june_swc 
sgs_toy_swc_mean<-aggregate(june_september_swc ~ x + y,mean,data=sgs_2)
head(sgs_toy_swc_mean)

#merge mean swc with with sgs_2
sgs_toy_3<-merge(sgs_2,sgs_toy_swc_mean,by=c('x','y'))
head(sgs_toy_3)
#
sgs_toy_3$june_september_swc_deviation <- sgs_toy_3$june_september_swc.x - sgs_toy_3$june_september_swc.y
head(sgs_toy_3)

#lists
list.r.square.sgs.swc.ppt<-list()
list.coefficients.final.sgs.swc.ppt<-list()

#Run the global model
for(i in 1:1000)
{
  
  test.strat.semiarid_steppe<-stratified(semiarid_steppe_above_below, c("map"), 0.01)
  stratified_final_sgs<-merge(test.strat.semiarid_steppe, sgs_toy_3,by=c('x','y'))
  stratified_final_sgs_lm<-lm(npp.x~june_september_swc_deviation*mm.y
                              ,stratified_final_sgs)
  
  newcoef1 <- stratified_final_sgs_lm$coefficients 
  df<-data.frame(newcoef1)
  df$id = i
  list.coefficients.final.sgs.swc.ppt[[i]] <- data.frame(df)
  
  #get r-squared
  #import r-squareds to list
  #veg
  r.square.sgs.swc<-summary(stratified_final_sgs_lm)$r.squared
  r.square.sgs.swc.df<-data.frame(r.square.sgs.swc)
  r.square.sgs.swc.df$id <- i
  list.r.square.sgs.swc.ppt[[i]] <- data.frame(r.square.sgs.swc.df)
}

#dataframe for slopes
summary(stratified_final_sgs_lm)
df.coefficients.sgs.swc.ppt <- do.call("rbind", list.coefficients.final.sgs.swc.ppt)
head(df.coefficients.sgs.swc.ppt)
df.coefficients.sgs.swc.ppt.2 <- cbind(rownames(df.coefficients.sgs.swc.ppt), data.frame(df.coefficients.sgs.swc.ppt, row.names=NULL))
head(df.coefficients.sgs.swc.ppt.2)
colnames(df.coefficients.sgs.swc.ppt.2)  <- c("predictor","coefficient","run.id")

df.coefficients.sgs.swc.ppt.2$predictor<-gsub('[[:digit:]]+', '', df.coefficients.sgs.2$predictor)
df.coefficients.sgs.swc.ppt.2$predictor<-gsub(':', '_', df.coefficients.sgs.2$predictor)
df2.sgs.swc.ppt<-reshape(df.coefficients.sgs.swc.ppt.2, idvar = "run.id", timevar = "predictor", direction = "wide")
colnames(df2.sgs)
head(df2.sgs.swc.ppt)

#dataframe for r-squared
df.square.sgs.ppt.swc <- do.call("rbind",  list.r.square.sgs.swc.ppt)
colnames(df.square.sgs.ppt.swc)[colnames(df.square.sgs.ppt.swc)=="r.square.sgs.precip"] <- "r.square"
df.square.sgs.ppt.swc$model<-'map_swc_dev'
head(df.square.sgs.ppt.swc)

#global model for map and ppt deviation
list.r.square.sgs.map.ppt <-list()

for(i in 1:1000)
{
  
  test.strat.semiarid_steppe<-stratified(semiarid_steppe_above_below, c("map"), 0.01)
  stratified_final_sgs<-merge(test.strat.semiarid_steppe, sgs_toy_3,by=c('x','y'))
  stratified_final_sgs_lm<-lm(npp.x~mm.dev*mm.y
                              ,stratified_final_sgs)
  
  #get r-squared
  #import r-squareds to list
  #veg
  r.square.sgs.swc<-summary(stratified_final_sgs_lm)$r.squared
  r.square.sgs.swc.df<-data.frame(r.square.sgs.swc)
  r.square.sgs.swc.df$id <- i
  list.r.square.sgs.map.ppt[[i]] <- data.frame(r.square.sgs.swc.df)
}


#dataframe for r-squared
df.square.sgs.ppt.map<- do.call("rbind",  list.r.square.sgs.map.ppt)
colnames(df.square.sgs.ppt.map)[colnames(df.square.sgs.ppt.map)=="r.square.sgs.precip"] <- "r.square"
df.square.sgs.ppt.map$model<-'map_ppt_dev'
head(df.square.sgs.ppt.map)

merge.sgs.swc.precip.r.square<-rbind(df.square.sgs.ppt.map,df.square.sgs.ppt.swc)
head(merge.sgs.swc.precip.r.square)
View(merge.sgs.swc.precip.r.square)

#plot to compare
library(ggplot2)
ggplot(merge.sgs.swc.precip.r.square,aes(x=r.square.sgs.swc,fill=model)) +
  geom_histogram(binwidth = 0.01,color='black',alpha=0.5) +
  xlab('Model r-square') +
  ylab('')

library(raster)
sgs_swc_raster<- rasterFromXYZ(sgs_toy_swc_mean)
plot(sgs_swc_raster)
plot(cper_extent,add=TRUE)
cper_extent<-extent(-104.77, 104.77, 40.82, 40.82)

sgs_swc_raster_3<-crop(sgs_swc_raster,extent(sgs_raster_2))
sgs_raster_2<-zoom(sgs_swc_raster)
sgs_swc_raster_3<-crop(sgs_swc_raster,extent(sgs_raster_2))
plot(sgs_swc_raster_3)
raster(sgs_swc_raster_3)
sgs_raster_2_df<-rasterFromXYZ(sgs_swc_raster_3)
head(sgs_raster_2_df)
#workaround
library(data.table)
dt <- data.table(as.data.frame(sgs_swc_raster_3, xy = TRUE))
head(dt) #worked
sgs_toy_4<-merge(sgs_toy_3,dt,by=c('x','y'))
head(sgs_toy_4)
par(mfrow = c(2, 2))
plot(npp.x~mm.dev,data=sgs_toy_4,xlab='precip deviation')
plot(npp.x~june_september_swc_deviation,data=sgs_toy_4,xlab='soil moisture deviation')
plot(npp.x~transp.dev,data=sgs_toy_4,xlab='transpiration deviation')
plot(sgs_swc_raster_3,main='mean swc near cper')

######california annuals toy dataframe#######################################################
head(rangeland_npp_covariates_1)
cali_toy<-subset(rangeland_npp_covariates_1,region.x=='california_annuals')
head(cali_toy)
#get precip deviation
cali_toy$mm.dev<-cali_toy$mm.x - cali_toy$mm.y

#get mean transp
cali_toy_mean<-aggregate(day_of_50_total_transp~ x + y,mean,data=cali_toy)
head(cali_toy_mean)

#merge mean transp with full dataframe
cali_toy_2<-merge(cali_toy,cali_toy_mean,by=c('x','y'))
cali_toy_2$transp.dev<-cali_toy_2$day_of_50_total_transp.x - cali_toy_2$day_of_50_total_transp.y

head(cali_toy_2)
cali_toy_2$jan_june_swc <- cali_toy_2$jan_march_swc + cali_toy_2$april_june_swc 
cali_toy_swc_mean<-aggregate(jan_june_swc ~ x + y,mean,data=cali_toy_2)
head(cali_toy_swc_mean)

#merge mean swc with with cali_toy_2
cali_toy_3<-merge(cali_toy_2,cali_toy_swc_mean,by=c('x','y'))
head(cali_toy_3)

#get swc deviation
cali_toy_3$jan_june_swc_deviation <- cali_toy_3$jan_june_swc.x - cali_toy_3$jan_june_swc.y
head(cali_toy_3)
cali_toy_final <- cali_toy_3 %>%
  select(x, y, year, region.x,npp.x,mm.y,mm.dev,day_of_50_total_transp.y,transp.dev,jan_june_swc.y,jan_june_swc_deviation)

head(cali_toy_final)
saveRDS(cali_toy_final, file = "cali_toy_dataset_1.rds")

#run through global model with mean ppt*swc dev
