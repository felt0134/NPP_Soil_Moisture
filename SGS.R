#sgs - remove outliers and produce a global model fit of soil moisture sensitivity

head(sensitivity_sgs_swc_coef_only_2)
hist(sensitivity_sgs_swc_coef_only_2$coef)
mean(sensitivity_sgs_swc_coef_only_2$coef)
sd(sensitivity_sgs_swc_coef_only_2$coef)
7.8 + (3*5.25)
15.75*7.8

sgs_swc_coef_no_outliers<- sensitivity_sgs_swc_coef_only_2 %>% dplyr::filter(coef < 23.55)
head(sgs_swc_coef_no_outliers)
hist(sgs_swc_coef_no_outliers$coef)

break_sensitivity_sgs_swc_no_outliers<-quantile(sgs_swc_coef_no_outliers$coef,seq(from=0.01, to = .99,by=0.01),na.rm=TRUE)
sensitivity_swc_sgs_raster_no_outliers<-rasterFromXYZ(sgs_swc_coef_no_outliers)

#plotting
colfunc <- colorRampPalette(c("yellow", "red"))
colfunc(10)
plot(rep(1,10),col=colfunc(25),pch=19,cex=3)
plot(sensitivity_swc_sgs_raster_no_outliers,col=colfunc(10))

#or this...
break_sensitivity_sgs_swc_no_outliers<-quantile(sgs_swc_coef_no_outliers$coef,seq(from=0.01, to = .99,by=0.01),na.rm=TRUE)
sensitivity_swc_sgs_raster_no_outliers<-rasterFromXYZ(sgs_swc_coef_no_outliers)

spplot(sensitivity_swc_sgs_raster,#scales = list(draw = TRUE),
       at=break_sensitivity_sgs_swc_no_outliers,
       asp=1,
       col.regions =
         rev(heat_hcl(length(break_sensitivity_sgs_swc_no_outliers)-1)),
       main="sgs soil moisture sensitivity") +
  latticeExtra::layer(sp.polygons(states_all_sites, lwd = 0.1))

#merge datasets
sgs_merged<-merge(sgs_swc_coef_no_outliers,sgs_swc,by=c('x','y'))
head(sgs_merged)
sgs_lm<-lm(npp.x~sgs_moisture,data=sgs_merged)
summary(sgs_lm)

#look at average residuals
#look all residuals
sgs_merged$resids <-residuals(sgs_lm)
View(sgs_merged)
#look all residuals
stratified_final$resids <-residuals(stratified_final_lm)
list.residuals.full[[i]] <- stratified_final

#look at mean residuals
mean.resids<-aggregate(resids~x+y,mean,data=sgs_merged)

#make rasters
residual.raster<-rasterFromXYZ(mean.resids)
residual.plot<-plot(residual.raster) #clear spatial autocorrelation
list.residual.rasters[[i]] <- data.frame(mean.resids)

#variogram of mean
library(gstat)
coordinates(mean.resids)= ~ x+y
TheVariogram_mean=variogram(resids~1, data=mean.resids)
variogram.plot<-plot(TheVariogram_mean,main='per-pixel mean')
list.variograms[[i]] <- variogram.plot
list.residuals.full[[i]] <- stratified_final
#look all residuals
stratified_final$resids <-residuals(stratified_final_lm)
list.residuals.full[[i]] <- stratified_final

#look at mean residuals
mean.resids<-aggregate(resids~x+y,mean,data=stratified_final)

#make rasters
residual.raster<-rasterFromXYZ(mean.resids)
#residual.plot<-plot(residual.raster)
list.residual.rasters[[i]] <- data.frame(mean.resids)

#variogram of mean
coordinates(mean.resids)= ~ x+y
TheVariogram_mean=variogram(resids~1, data=mean.resids)
variogram.plot<-plot(TheVariogram_mean,main='per-pixel mean')
list.variograms[[i]] <- variogram.plot
#look at mean residuals
mean.resids<-aggregate(resids~x+y,mean,data=sgs_final)

#make rasters
residual.raster<-rasterFromXYZ(mean.resids)
#residual.plot<-plot(residual.raster)
list.residual.rasters[[i]] <- data.frame(mean.resids)

#variogram of mean
coordinates(mean.resids)= ~ x+y
TheVariogram_mean=variogram(resids~1, data=mean.resids)
variogram.plot<-plot(TheVariogram_mean,main='per-pixel mean')
list.variograms[[i]] <- variogram.plot


#loop
list.coefficients.final.sgs.swc<-list()
list.variograms.sgs.swc<-list()
list.residuals.full.sgs.swc<-list()
list.residual.rasters.sgs.swc<-list()
head(sgs_swc)
head(stratified_final)
for(i in 1:1000)
{
  
  test.strat.semiarid_steppe<-stratified(semiarid_steppe_above_below, c("map"), 0.01)
  stratified_final<-merge(test.strat.semiarid_steppe, sgs_swc,by=c('x','y'))
  stratified_final_lm<-lm(npp.x~sgs_moisture
                          ,stratified_final)
 
  newcoef1 <- stratified_final_lm$coefficients 
  df<-data.frame(newcoef1)
  df$id = i
  list.coefficients.final.sgs.swc[[i]] <- data.frame(df)
  #look all residuals
  stratified_final$resids <-residuals(stratified_final_lm)
  list.residuals.full.sgs.swc[[i]] <- stratified_final
  
  #look at mean residuals
  mean.resids<-aggregate(resids~x+y,mean,data=stratified_final)
  
  #make rasters
  residual.raster<-rasterFromXYZ(mean.resids)
  #residual.plot<-plot(residual.raster)
  list.residual.rasters.sgs.swc[[i]] <- data.frame(mean.resids)
  
  #variogram of mean
  coordinates(mean.resids)= ~ x+y
  TheVariogram_mean=variogram(resids~1, data=mean.resids)
  variogram.plot<-plot(TheVariogram_mean,main='per-pixel mean')
  list.variograms.sgs.swc[[i]] <- variogram.plot
  
}

plot(list.variograms.sgs.swc[508])

summary(stratified_final_lm)
df.coefficients.sgs <- do.call("rbind", list.coefficients.final.sgs.swc)
head(df.coefficients.sgs)
df.coefficients.sgs.2 <- cbind(rownames(df.coefficients.sgs), data.frame(df.coefficients.sgs, row.names=NULL))

colnames(df.coefficients.sgs.2)  <- c("predictor","coefficient","run.id")

df.coefficients.sgs.2$predictor<-gsub('[[:digit:]]+', '', df.coefficients.sgs.2$predictor)
df.coefficients.sgs.2$predictor<-gsub(':', '_', df.coefficients.sgs.2$predictor)
df2.sgs<-reshape(df.coefficients.sgs.2, idvar = "run.id", timevar = "predictor", direction = "wide")
colnames(df2.sgs)
head(df2.sgs)
hist(df2$coefficient.sgs_moisture)


error.95 <-function(x) {
  n = length(x)
  se = sd(x)/sqrt(n)
  error <- qnorm(0.975)*se
  return(error)
}

error.95(df2$coefficient.sgs_moisture)
mean(df2$coefficient.sgs_moisture)
