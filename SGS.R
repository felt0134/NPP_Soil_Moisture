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

summary(sgs_merged)

#get mean and deviation of soil moisture for each pixel
sgs_merged_2<-sgs_merged[c(1,2,4,5,7,20)]
head(sgs_merged_2)

#get mean moisture per grid cell
sgs_mean_moisture<-aggregate(sgs_moisture ~ x + y,mean,data=sgs_merged_2)
head(sgs_mean_moisture)

#combine them
sgs_merged_3<-merge(sgs_mean_moisture, sgs_merged_2,by=c('x','y'))
head(sgs_merged_3)

#add moisture deviation
sgs_merged_3$moisture_dev<-sgs_merged_3$sgs_moisture.y-sgs_merged_3$sgs_moisture.x
head(sgs_merged_3)

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

#stratified spatial block boostrapping

for(i in 1:1000)
{
  
  test.strat.semiarid_steppe<-stratified(semiarid_steppe_above_below, c("map"), 0.01)
  stratified_final_sgs<-merge(test.strat.semiarid_steppe, sgs_merged_3,by=c('x','y'))
  stratified_final_sgs_lm<-lm(npp.x~moisture_dev*sgs_moisture.x
                          ,stratified_final_sgs)
 
  newcoef1 <- stratified_final_sgs_lm$coefficients 
  df<-data.frame(newcoef1)
  df$id = i
  list.coefficients.final.sgs.swc[[i]] <- data.frame(df)
  
  #look all residuals
  stratified_final_sgs$resids <-residuals(stratified_final_sgs_lm)
  list.residuals.full.sgs.swc[[i]] <- stratified_final_sgs
  
  #look at mean residuals
  mean.resids<-aggregate(resids~x+y,mean,data=stratified_final_sgs)
  
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

summary(stratified_final_sgs_lm)
df.coefficients.sgs <- do.call("rbind", list.coefficients.final.sgs.swc)
head(df.coefficients.sgs)
df.coefficients.sgs.2 <- cbind(rownames(df.coefficients.sgs), data.frame(df.coefficients.sgs, row.names=NULL))

colnames(df.coefficients.sgs.2)  <- c("predictor","coefficient","run.id")

df.coefficients.sgs.2$predictor<-gsub('[[:digit:]]+', '', df.coefficients.sgs.2$predictor)
df.coefficients.sgs.2$predictor<-gsub(':', '_', df.coefficients.sgs.2$predictor)
df2.sgs<-reshape(df.coefficients.sgs.2, idvar = "run.id", timevar = "predictor", direction = "wide")
colnames(df2.sgs)
head(df2.sgs)
hist(df2.sgs$coefficient.sgs_moisture.x)

colnames(df2.sgs)[colnames(df2.sgs)=="coefficient.(Intercept)"] <- "intercept"

#95% CI
error.95 <-function(x) {
  n = length(x)
  se = sd(x)/sqrt(n)
  error <- qnorm(0.975)*se
  return(error)
}

#produce means and 95% CI for
error.95(df2$coefficient.sgs_moisture)
mean(df2$coefficient.sgs_moisture)

#produce a global model
beta_i_sgs <- mean(df2.sgs$intercept)
beta_s_sgs <- mean(df2.sgs$coefficient.sgs_moisture.x)
beta_t_sgs <- mean(df2.sgs$coefficient.moisture_dev)
beta_sxt_sgs <- mean(df2.sgs$coefficient.moisture_dev_sgs_moisture.x)

#calculate temporal slope at regional the 'average' soil moisture. Otherwise, the raw interpretation of the covariate is at zero
df2.sgs$temporal_sensitivity <-
  df2.sgs$coefficient.moisture_dev + df2.sgs$coefficient.moisture_dev_sgs_moisture.x*4.5

#a global model of NPP and soil moisture for SGS
sgs_fit$NPP = (beta_i_sgs + beta_s_sgs*sgs_fit$swc.mean) + 
  (beta_t_sgs + beta_sxt_sgs*sgs_fit$swc.mean)*sgs_fit$swc.dev

# toy data
head(sgs_merged_3)
hist(sgs_merged_3$sgs_moisture.y)
summary(sgs_merged_3)
summary(df2.sgs)
head(df2.sgs)
sgs_fit<-expand.grid(list(swc.dev=seq(-5,15,1),swc.mean=seq(0.5,20,1)))
sgs_fit$ID <- seq.int(nrow(sgs_fit))
head(sgs_fit)

#put into an lm by fitting to toy data
sgs_global<-lm(NPP~swc.dev*swc.mean,data=sgs_fit)
summary(sgs_global)

#use the predict function

#
plot(NPP~swc.mean,data=sgs_fit,xlab='Mean soil moisture',main='SGS NPP-soil moisture dynamics')

ggplot(sgs_fit,aes(swc.dev,NPP,color=as.factor(swc.mean))) +
  geom_line() +
  xlab('Soil moisture deviation') +
  ylab('Net primary productivity') +
  theme(
    axis.text.x = element_text(color='black',size=10), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=8),
    axis.title = element_text(color='black',size=14),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=15),
    legend.position = c('none'),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

for(i in 1:nrow(sedgwick_data)){
  #row=
  out = grow_res(seeds_res=1:200,Fec=sedgwick_data$lambda[i],alpha=sedgwick_data$alpha_intra[i],seedSurv=sedgwick_data$s[i],G_res=sedgwick_data$g[i],1)
  out$Species = sedgwick_data$species[i]
  list.out[[i]] <- data.frame(out)
}

npp.predict <-function(x) {
 
  NPP = (beta_i_sgs + beta_s_sgs*sgs_fit$swc.mean) + 
    (beta_t_sgs + beta_sxt_sgs*sgs_fit$swc.mean)*sgs_fit$swc.dev 
  
}