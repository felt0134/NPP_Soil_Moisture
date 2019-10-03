# california annuals

cali_swc<-subset(rangeland_npp_covariates_1,region.x=='cali')
head(cali_swc)
head(sensitivity_cali_swc)


#get rid of outleir pixels
summary(sensitivity_cali_swc_coef_only_2)
head(sensitivity_cali_swc_coef_only_2)
hist(sensitivity_cali_swc_coef_only_2$coef)
mean(sensitivity_cali_swc_coef_only_2$coef)
sd(sensitivity_cali_swc_coef_only_2$coef)
2.44 + (3*1.93)
2.44 - (3*1.93)
cali_swc_coef_no_outliers<- sensitivity_cali_swc_coef_only_2 %>% dplyr::filter(coef < 8.23, coef > -3.35)
summary(cali_swc_coef_no_outliers)
head(cali_swc_coef_no_outliers)
hist(cali_swc_coef_no_outliers$coef)

#merge datasets
cali_merged<-merge(cali_swc_coef_no_outliers,cali_swc,by=c('x','y'))
head(cali_merged)
cali_lm<-lm(npp.x~cali_moisture,data=cali_merged)
summary(cali_lm)



list.coefficients.final.cali.swc<-list()
list.variograms.cali.swc<-list()
list.residuals.full.cali.swc<-list()
list.residual.rasters.cali.swc<-list()
head(cali_swc)
head(stratified_final)

for(i in 1:1000)
{
  
  test.strat.cali<-stratified(california_annuals_above_below, c("map"), 0.01)
  stratified_final<-merge(test.strat.cali, cali_merged,by=c('x','y'))
  stratified_final_lm<-lm(npp.x~cali_moisture 
                          ,stratified_final)
  
  newcoef1 <- stratified_final_lm$coefficients 
  df<-data.frame(newcoef1)
  df$id = i
  list.coefficients.final.cali.swc[[i]] <- data.frame(df)
  #look all residuals
  stratified_final$resids <-residuals(stratified_final_lm)
  list.residuals.full.cali.swc[[i]] <- stratified_final
  
  #look at mean residuals
  mean.resids<-aggregate(resids~x+y,mean,data=stratified_final)
  
  #make rasters
  residual.raster<-rasterFromXYZ(mean.resids)
  #residual.plot<-plot(residual.raster)
  list.residual.rasters.cali.swc[[i]] <- data.frame(mean.resids)
  
  #variogram of mean
  coordinates(mean.resids)= ~ x+y
  TheVariogram_mean=variogram(resids~1, data=mean.resids)
  variogram.plot<-plot(TheVariogram_mean,main='per-pixel mean')
  list.variograms.cali.swc[[i]] <- variogram.plot
  
}

list.variograms.cali.swc[700]

summary(stratified_final_lm)
df.coefficients.cali <- do.call("rbind", list.coefficients.final.cali.swc)
head(df.coefficients.cali)
df.coefficients.cali.2 <- cbind(rownames(df.coefficients.cali), data.frame(df.coefficients.cali, row.names=NULL))

colnames(df.coefficients.cali.2)  <- c("predictor","coefficient","run.id")

df.coefficients.cali.2$predictor<-gsub('[[:digit:]]+', '', df.coefficients.cali.2$predictor)
df.coefficients.cali.2$predictor<-gsub(':', '_', df.coefficients.cali.2$predictor)
df2.cali<-reshape(df.coefficients.cali.2, idvar = "run.id", timevar = "predictor", direction = "wide")
colnames(df2.cali)
head(df2.cali)
hist(df2.cali$coefficient.cali_moisture)


error.95 <-function(x) {
  n = length(x)
  se = sd(x)/sqrt(n)
  error <- qnorm(0.975)*se
  return(error)
}

error.95(df2.cali$coefficient.cali_moisture)
mean(df2.cali$coefficient.cali_moisture)