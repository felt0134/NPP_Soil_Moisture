# northern mixed prairies

northern_mixed_swc<-subset(rangeland_npp_covariates_1,region.x=='northern_mixed')
head(northern_mixed_swc)
head(sensitivity_northern_mixed_swc)


#get rid of outleir pixels
summary(sensitivity_northern_mixed_swc_coef_only_2)
head(sensitivity_northern_mixed_swc_coef_only_2)
hist(sensitivity_northern_mixed_swc_coef_only_2$coef)
mean(sensitivity_northern_mixed_swc_coef_only_2$coef)
sd(sensitivity_northern_mixed_swc_coef_only_2$coef)
3.87 + (3*2.31)
3.87 - (3*2.31)
northern_mixed_swc_coef_no_outliers<- sensitivity_northern_mixed_swc_coef_only_2 %>% dplyr::filter(coef < 10.8, coef > -3.06)
summary(northern_mixed_swc_coef_no_outliers)
head(northern_mixed_swc_coef_no_outliers)
hist(northern_mixed_swc_coef_no_outliers$coef)

#merge datasets
northern_mixed_merged<-merge(northern_mixed_swc_coef_no_outliers,northern_mixed_swc,by=c('x','y'))
head(northern_mixed_merged)
northern_mixed_lm<-lm(npp.x~northern_mixed_moisture,data=northern_mixed_merged)
summary(northern_mixed_lm)



list.coefficients.final.northern_mixed.swc<-list()
list.variograms.northern_mixed.swc<-list()
list.residuals.full.northern_mixed.swc<-list()
list.residual.rasters.northern_mixed.swc<-list()
head(northern_mixed_swc)
head(stratified_final)

for(i in 1:1000)
{
  
  test.strat.northern_mixed<-stratified(northern_mixed_prairies_above_below, c("map"), 0.01)
  stratified_final<-merge(test.strat.northern_mixed, northern_mixed_merged,by=c('x','y'))
  stratified_final_lm<-lm(npp.x~northern_mixed_moisture 
                          ,stratified_final)
  
  newcoef1 <- stratified_final_lm$coefficients 
  df<-data.frame(newcoef1)
  df$id = i
  list.coefficients.final.northern_mixed.swc[[i]] <- data.frame(df)
  #look all residuals
  stratified_final$resids <-residuals(stratified_final_lm)
  list.residuals.full.northern_mixed.swc[[i]] <- stratified_final
  
  #look at mean residuals
  mean.resids<-aggregate(resids~x+y,mean,data=stratified_final)
  
  #make rasters
  residual.raster<-rasterFromXYZ(mean.resids)
  #residual.plot<-plot(residual.raster)
  list.residual.rasters.northern_mixed.swc[[i]] <- data.frame(mean.resids)
  
  #variogram of mean
  coordinates(mean.resids)= ~ x+y
  TheVariogram_mean=variogram(resids~1, data=mean.resids)
  variogram.plot<-plot(TheVariogram_mean,main='per-pixel mean')
  list.variograms.northern_mixed.swc[[i]] <- variogram.plot
  
}

list.variograms.northern_mixed.swc[700]

summary(stratified_final_lm)
df.coefficients.northern_mixed <- do.call("rbind", list.coefficients.final.northern_mixed.swc)
head(df.coefficients.northern_mixed)
df.coefficients.northern_mixed.2 <- cbind(rownames(df.coefficients.northern_mixed), data.frame(df.coefficients.northern_mixed, row.names=NULL))

colnames(df.coefficients.northern_mixed.2)  <- c("predictor","coefficient","run.id")

df.coefficients.northern_mixed.2$predictor<-gsub('[[:digit:]]+', '', df.coefficients.northern_mixed.2$predictor)
df.coefficients.northern_mixed.2$predictor<-gsub(':', '_', df.coefficients.northern_mixed.2$predictor)
df2.northern_mixed<-reshape(df.coefficients.northern_mixed.2, idvar = "run.id", timevar = "predictor", direction = "wide")
colnames(df2.northern_mixed)
head(df2.northern_mixed)
hist(df2.northern_mixed$coefficient.northern_mixed_moisture)


error.95 <-function(x) {
  n = length(x)
  se = sd(x)/sqrt(n)
  error <- qnorm(0.975)*se
  return(error)
}

error.95(df2.northern_mixed$coefficient.northern_mixed_moisture)
mean(df2.northern_mixed$coefficient.northern_mixed_moisture)