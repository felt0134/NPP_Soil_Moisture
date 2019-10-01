# cold deserts

cold_deserts_swc<-subset(rangeland_npp_covariates_1,region.x=='cold_deserts')
head(cold_deserts_swc)
head(sensitivity_cold_deserts_swc)

list.coefficients.final.cold_deserts.swc<-list()
list.variograms.cold_deserts.swc<-list()
list.residuals.full.cold_deserts.swc<-list()
list.residual.rasters.cold_deserts.swc<-list()
head(cold_deserts_swc)
head(stratified_final)
for(i in 1:1000)
{
  
  test.strat.cold_deserts<-stratified(cold_deserts_above_below, c("map"), 0.01)
  stratified_final<-merge(test.strat.cold_deserts, cold_deserts_swc,by=c('x','y'))
  stratified_final_lm<-lm(npp.x~april_june_swc 
                          ,stratified_final)
  
  newcoef1 <- stratified_final_lm$coefficients 
  df<-data.frame(newcoef1)
  df$id = i
  list.coefficients.final.cold_deserts.swc[[i]] <- data.frame(df)
  #look all residuals
  stratified_final$resids <-residuals(stratified_final_lm)
  list.residuals.full.cold_deserts.swc[[i]] <- stratified_final
  
  #look at mean residuals
  mean.resids<-aggregate(resids~x+y,mean,data=stratified_final)
  
  #make rasters
  residual.raster<-rasterFromXYZ(mean.resids)
  #residual.plot<-plot(residual.raster)
  list.residual.rasters.cold_deserts.swc[[i]] <- data.frame(mean.resids)
  
  #variogram of mean
  coordinates(mean.resids)= ~ x+y
  TheVariogram_mean=variogram(resids~1, data=mean.resids)
  variogram.plot<-plot(TheVariogram_mean,main='per-pixel mean')
  list.variograms.cold_deserts.swc[[i]] <- variogram.plot
  
}

list.variograms.cold_deserts.swc[700]

summary(stratified_final_lm)
df.coefficients.cold_deserts <- do.call("rbind", list.coefficients.final.cold_deserts.swc)
head(df.coefficients.cold_deserts)
df.coefficients.cold_deserts.2 <- cbind(rownames(df.coefficients.cold_deserts), data.frame(df.coefficients.cold_deserts, row.names=NULL))

colnames(df.coefficients.cold_deserts.2)  <- c("predictor","coefficient","run.id")

df.coefficients.cold_deserts.2$predictor<-gsub('[[:digit:]]+', '', df.coefficients.cold_deserts.2$predictor)
df.coefficients.cold_deserts.2$predictor<-gsub(':', '_', df.coefficients.cold_deserts.2$predictor)
df2.cold_deserts<-reshape(df.coefficients.cold_deserts.2, idvar = "run.id", timevar = "predictor", direction = "wide")
colnames(df2.cold_deserts)
head(df2.cold_deserts)
hist(df2.cold_deserts$coefficient.april_june_swc)


error.95 <-function(x) {
  n = length(x)
  se = sd(x)/sqrt(n)
  error <- qnorm(0.975)*se
  return(error)
}

error.95(df2.cold_deserts$coefficient.april_june_swc)
mean(df2.cold_deserts$coefficient.april_june_swc)