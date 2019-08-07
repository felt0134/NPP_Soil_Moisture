#Cold deserts

list.coefficients.cold_deserts<-list()

for(i in 1:1000)
{
  test.strat.cold_deserts<-stratified(cold_deserts_above_below, c("map"), 0.01)
  stratified_final_cold_deserts<-merge(rangeland_npp_covariates_1, test.strat.cold_deserts,by=c('x','y'))
  
  #soil moisture
  soil.moisture.lm.cold.deserts <- lm(npp.x ~ april_june_swc*october_december_swc,data = stratified_final_cold_deserts) 
  
    newcoef1 <- soil.moisture.lm.cold.deserts$coefficients 
    df.cold_deserts<-data.frame(newcoef1)
    df.cold_deserts$id = i
    list.coefficients.cold_deserts[[i]] <- data.frame(df.cold_deserts)
  
}


summary(soil.moisture.lm.cold.deserts)

#soil moisture - changing list to dataframe
coef_soil_moisture_cold_deserts<- do.call("rbind", list.coefficients.cold_deserts)
head(coef_soil_moisture_cold_deserts)
coef_soil_moisture_cold_deserts.2 <- cbind(rownames(coef_soil_moisture_cold_deserts), data.frame(coef_soil_moisture_cold_deserts, row.names=NULL))
head(coef_soil_moisture_cold_deserts.2)
colnames(coef_soil_moisture_cold_deserts.2)  <- c("predictor","coefficient","run.id")
head(coef_soil_moisture_cold_deserts.2)
coef_soil_moisture_cold_deserts.2$predictor<-gsub('[[:digit:]]+', '', coef_soil_moisture_cold_deserts.2$predictor)
coef_soil_moisture_cold_deserts.2$predictor<-gsub(':', '_', coef_soil_moisture_cold_deserts.2$predictor)
coef_soil_moisture_cold_deserts.2$predictor<-gsub('-', '_', coef_soil_moisture_cold_deserts.2$predictor)

coef_soil_moisture_cold_deserts.3<-reshape(coef_soil_moisture_cold_deserts.2, idvar = "run.id", timevar = "predictor", direction = "wide")
head(coef_soil_moisture_cold_deserts.3)
summary(df2_mean)


hist(coef_soil_moisture_cold_deserts.3$coefficient.april_june_swc_october_december_swc)
