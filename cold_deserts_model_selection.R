#Cold deserts

list.coefficients.cold.deserts<-list()

for(i in 1:1000)
{
  test.strat.cold_deserts<-stratified(cold_deserts_above_below, c("map"), 0.01)
  stratified_final_cold_deserts<-merge(rangeland_npp_covariates_1, test.strat.cold_deserts,by=c('x','y'))
  
  #soil moisture
  soil.moisture.lm.cold.deserts <- lm(npp.x ~ april_june_swc*october_december_swc,data = stratified_final_cold_deserts) %>%
  
    newcoef1 <- soil.moisture.lm.cold.deserts$coefficients 
    df.cold_deserts<-data.frame(newcoef1)
    df.cold_deserts$id = i
    list.coefficients.cold_deserts[[i]] <- data.frame(df.cold_deserts)
  
}

summary(soil.moisture.lm.cold.deserts)
