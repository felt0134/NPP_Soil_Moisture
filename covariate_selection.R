#assessing different covariates for soil moisture and hot-dry metric

soil.moisture.list<-list()
hot.dry.list<-list()

for(i in 1:1000)
{
  test.strat.northern_mixed<-stratified(northern_mixed_prairies_above_below, c("map"), 0.01)
  test.strat.cold_deserts<-stratified(cold_deserts_above_below, c("map"), 0.01)
  test.strat.california_annuals<-stratified(california_annuals_above_below, c("map"), 0.05)
  test.strat.semiarid_steppe<-stratified(semiarid_steppe_above_below, c("map"), 0.02)
  test.strat.hot_deserts<-stratified(hot_deserts_above_below, c("map"), 0.02)
  test.strat<-rbind(test.strat.northern_mixed, test.strat.cold_deserts, test.strat.california_annuals, 
                    test.strat.semiarid_steppe, test.strat.hot_deserts)
  stratified_final_mean<-merge(rangeland_npp_covariates_1, test.strat,by=c('x','y'))
  head(stratified_final_mean)
  #soil moisture
  df.soil.moisture<- stratified_final_mean %>% group_by(region) %>%
    dplyr::do(soil.moisture.lm = lm(npp.x ~  jan_march_swc + april_june_swc + july_september_swc + october_december_swc,data = .)) %>%
    dplyr::mutate(jan_march_swc=coef(soil.moisture.lm)[2]) %>%
    dplyr::mutate(april_june_swc =coef(soil.moisture.lm)[3]) %>%
    dplyr::mutate(july_september_swc =coef(soil.moisture.lm)[4]) %>%
    dplyr::mutate(october_december_swc =coef(soil.moisture.lm)[5]) 
    df.soil.moisture_2<-df.soil.moisture[-2]
  soil.moisture.list[[i]]<-data.frame(df.soil.moisture_2)
  
  #hot_dry
  df.hot.dry<- stratified_final_mean %>% group_by(region) %>%
    dplyr::do(hot.dry.lm = lm(npp.x ~  hot_dry_jan_march  + hot_dry_april_june  + hot_dry_july_september + hot_dry_oct_dec, data = .)) %>%
    dplyr::mutate(hot_dry_jan_march=coef(hot.dry.lm)[2]) %>%
    dplyr::mutate(hot_dry_april_june=coef(hot.dry.lm)[3]) %>%
    dplyr::mutate(hot_dry_july_september =coef(hot.dry.lm)[4]) %>%
    dplyr::mutate(hot_dry_oct_dec =coef(hot.dry.lm)[5]) 
    df.hot.dry_2<-df.hot.dry[-2]
  hot.dry.list[[i]]<-data.frame(df.hot.dry_2)
  
}

hot.dry.lm = lm(npp.x ~  hot_dry_jan_march  + hot_dry_april_june  + hot_dry_july_september + hot_dry_oct_dec, data = stratified_final_mean)
summary(hot.dry.lm)
coef_nolag <- do.call("rbind", list.aic.nolag)
head(coef_nolag)
coef_nolag.2 <- cbind(rownames(coef_nolag), data.frame(coef_nolag, row.names=NULL))
head(coef_nolag.2)
colnames(coef_nolag.2) <- c("id","region","AIC.nolag")
head(coef_nolag.2)

coef_lag <- do.call("rbind", list.aic.lag)
head(coef_lag)
coef_lag.2 <- cbind(rownames(coef_lag), data.frame(coef_lag, row.names=NULL))
head(coef_lag.2)
colnames(coef_lag.2) <- c("id","region","AIC.lag",'mm.coef','prev.year.coef','interaction')
head(coef_lag.2)

merge.lags<-merge(coef_nolag.2,coef_lag.2,by=c('id','region'))
head(merge.lags)

merge.lags$aic.diff <- merge.lags$AIC.lag - merge.lags$AIC.nolag 
hist(merge.lags$prev.year.coef)
