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

#soil moisture - changing list to dataframe
coef_soil_moisture<- do.call("rbind", soil.moisture.list)
head(coef_soil_moisture)
coef_soil_moisture.2 <- cbind(rownames(coef_soil_moisture), data.frame(coef_soil_moisture, row.names=NULL))
head(coef_soil_moisture.2)
colnames(coef_soil_moisture.2) <- c("id","region","jan_march_swc",'april_june_swc','july_september_swc','october_december_swc')
head(coef_soil_moisture.2)

data_long_soil_moisture <- gather(coef_soil_moisture.2, coefficient,value,-id,-region, factor_key=TRUE)
head(data_long_soil_moisture)



#look at sgs
sgs_soil_moisture<-subset(coef_soil_moisture.2,region=='semi_arid_steppe')
head(sgs_soil_moisture)

hist(sgs_soil_moisture$jan_march_swc)
hist(sgs_soil_moisture$april_june_swc)
hist(sgs_soil_moisture$july_september_swc)
hist(sgs_soil_moisture$october_december_swc)

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

#plot
summary(data_long_soil_moisture)
ggplot(data_long_soil_moisture,aes(x=value,fill=coefficient)) +
  geom_histogram(binwidth = 0.25,color='black') +
  geom_vline(xintercept=0,size=1.5,color='black') +
  facet_wrap(~region,nrow=5,scales='free_y',labeller = as_labeller(veg_names)) +
  xlab('Soil moisture impact on NPP') +
  scale_x_continuous(limit=c(-20,20)) +
  ylab("count") +
  theme(
    axis.text.x = element_text(color='black',size=10), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=12),
    axis.title = element_text(color='black',size=15),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size=13),
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=15),
    panel.background = element_rect(fill=NA),
    legend.position = c(0.2,0.15),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))
