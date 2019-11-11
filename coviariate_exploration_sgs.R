#Assessing relationships between covariates

#compare r-squared for precip versus soil moisture
#add precip deviation
head(rangeland_npp_covariates_1)
sgs_precip<-subset(rangeland_npp_covariates_1,region.x=='semi_arid_steppe')
sgs_precip$mm.dev<-sgs_precip$mm.x - sgs_precip$mm.y
head(sgs_precip)


list.r.square.sgs.precip<-list()
  
for(i in 1:1000)
{
  
  test.strat.semiarid_steppe<-stratified(semiarid_steppe_above_below, c("map"), 0.01)
  stratified_final_sgs<-merge(test.strat.semiarid_steppe, sgs_precip,by=c('x','y'))
  stratified_final_sgs_lm<-lm(npp.x~mm.dev*mm.y
                              ,stratified_final_sgs)
  
  
  
  #get r-squared
  #import r-squareds to list
  #veg
  r.square.sgs.precip<-summary(stratified_final_sgs_lm)$r.squared
  r.square.sgs.precip.df<-data.frame(r.square.sgs.precip)
  r.square.sgs.precip.df$id <- i
  list.r.square.sgs.precip[[i]] <- data.frame(r.square.sgs.precip.df)
  

  
}

#dataframe of r-squared to compare to precipitation
df.square.sgs.precip <- do.call("rbind",  list.r.square.sgs.precip)
colnames(df.square.sgs.precip)[colnames(df.square.sgs.precip)=="r.square.sgs.precip"] <- "r.square"
df.square.sgs.precip$model<-'precipitation'
head(df.square.sgs.precip)

merge.swc.precip.r.square<-rbind(df.square.sgs.precip,df.rsquare.sgs.swc)
head(merge.swc.precip.r.square)
View(merge.swc.precip.r.square)

#plot to compare
ggplot(merge.swc.precip.r.square,aes(x=r.square,fill=model)) +
  geom_histogram(binwidth = 0.01,color='black') +
  scale_fill_manual(name = 'SGS Spatiotemporal model',values=c('precipitation'='red','soil_moisture'='lightblue'),
                    labels=c('precipitation'='Annual precipitation','soil_moisture'='Growing season soil moisture')) +
  xlab('Model r-square') +
  ylab('') +
  theme(
    axis.text.x = element_text(color='black',size=20), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=20),
    axis.title = element_text(color='black',size=25),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    legend.title = element_text(size=12),
    legend.text = element_text(size=10),
    legend.position = c('top'),
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=15),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

#look at correlations
head(rangeland_npp_covariates_1)
library("PerformanceAnalytics")
sgs_precip_2 <- sgs_precip[, c(5,8,11,13)]
chart.Correlation(sgs_precip_2, histogram=TRUE, pch=19)