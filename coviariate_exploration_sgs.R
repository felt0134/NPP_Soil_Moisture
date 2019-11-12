#Assessing relationships between covariates

#compare r-squared for precip versus soil moisture
#add precip deviation
head(rangeland_npp_covariates_1)
sgs_precip<-subset(rangeland_npp_covariates_1,region.x=='semi_arid_steppe')
sgs_precip$mm.dev<-sgs_precip$mm.x - sgs_precip$mm.y
head(sgs_precip)

#look just at april-june soil moisture
sgs_april_june_swc_mean<-aggregate(april_june_swc~ x + y,mean,data=sgs_precip)
head(sgs_april_june_swc_mean)
merge_sgs_precip_a_j_mean <-merge(sgs_april_june_swc_mean,sgs_precip,by=c('x','y'))
head(merge_sgs_precip_a_j_mean)
merge_sgs_precip_a_j_mean$april_june_swc_deviation<- merge_sgs_precip_a_j_mean$april_june_swc.y - merge_sgs_precip_a_j_mean$april_june_swc.x 
head(merge_sgs_precip_a_j_mean)

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

#merge with april-september soil moisture
merge.swc.precip.r.square<-rbind(df.square.sgs.precip,df.rsquare.sgs.swc)
head(merge.swc.precip.r.square)
View(merge.swc.precip.r.square)

#april june and soil moisture global model
list.r.square.sgs.april.june.swc<-list()

for(i in 1:1000)
{
  
  test.strat.semiarid_steppe<-stratified(semiarid_steppe_above_below, c("map"), 0.01)
  stratified_final_sgs<-merge(test.strat.semiarid_steppe, merge_sgs_precip_a_j_mean,by=c('x','y'))
  stratified_final_sgs_lm<-lm(npp.x~april_june_swc_deviation*april_june_swc.x
                              ,stratified_final_sgs)
  
  
  
  #get r-squared
  #import r-squareds to list
  #veg
  r.square.sgs.april.june.swc<-summary(stratified_final_sgs_lm)$r.squared
  r.square.sgs.april.june.swc.df<-data.frame(r.square.sgs.april.june.swc)
  r.square.sgs.april.june.swc.df$id <- i
  list.r.square.sgs.april.june.swc[[i]] <- data.frame(r.square.sgs.april.june.swc.df)
  
  
  
}

#dataframe of r-squared to compare to precipitation
df.square.sgs.april.june.swc<- do.call("rbind",  list.r.square.sgs.april.june.swc)
head(df.square.sgs.april.june.swc)
colnames(df.square.sgs.april.june.swc)[colnames(df.square.sgs.april.june.swc)=="r.square.sgs.april.june.swc"] <- "r.square"
df.square.sgs.april.june.swc$model<-'april_june_swc'
head(df.square.sgs.april.june.swc)

#merge with april-september soil moisture
merge.swc.april.june.precip.r.square<-rbind(df.square.sgs.precip,df.square.sgs.april.june.swc)
head(merge.swc.april.june.precip.r.square)
View(merge.swc.precip.r.square)

#plot to compare
library(ggplot2)
ggplot(merge.swc.april.june.precip.r.square,aes(x=r.square,fill=model)) +
  geom_histogram(binwidth = 0.01,color='black',alpha=0.5) +
  scale_fill_manual(name = 'SGS Spatiotemporal model',values=c('precipitation'='red','april_june_swc'='lightblue'),
                    labels=c('precipitation'='Annual precipitation','april_june_swc'='April-June soil moisture')) +
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