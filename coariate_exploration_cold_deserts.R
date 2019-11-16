#Assessing relationships between covariates

#compare r-squared for precip versus soil moisture
#add precip deviation
head(rangeland_npp_covariates_1)
cold_deserts_precip<-subset(rangeland_npp_covariates_1,region.x=='cold_deserts')
cold_deserts_precip$mm.dev<-cold_deserts_precip$mm.x - cold_deserts_precip$mm.y
head(cold_deserts_precip)

#look just at april-june soil moisture
cold_deserts_april_june_swc_mean<-aggregate(april_june_swc~ x + y,mean,data=cold_deserts_precip)
head(cold_deserts_april_june_swc_mean)
merge_cold_deserts_precip_a_j_mean <-merge(cold_deserts_april_june_swc_mean,cold_deserts_precip,by=c('x','y'))
head(merge_cold_deserts_precip_a_j_mean)
merge_cold_deserts_precip_a_j_mean$april_june_swc_deviation<- merge_cold_deserts_precip_a_j_mean$april_june_swc.y - merge_cold_deserts_precip_a_j_mean$april_june_swc.x 
head(merge_cold_deserts_precip_a_j_mean)

list.r.square.cold_deserts.precip<-list()

for(i in 1:1000)
{
  
  test.strat.cold_deserts<-stratified(cold_deserts_above_below, c("map"), 0.01)
  stratified_final_cold_deserts<-merge(test.strat.cold_deserts, cold_deserts_precip,by=c('x','y'))
  stratified_final_cold_deserts_lm<-lm(npp.x~mm.dev*mm.y
                              ,stratified_final_cold_deserts)
  
  
  
  #get r-squared
  #import r-squareds to list
  #veg
  r.square.cold_deserts.precip<-summary(stratified_final_cold_deserts_lm)$r.squared
  r.square.cold_deserts.precip.df<-data.frame(r.square.cold_deserts.precip)
  r.square.cold_deserts.precip.df$id <- i
  list.r.square.cold_deserts.precip[[i]] <- data.frame(r.square.cold_deserts.precip.df)
  
  
  
}

#dataframe of r-squared to compare to precipitation
df.square.cold_deserts.precip <- do.call("rbind",  list.r.square.cold_deserts.precip)
colnames(df.square.cold_deserts.precip)[colnames(df.square.cold_deserts.precip)=="r.square.cold_deserts.precip"] <- "r.square"
df.square.cold_deserts.precip$model<-'precipitation'
head(df.square.cold_deserts.precip)

#merge with april-september soil moisture
merge.swc.precip.r.square<-rbind(df.square.cold_deserts.precip,df.rsquare.cold_deserts.swc)
head(merge.swc.precip.r.square)
View(merge.swc.precip.r.square)

#april june and soil moisture global model
list.r.square.cold_deserts.april.june.swc<-list()

for(i in 1:1000)
{
  
  test.strat.cold_deserts<-stratified(cold_deserts_above_below, c("map"), 0.01)
  stratified_final_cold_deserts<-merge(test.strat.cold_deserts, merge_cold_deserts_precip_a_j_mean,by=c('x','y'))
  stratified_final_cold_deserts_lm<-lm(npp.x~april_june_swc_deviation*april_june_swc.x
                              ,stratified_final_cold_deserts)
  
  
  
  #get r-squared
  #import r-squareds to list
  #veg
  r.square.cold_deserts.april.june.swc<-summary(stratified_final_cold_deserts_lm)$r.squared
  r.square.cold_deserts.april.june.swc.df<-data.frame(r.square.cold_deserts.april.june.swc)
  r.square.cold_deserts.april.june.swc.df$id <- i
  list.r.square.cold_deserts.april.june.swc[[i]] <- data.frame(r.square.cold_deserts.april.june.swc.df)
  
  
  
}

#dataframe of r-squared to compare to precipitation
df.square.cold_deserts.april.june.swc<- do.call("rbind",  list.r.square.cold_deserts.april.june.swc)
head(df.square.cold_deserts.april.june.swc)
colnames(df.square.cold_deserts.april.june.swc)[colnames(df.square.cold_deserts.april.june.swc)=="r.square.cold_deserts.april.june.swc"] <- "r.square"
df.square.cold_deserts.april.june.swc$model<-'april_june_swc'
head(df.square.cold_deserts.april.june.swc)

#merge with april-september soil moisture
merge.swc.april.june.precip.r.square<-rbind(df.square.cold_deserts.precip,df.square.cold_deserts.april.june.swc)
head(merge.swc.april.june.precip.r.square)
View(merge.swc.precip.r.square)

#plot to compare
library(ggplot2)
ggplot(merge.swc.april.june.precip.r.square,aes(x=r.square,fill=model)) +
  geom_histogram(binwidth = 0.01,color='black',alpha=0.5) +
  scale_fill_manual(name = 'Cold deserts spatiotemporal model',values=c('precipitation'='red','april_june_swc'='lightblue'),
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
cold_deserts_precip_2 <- cold_deserts_precip[, c(5,8,11,13)]
head(cold_deserts_precip_2)
chart.Correlation(cold_deserts_precip_2, histogram=TRUE, pch=19)
