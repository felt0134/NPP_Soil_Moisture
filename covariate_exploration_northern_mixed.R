#Assessing relationships between covariates

#compare r-squared for precip versus soil moisture
#add precip deviation
head(rangeland_npp_covariates_1)
northern_mixed_precip<-subset(rangeland_npp_covariates_1,region.x=='northern_mixed_prairies')
northern_mixed_precip$mm.dev<-northern_mixed_precip$mm.x - northern_mixed_precip$mm.y
head(northern_mixed_precip)

#look just at april-june soil moisture
northern_mixed_april_june_swc_mean<-aggregate(april_june_swc~ x + y,mean,data=northern_mixed_precip)
head(northern_mixed_april_june_swc_mean)
merge_northern_mixed_precip_a_j_mean <-merge(northern_mixed_april_june_swc_mean,northern_mixed_precip,by=c('x','y'))
head(merge_northern_mixed_precip_a_j_mean)
merge_northern_mixed_precip_a_j_mean$april_june_swc_deviation<- merge_northern_mixed_precip_a_j_mean$april_june_swc.y - merge_northern_mixed_precip_a_j_mean$april_june_swc.x 
head(merge_northern_mixed_precip_a_j_mean)

list.r.square.northern_mixed.precip<-list()

for(i in 1:1000)
{
  
  test.strat.northern_mixed<-stratified(northern_mixed_prairies_above_below, c("map"), 0.01)
  stratified_final_northern_mixed<-merge(test.strat.northern_mixed, northern_mixed_precip,by=c('x','y'))
  stratified_final_northern_mixed_lm<-lm(npp.x~mm.dev*mm.y
                                       ,stratified_final_northern_mixed)
  
  
  
  #get r-squared
  #import r-squareds to list
  #veg
  r.square.northern_mixed.precip<-summary(stratified_final_northern_mixed_lm)$r.squared
  r.square.northern_mixed.precip.df<-data.frame(r.square.northern_mixed.precip)
  r.square.northern_mixed.precip.df$id <- i
  list.r.square.northern_mixed.precip[[i]] <- data.frame(r.square.northern_mixed.precip.df)
  
  
  
}

#dataframe of r-squared to compare to precipitation
df.square.northern_mixed.precip <- do.call("rbind",  list.r.square.northern_mixed.precip)
colnames(df.square.northern_mixed.precip)[colnames(df.square.northern_mixed.precip)=="r.square.northern_mixed.precip"] <- "r.square"
df.square.northern_mixed.precip$model<-'precipitation'
head(df.square.northern_mixed.precip)

#merge with april-september soil moisture
merge.swc.precip.r.square<-rbind(df.square.northern_mixed.precip,df.rsquare.northern_mixed.swc)
head(merge.swc.precip.r.square)
View(merge.swc.precip.r.square)

#april june and soil moisture global model
list.r.square.northern_mixed.april.june.swc<-list()

for(i in 1:1000)
{
  
  test.strat.northern_mixed<-stratified(northern_mixed_prairies_above_below, c("map"), 0.01)
  stratified_final_northern_mixed<-merge(test.strat.northern_mixed, merge_northern_mixed_precip_a_j_mean,by=c('x','y'))
  stratified_final_northern_mixed_lm<-lm(npp.x~april_june_swc_deviation*april_june_swc.x
                                       ,stratified_final_northern_mixed)
  
  
  
  #get r-squared
  #import r-squareds to list
  #veg
  r.square.northern_mixed.april.june.swc<-summary(stratified_final_northern_mixed_lm)$r.squared
  r.square.northern_mixed.april.june.swc.df<-data.frame(r.square.northern_mixed.april.june.swc)
  r.square.northern_mixed.april.june.swc.df$id <- i
  list.r.square.northern_mixed.april.june.swc[[i]] <- data.frame(r.square.northern_mixed.april.june.swc.df)
  
  
  
}

#dataframe of r-squared to compare to precipitation
df.square.northern_mixed.april.june.swc<- do.call("rbind",  list.r.square.northern_mixed.april.june.swc)
head(df.square.northern_mixed.april.june.swc)
colnames(df.square.northern_mixed.april.june.swc)[colnames(df.square.northern_mixed.april.june.swc)=="r.square.northern_mixed.april.june.swc"] <- "r.square"
df.square.northern_mixed.april.june.swc$model<-'april_june_swc'
head(df.square.northern_mixed.april.june.swc)

#merge with april-september soil moisture
merge.swc.april.june.precip.r.square<-rbind(df.square.northern_mixed.precip,df.square.northern_mixed.april.june.swc)
head(merge.swc.april.june.precip.r.square)
View(merge.swc.precip.r.square)

#plot to compare
library(ggplot2)
ggplot(merge.swc.april.june.precip.r.square,aes(x=r.square,fill=model)) +
  geom_histogram(binwidth = 0.01,color='black',alpha=0.5) +
  scale_fill_manual(name = 'Northern mixed spatiotemporal model',values=c('precipitation'='red','april_june_swc'='lightblue'),
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
northern_mixed_precip_2 <- northern_mixed_precip[, c(5,8,11,13)]
head(northern_mixed_precip_2)
chart.Correlation(northern_mixed_precip_2, histogram=TRUE, pch=19)
