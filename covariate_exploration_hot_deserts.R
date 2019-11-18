#Assessing relationships between covariates

#compare r-squared for precip versus soil moisture
#add precip deviation
head(rangeland_npp_covariates_1)
hot_deserts_precip<-subset(rangeland_npp_covariates_1,region.x=='hot_deserts')
hot_deserts_precip$mm.dev<-hot_deserts_precip$mm.x - hot_deserts_precip$mm.y
head(hot_deserts_precip)

#look just at jan-june soil moisture
hot_deserts_precip$hot_deserts_moisture <- hot_deserts_precip$april_june_swc + hot_deserts_precip$july_september_swc
hot_deserts_swc_mean<-aggregate(hot_deserts_moisture ~ x + y,mean,data=hot_deserts_precip)
head(hot_deserts_swc_mean)
merge_hot_deserts_precip_swc_mean<-merge(hot_deserts_swc_mean,hot_deserts_precip,by=c('x','y'))
head(merge_hot_deserts_precip_swc_mean)
merge_hot_deserts_precip_swc_mean$hot_deserts_swc_deviation <- merge_hot_deserts_precip_swc_mean$hot_deserts_moisture.y- merge_hot_deserts_precip_swc_mean$hot_deserts_moisture.x 
head(merge_hot_deserts_precip_swc_mean)

list.r.square.hot_deserts.precip<-list()

for(i in 1:1000)
{
  
  test.strat.hot_deserts<-stratified(hot_deserts_above_below, c("map"), 0.01)
  stratified_final_hot_deserts<-merge(test.strat.hot_deserts, hot_deserts_precip,by=c('x','y'))
  stratified_final_hot_deserts_lm<-lm(npp.x~mm.dev*mm.y
                               ,stratified_final_hot_deserts)
  
  
  
  #get r-squared
  #import r-squareds to list
  #veg
  r.square.hot_deserts.precip<-summary(stratified_final_hot_deserts_lm)$r.squared
  r.square.hot_deserts.precip.df<-data.frame(r.square.hot_deserts.precip)
  r.square.hot_deserts.precip.df$id <- i
  list.r.square.hot_deserts.precip[[i]] <- data.frame(r.square.hot_deserts.precip.df)
  
  
  
}

#dataframe of r-squared to compare to precipitation
df.square.hot_deserts.precip <- do.call("rbind",  list.r.square.hot_deserts.precip)
colnames(df.square.hot_deserts.precip)[colnames(df.square.hot_deserts.precip)=="r.square.hot_deserts.precip"] <- "r.square"
df.square.hot_deserts.precip$model<-'precipitation'
head(df.square.hot_deserts.precip)


#january june and soil moisture global model
list.r.square.hot_deserts.swc<-list()

for(i in 1:1000)
{
  
  test.strat.hot_deserts<-stratified(hot_deserts_above_below, c("map"), 0.01)
  stratified_final_hot_deserts<-merge(test.strat.hot_deserts, merge_hot_deserts_precip_swc_mean,by=c('x','y'))
  stratified_final_hot_deserts_lm<-lm(npp.x~hot_deserts_swc_deviation*hot_deserts_moisture.x
                               ,stratified_final_hot_deserts)
  
  
  
  #get r-squared
  #import r-squareds to list
  #veg
  r.square.hot_deserts.swc<-summary(stratified_final_hot_deserts_lm)$r.squared
  r.square.hot_deserts.swc.df<-data.frame(r.square.hot_deserts.swc)
  r.square.hot_deserts.swc.df$id <- i
  list.r.square.hot_deserts.swc[[i]] <- data.frame(r.square.hot_deserts.swc.df)
  
  
  
}

#dataframe of r-squared to compare to precipitation
df.square.hot_deserts.swc<- do.call("rbind",  list.r.square.hot_deserts.swc)
head(df.square.hot_deserts.swc)
colnames(df.square.hot_deserts.swc)[colnames(df.square.hot_deserts.swc)=="r.square.hot_deserts.swc"] <- "r.square"
df.square.hot_deserts.swc$model<-'hot_deserts_swc'
head(df.square.hot_deserts.swc)

#merge with jan-june soil moisture
merge.swc.precip.r.square_hot_deserts<-rbind(df.square.hot_deserts.precip,df.square.hot_deserts.swc)
head(merge.swc.precip.r.square)
View(merge.swc.precip.r.square)

#plot to compare
library(ggplot2)
ggplot(merge.swc.precip.r.square_hot_deserts,aes(x=r.square,fill=model)) +
  geom_histogram(binwidth = 0.01,color='black',alpha=0.5) +
  scale_fill_manual(name = 'Hot deserts model',values=c('precipitation'='red','soil_moisture'='lightblue'),
                    labels=c('precipitation'='Annual precipitation','soil_moisture'='April-Sep soil moisture')) +
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
head(merge_hot_deserts_precip_hot_deserts_swc_mean)
merge_hot_deserts_precip_hot_deserts_swc_mean$hot_dry_april_sep <-  merge_hot_deserts_precip_hot_deserts_swc_mean$hot_dry_april_june + merge_hot_deserts_precip_hot_deserts_swc_mean$hot_dry_july_september
library("PerformanceAnalytics")
hot_deserts_precip_2 <- merge_hot_deserts_precip_hot_deserts_swc_mean[, c(6,12,21,23)]
head(hot_deserts_precip_2)
chart.Correlation(hot_deserts_precip_2, histogram=TRUE, pch=19)
