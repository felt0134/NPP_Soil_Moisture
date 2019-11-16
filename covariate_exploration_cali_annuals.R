#Assessing relationships between covariates

#compare r-squared for precip versus soil moisture
#add precip deviation
head(rangeland_npp_covariates_1)
cali_precip<-subset(rangeland_npp_covariates_1,region.x=='california_annuals')
cali_precip$mm.dev<-cali_precip$mm.x - cali_precip$mm.y
head(cali_precip)

#look just at jan-june soil moisture
cali_precip$cali_moisture <- cali_precip$april_june_swc + cali_precip$jan_march_swc
cali_swc_mean<-aggregate(cali_moisture ~ x + y,mean,data=cali_precip)
head(cali_swc_mean)
merge_cali_precip_cali_swc_mean<-merge(cali_swc_mean,cali_precip,by=c('x','y'))
head(merge_cali_precip_cali_swc_mean)
merge_cali_precip_cali_swc_mean$cali_swc_deviation <- merge_cali_precip_cali_swc_mean$cali_moisture.y- merge_cali_precip_cali_swc_mean$cali_moisture.x 
head(merge_cali_precip_cali_swc_mean)

list.r.square.cali.precip<-list()

for(i in 1:1000)
{
  
  test.strat.cali<-stratified(california_annuals_above_below, c("map"), 0.01)
  stratified_final_cali<-merge(test.strat.cali, cali_precip,by=c('x','y'))
  stratified_final_cali_lm<-lm(npp.x~mm.dev*mm.y
                                         ,stratified_final_cali)
  
  
  
  #get r-squared
  #import r-squareds to list
  #veg
  r.square.cali.precip<-summary(stratified_final_cali_lm)$r.squared
  r.square.cali.precip.df<-data.frame(r.square.cali.precip)
  r.square.cali.precip.df$id <- i
  list.r.square.cali.precip[[i]] <- data.frame(r.square.cali.precip.df)
  
  
  
}

#dataframe of r-squared to compare to precipitation
df.square.cali.precip <- do.call("rbind",  list.r.square.cali.precip)
colnames(df.square.cali.precip)[colnames(df.square.cali.precip)=="r.square.cali.precip"] <- "r.square"
df.square.cali.precip$model<-'precipitation'
head(df.square.cali.precip)


#january june and soil moisture global model
list.r.square.cali.swc<-list()

for(i in 1:1000)
{
  
  test.strat.cali<-stratified(california_annuals_above_below, c("map"), 0.01)
  stratified_final_cali<-merge(test.strat.cali, merge_cali_precip_cali_swc_mean,by=c('x','y'))
  stratified_final_cali_lm<-lm(npp.x~cali_swc_deviation*cali_moisture.x
                                         ,stratified_final_cali)
  
  
  
  #get r-squared
  #import r-squareds to list
  #veg
  r.square.cali.swc<-summary(stratified_final_cali_lm)$r.squared
  r.square.cali.swc.df<-data.frame(r.square.cali.swc)
  r.square.cali.swc.df$id <- i
  list.r.square.cali.swc[[i]] <- data.frame(r.square.cali.swc.df)
  
  
  
}

#dataframe of r-squared to compare to precipitation
df.square.cali.swc<- do.call("rbind",  list.r.square.cali.swc)
head(df.square.cali.swc)
colnames(df.square.cali.swc)[colnames(df.square.cali.swc)=="r.square.cali.swc"] <- "r.square"
df.square.cali.swc$model<-'cali_swc'
head(df.square.cali.swc)

#merge with jan-june soil moisture
merge.swc.precip.r.square_cali<-rbind(df.square.cali.precip,df.square.cali.swc)
head(merge.swc.precip.r.square_cali)
View(merge.swc.precip.r.square_cali)

#plot to compare
library(ggplot2)
ggplot(merge.swc.precip.r.square_cali,aes(x=r.square,fill=model)) +
  geom_histogram(binwidth = 0.01,color='black',alpha=0.5) +
  scale_fill_manual(name = 'California model',values=c('precipitation'='red','cali_swc'='lightblue'),
                    labels=c('precipitation'='Annual precipitation','cali_swc'='Jan-June soil moisture')) +
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
head(merge_cali_precip_cali_swc_mean)
merge_cali_precip_cali_swc_mean$hot_dry_jan_june <-  merge_cali_precip_cali_swc_mean$hot_dry_jan_march + merge_cali_precip_cali_swc_mean$hot_dry_april_june
library("PerformanceAnalytics")
cali_precip_2 <- merge_cali_precip_cali_swc_mean[, c(6,12,22,23)]
head(cali_precip_2)
chart.Correlation(cali_precip_2, histogram=TRUE, pch=19)
