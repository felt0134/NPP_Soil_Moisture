#produce sgs dataframe for regularization
#want: mean ppt, ppt deviation, tranpiration deviation
head(rangeland_npp_covariates_1)
sgs<-subset(rangeland_npp_covariates_1,region.x=='semi_arid_steppe')

#get precip deviation
sgs$mm.dev<-sgs$mm.x - sgs$mm.y

#get mean transp
sgs_transp_mean<-aggregate(day_of_50_total_transp~ x + y,mean,data=sgs)
head(sgs_transp_mean)

#merge mean transp with full dataframe
sgs_2<-merge(sgs,sgs_transp_mean,by=c('x','y'))
sgs_2$transp.dev<-sgs_2$day_of_50_total_transp.x - sgs_2$day_of_50_total_transp.y
head(sgs_2)
sgs_3<-sgs_2[c(1,2,3,4,6,18,19,20,21)]
head(sgs_3)
plot(npp.x~transp.dev,data=sgs_3)

saveRDS(sgs_3, file = "sgs_covariates_for_regularization_1.rds")
