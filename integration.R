#combining site results

#mean and 95% ci for vwc slopes

site<-c('cold_deserts','california_annuals','northern_mixed','sgs')
slope<-c(5.79,4.27,6.2,7.19)
ci<-c(0.028,0.11,0.027,0.048)
id<-c(1,2,3,4)

slopes.ci.vwc<-data.frame(site,slope,ci,id)

#plot this

ggplot(slopes.ci.vwc,aes(as.factor(id),slope)) +
  geom_errorbar(aes(ymin=slope-ci, ymax=slope+ci),width=.1) +
  stat_summary(fun.y='mean',geom='point',size= 7,pch=21,color='black',fill='grey') +
  scale_x_discrete(labels=c( "1" = "Cold deserts","2" = "California annuals",
                            "3" = "Northern mixed prairies","4" = "Shortgrass steppe")) +
  xlab('') +
  ylab("Sensitivity to soil moisture (slope)") +
  ggtitle('coefficient means and 95% CI') +
  theme(
    axis.text.x = element_text(color='black',size=12), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=12),
    axis.title = element_text(color='black',size=20),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size=17),
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=15),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

#combine to make raster
rbind_cd_ca<-rbind(cold_deserts_swc_coef_no_outliers,cali_swc_coef_no_outliers)
head(rbind_cd_ca)
rbind_cd_ca_nmp<-rbind(rbind_cd_ca,northern_mixed_swc_coef_no_outliers)
head(rbind_cd_ca_nmp)
rbind_cd_ca_nmp_sgs<-rbind(rbind_cd_ca_nmp,sgs_swc_coef_no_outliers)
veg_swc_sensitivities<-rasterFromXYZ(rbind_cd_ca_nmp_sgs)
summary(rbind_cd_ca_nmp_sgs)
plot(veg_swc_sensitivities)

#graphs
#plotting
colfunc <- colorRampPalette(c("white", "red"))
cuts=c(0, 0.2,0.4,0.6,0.8,0.10,0.12) #set breaks
pal <- colorRampPalette(c("white","black"))
colfunc(20)
plot(rep(1,10),col=colfunc(25),pch=19,cex=3)
plot(veg_swc_sensitivities,breaks='cuts',col=colfunc(10))


break_veg_swc_sensitivities<-quantile(rbind_cd_ca_nmp_sgs$coef,seq(from=0.0, to = 1,by=0.01),na.rm=TRUE)

break_veg_swc_sensitivities<-seq(rbind_cd_ca_nmp_sgs$coef,from=0.0, to = 20,by=1,na.rm=TRUE)

spplot(veg_swc_sensitivities,#scales = list(draw = TRUE),
       at=break_veg_swc_sensitivities,
       asp=1,
       col.regions =
         rev(heat_hcl(length(break_veg_swc_sensitivities)-1)),
       main="NPP soil moisture sensitivity") +
  latticeExtra::layer(sp.polygons(states_all_sites, lwd = 0.1))