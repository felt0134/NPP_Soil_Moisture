#plotting maps of soil moisture sensitivities
library(colorspace)
library(latticeExtra)
library(sp)
library(ggplot2)
#shapefile referecne for state outlines
us<-getData("GADM", country='USA', level=1,download=TRUE)
states_all_sites <- us[us$NAME_1 %in% c('California','New Mexico','Arizona','Utah',
                                        'Arizona','Colorado','Washington','Wyoming',
                                        'Idaho','Oregon','Idaho','Montana','Texas',
                                        'North Dakota','South Dakota','Nebraska',
                                        'Oklahoma','Kansas'),]


#####cold deserts ######
#october december

cold_deserts_swc<-subset(rangeland_npp_covariates_1,region.x=='cold_deserts')

#NPP sensitivity to oct-dec soil moisture
sensitivity_october_december_swc_cold_deserts <- cold_deserts_swc %>% group_by(x, y) %>%
  dplyr::do(model = lm(npp.x~october_december_swc, data = .)) %>%
  dplyr::mutate(coef=coef(model)[2])

head(sensitivity_october_december_swc_cold_deserts)
sensitivity_october_december_swc_cold_deserts_coef_only<- sensitivity_october_december_swc_cold_deserts[ -c(3) ] #isolate coefficient so only slope is graphed
head(sensitivity_october_december_swc_cold_deserts_coef_only)
summary(sensitivity_october_december_swc_cold_deserts_coef_only)
hist(sensitivity_october_december_swc_cold_deserts_coef_only$coef)
sd(sensitivity_october_december_swc_cold_deserts_coef_only$coef)
sensitivity_october_december_swc_cold_deserts_raster<-rasterFromXYZ(sensitivity_october_december_swc_cold_deserts_coef_only)
plot(sensitivity_october_december_swc_cold_deserts_raster)
break_sensitivity_october_december_swc_cold_deserts<-quantile(sensitivity_october_december_swc_cold_deserts_coef_only$coef,seq(from=0.01, to = .99,by=0.01),na.rm=TRUE)

spplot(sensitivity_october_december_swc_cold_deserts_raster,#scales = list(draw = TRUE),
       at=break_sensitivity_october_december_swc_cold_deserts,
       asp=1,
       col.regions =
         rev(heat_hcl(length(break_sensitivity_october_december_swc_cold_deserts)-1)),
       main="cold deserts soil moisture sensitivity: October-December") +
  latticeExtra::layer(sp.polygons(states_all_sites, lwd = 0.1))

#April and June
#NPP sensitivity to april-june soil moisture
sensitivity_april_june_swc_cold_deserts <- cold_deserts_swc %>% group_by(x, y) %>%
  dplyr::do(model = lm(npp.x~april_june_swc, data = .)) %>%
  dplyr::mutate(coef=coef(model)[2])

head(sensitivity_april_june_swc_cold_deserts)
sensitivity_april_june_swc_cold_deserts_coef_only<- sensitivity_april_june_swc_cold_deserts[ -c(3) ] #isolate coefficient so only slope is graphed
head(sensitivity_april_june_swc_cold_deserts_coef_only)
summary(sensitivity_april_june_swc_cold_deserts_coef_only)
hist(sensitivity_april_june_swc_cold_deserts_coef_only$coef)
sd(sensitivity_april_june_swc_cold_deserts_coef_only$coef)
sensitivity_april_june_swc_cold_deserts_raster<-rasterFromXYZ(sensitivity_april_june_swc_cold_deserts_coef_only)
plot(sensitivity_april_june_swc_cold_deserts_raster)
break_sensitivity_april_june_swc_cold_deserts<-quantile(sensitivity_april_june_swc_cold_deserts_coef_only$coef,seq(from=0.01, to = .99,by=0.01),na.rm=TRUE)

spplot(sensitivity_april_june_swc_cold_deserts_raster,#scales = list(draw = TRUE),
       at=break_sensitivity_april_june_swc_cold_deserts,
       asp=1,
       col.regions =
         rev(heat_hcl(length(break_sensitivity_april_june_swc_cold_deserts)-1)),
       main="cold deserts soil moisture sensitivity: April-June") +
  latticeExtra::layer(sp.polygons(states_all_sites, lwd = 0.1))


cold_deserts_swc<-subset(rangeland_npp_covariates_1,region.x=='cold_deserts')
head(cold_deserts_swc)

sensitivity_cold_deserts_swc <- cold_deserts_swc %>% group_by(x, y) %>%
  dplyr::do(model = lm(npp.x~april_june_swc, data = .)) %>%
  dplyr::mutate(coef=coef(model)[2])


head(sensitivity_cold_deserts_swc)
sensitivity_cold_deserts_swc_coef_only<- sensitivity_cold_deserts_swc[ -c(3) ] #isolate coefficient so only slope is graphed
head(sensitivity_cold_deserts_swc_coef_only)
sensitivity_cold_deserts_swc_coef_only_2<-data.frame(sensitivity_cold_deserts_swc_coef_only)
mean(sensitivity_cold_deserts_swc_coef_only_2$coef)
sd(sensitivity_cold_deserts_swc_coef_only_2$coef)
3.61 + (3*3.88)
hist(sensitivity_cold_deserts_swc_coef_only_2$coef)
sensitivity_swc_cold_deserts_raster<-rasterFromXYZ(sensitivity_cold_deserts_swc_coef_only_2)

#graphs
plot(sensitivity_swc_cold_deserts_raster)

break_sensitivity_cold_deserts_swc<-quantile(sensitivity_cold_deserts_swc_coef_only_2$coef,seq(from=0.01, to = .99,by=0.01),na.rm=TRUE)

spplot(sensitivity_swc_cold_deserts_raster,#scales = list(draw = TRUE),
       at=break_sensitivity_cold_deserts_swc,
       asp=1,
       col.regions =
         rev(heat_hcl(length(break_sensitivity_cold_deserts_swc)-1)),
       main="cold deserts soil moisture sensitivity") +
  latticeExtra::layer(sp.polygons(states_all_sites, lwd = 0.1))

ggplot(sensitivity_cold_deserts_swc_coef_only_2,aes(x=coef)) +
  geom_histogram(binwidth = .4,color='black',fill='white') +
  geom_vline(xintercept=15.25,size=1,color='red') +
  xlab('Per-pixel april - june soil moisture sensitivity') +
  ylab("Count") +
  ggtitle('Cold deserts') +
  theme(
    axis.text.x = element_text(color='black',size=10), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=12),
    axis.title = element_text(color='black',size=15),
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

cold_deserts_swc_mean<-aggregate(april_june_swc~x + y,mean,data=cold_deserts_swc)
cold_deserts_npp_mean<-aggregate(npp.x~x + y,mean,data=cold_deserts_swc)
merge_cold_deserts_swc_npp_means<-merge(cold_deserts_swc_mean,cold_deserts_npp_mean,by=c('x','y'))
head(merge_cold_deserts_swc_npp_means)
plot(npp.x~april_june_swc,data=merge_cold_deserts_swc_npp_means,xlab='Mean soil moisture',ylab='Mean net primary productivity')


###shortgrass steppe#######
sgs_swc<-subset(rangeland_npp_covariates_1,region.x=='semi_arid_steppe')

#add together for full growing swc estimate
sgs_swc$sgs_moisture<-sgs_swc$april_june_swc + sgs_swc$july_september_swc
head(sgs_swc)

sensitivity_sgs_swc <- sgs_swc %>% group_by(x, y) %>%
  dplyr::do(model = lm(npp.x~sgs_moisture, data = .)) %>%
  dplyr::mutate(coef=coef(model)[2])

head(sensitivity_sgs_swc)
sensitivity_sgs_swc_coef_only<- sensitivity_sgs_swc[ -c(3) ] #isolate coefficient so only slope is graphed
head(sensitivity_sgs_swc_coef_only)
sensitivity_sgs_swc_coef_only_2<-data.frame(sensitivity_sgs_swc_coef_only)
mean(sensitivity_sgs_swc_coef_only_2$coef)
sd(sensitivity_sgs_swc_coef_only_2$coef)
7.8 + (3*5.25)
15.75*7.8
summary(sensitivity_sgs_swc_coef_only_2)
hist(sensitivity_sgs_swc_coef_only_2$coef)
sensitivity_swc_sgs_raster<-rasterFromXYZ(sensitivity_sgs_swc_coef_only_2)

#graphs
break_sensitivity_sgs_swc<-quantile(sensitivity_sgs_swc_coef_only_2$coef,seq(from=0.01, to = .99,by=0.01),na.rm=TRUE)

spplot(sensitivity_swc_sgs_raster,#scales = list(draw = TRUE),
       at=break_sensitivity_sgs_swc,
       asp=1,
       col.regions =
         rev(heat_hcl(length(break_sensitivity_sgs_swc)-1)),
       main="sgs soil moisture sensitivity") +
  latticeExtra::layer(sp.polygons(states_all_sites, lwd = 0.1))


#histogram plot
ggplot(sensitivity_sgs_swc_coef_only_2,aes(x=coef)) +
  geom_histogram(binwidth = .5,color='black',fill='white') +
  geom_vline(xintercept=23.55,size=1,color='red') +
  xlab('Per-pixel april - september soil moisture sensitivity') +
  ylab("Count") +
  ggtitle('Shortgrass steppe') +
  theme(
    axis.text.x = element_text(color='black',size=10), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=12),
    axis.title = element_text(color='black',size=15),
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

sgs_swc_mean<-aggregate(sgs_moisture~x + y,mean,data=sgs_swc)
sgs_npp_mean<-aggregate(npp.x~x + y,mean,data=sgs_swc)
merge_sgs_swc_npp_means<-merge(sgs_swc_mean,sgs_npp_mean,by=c('x','y'))
head(merge_sgs_swc_npp_means)
plot(npp.x~sgs_moisture,data=merge_sgs_swc_npp_means,xlab='Mean soil moisture',ylab='Mean net primary productivity')
