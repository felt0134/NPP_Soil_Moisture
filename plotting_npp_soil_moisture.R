#plotting maps of soil moisture sensitivities
library(colorspace)
library(latticeExtra)
library(sp)

#shapefile referecne for state outlines
us<-getData("GADM", country='USA', level=1,download=TRUE)
states_all_sites <- us[us$NAME_1 %in% c('California','New Mexico','Arizona','Utah',
                                        'Arizona','Colorado','Washington','Wyoming',
                                        'Idaho','Oregon','Idaho','Montana','Texas',
                                        'North Dakota','South Dakota','Nebraska',
                                        'Oklahoma','Kansas'),]

plot(states_all_sites)


#NPP sensitivity to jan-march soil moisture
sensitivity_jan_march_swc_cold_deserts <- rangeland_npp_covariates_1 %>% group_by(x, y) %>%
  dplyr::do(model = lm(npp.x~jan_march_swc, data = .)) %>%
  dplyr::mutate(coef=coef(model)[2])

head(sensitivity_jan_march_swc_conus)
sensitivity_jan_march_swc_conus_coef_only<- sensitivity_jan_march_swc_conus[ -c(3) ] #isolate coefficient so only slope is graphed
head(sensitivity_jan_march_swc_conus_coef_only)
summary(sensitivity_jan_march_swc_conus_coef_only)
hist(sensitivity_jan_march_swc_conus_coef_only$coef)
sd(sensitivity_jan_march_swc_conus_coef_only$coef)
sensitivity_jan_march_swc_raster<-rasterFromXYZ(sensitivity_jan_march_swc_conus_coef_only)
plot(sensitivity_jan_march_swc_raster)
break_sensitivity_jan_march_swc<-quantile(sensitivity_jan_march_swc_conus_coef_only$coef,seq(from=0.1, to = .9,by=0.01),na.rm=TRUE)

spplot(sensitivity_jan_march_swc_raster,#scales = list(draw = TRUE),
       at=break_sensitivity_jan_march_swc,
       asp=1,
       col.regions =
         rev(heat_hcl(length(break_sensitivity_jan_march_swc)-1)),
       main="") +
  latticeExtra::layer(sp.polygons(states_all_sites, lwd = 0.1))

#NPP sensitivity to April-June soil moisture
sensitivity_april_june_swc_conus <- rangeland_npp_covariates_1 %>% group_by(x, y) %>%
  dplyr::do(model = lm(npp.x~april_june_swc, data = .)) %>%
  dplyr::mutate(coef=coef(model)[2])

head(sensitivity_april_june_swc_conus)
sensitivity_april_june_swc_conus_coef_only<- sensitivity_april_june_swc_conus[ -c(3) ] #isolate coefficient so only slope is graphed
head(sensitivity_april_june_swc_conus_coef_only)
summary(sensitivity_april_june_swc_conus_coef_only)
hist(sensitivity_april_june_swc_conus_coef_only$coef)
sd(sensitivity_april_june_swc_conus_coef_only$coef)
sensitivity_april_june_swc_raster<-rasterFromXYZ(sensitivity_april_june_swc_conus_coef_only)
plot(sensitivity_april_june_swc_raster)
break_sensitivity_april_june_swc<-quantile(sensitivity_april_june_swc_conus_coef_only$coef,seq(from=0.01, to = .05,by=0.01),na.rm=TRUE)

spplot(sensitivity_april_june_swc_raster,#scales = list(draw = TRUE),
       at=break_sensitivity_april_june_swc,
       asp=1,
       col.regions =
         rev(heat_hcl(length(break_sensitivity_april_june_swc)-1)),
       main="April-June extreme sensitivity pixels (below 5th percentile)") +
  latticeExtra::layer(sp.polygons(states_all_sites, lwd = 0.1))



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
