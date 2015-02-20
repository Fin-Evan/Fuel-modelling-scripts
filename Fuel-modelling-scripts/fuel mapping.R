
library(nlme)
library(plyr)
library(dismo)
library(packrat)
library(base)
library(sp)
library(maptools)
library(raster)
library(rgeos)
library(rgdal)

#Fine fuels

#Brett's data: data<-read.table("C:/Users/brettm/Dropbox/Savanna carbon/Spinifex/Data/Files for R/spinifex_fuels.txt", header=T)

fine_fuels<-read.table("/Users/finleyroberts/Documents/spatial_analysis/fuel mapping/fine.txt", header=T)
names(fine_fuels)

fuel_data<-read.csv("/Users/finleyroberts/Documents/spatial_analysis/fuel mapping/fuels data from Brett (prefire only and woodland).csv", header=T)



#subsetting
#just fine
fuel_data[fuel_data$veg=="EOF",c("fine","tsf")]

veg_types<-c("EOF","W","SH")
fuel_types<-c("grass","litter","fine")

i=1
j=2
fuel_type_i<-as.vector(fuel_types[j])

fuel_load_data_i<-fuel_data$fuel_type_i

fuel_data[fuel_data$veg]


for (j in seq(fuel_types)){
  fuel_type_j<-fuel_data[,c("veg", "tsf",fuel_types[j])]                                           
  for (i in seq(veg_types)){
    subset_by_veg<-fuel_type_j[fuel_type_j$veg==veg_types[i],]
    subset_by_veg<-na.omit(subset_by_veg)
    names(subset_by_veg)[names(subset_by_veg)==fuel_types[j]]<-"fuel_type"
    fuel_model_gnls_i<-nls(fuel_type~ss*(1-exp(-k*tsf)), data=subset_by_veg, start=c(ss=9.15,k=0.093))
  }
}

#EOF
i=1

#Grass fuels

j-1

fuel_type_j<-fuel_data[,c("veg", "tsf",fuel_types[j])]
subset_by_veg<-fuel_type_j[fuel_type_j$veg==veg_types[i],]
EOF_subset<-na.omit(subset_by_veg)
EOW_model_gnls_i<-gnls(grass~ss*(1-exp(-k*tsf)), data=EOF_subset, start=c(ss=2.15,k=0.093))

#litter

j=2

fuel_type_j<-fuel_data[,c("veg", "tsf",fuel_types[j])]
subset_by_veg<-fuel_type_j[fuel_type_j$veg==veg_types[i],]
EOF_subset<-na.omit(subset_by_veg)
EOW_model_gnls_i<-gnls(litter~ss*(1-exp(-k*tsf)), data=EOF_subset, start=c(ss=2.15,k=0.093))

#fine

j=3

fuel_type_j<-fuel_data[,c("veg", "tsf",fuel_types[j])]
subset_by_veg<-fuel_type_j[fuel_type_j$veg==veg_types[i],]
EOF_subset<-na.omit(subset_by_veg)
EOW_model_gnls_i<-gnls(fine~ss*(1-exp(-k*tsf)), data=EOF_subset, start=c(ss=2.15,k=0.093))

#W
i=2

#Grass fuels

j=1

fuel_type_j<-fuel_data[,c("veg", "tsf",fuel_types[j])]
subset_by_veg<-fuel_type_j[fuel_type_j$veg==veg_types[i],]
W_subset<-na.omit(subset_by_veg)
W_model_gnls_i<-gnls(grass~ss*(1-exp(-k*tsf)), data=W_subset, start=c(ss=2.15,k=0.093))

#litter

j=2

fuel_type_j<-fuel_data[,c("veg", "tsf",fuel_types[j])]
subset_by_veg<-fuel_type_j[fuel_type_j$veg==veg_types[i],]
W_subset<-na.omit(subset_by_veg)
W_model_gnls_i<-gnls(litter~ss*(1-exp(-k*tsf)), data=W_subset, start=c(ss=2.15,k=0.093))

#fine

j=3

fuel_type_j<-fuel_data[,c("veg", "tsf",fuel_types[j])]
subset_by_veg<-fuel_type_j[fuel_type_j$veg==veg_types[i],]
W_subset<-na.omit(subset_by_veg)
W_model_gnls_i<-gnls(fine~ss*(1-exp(-k*tsf)), data=W_subset, start=c(ss=2.15,k=0.093))

#SH
i=3

#Grass fuels

j=1

fuel_type_j<-fuel_data[,c("veg", "tsf",fuel_types[j])]
subset_by_veg<-fuel_type_j[fuel_type_j$veg==veg_types[i],]
SH_subset<-na.omit(subset_by_veg)
SH_model_gnls_i<-gnls(grass~ss*(1-exp(-k*tsf)), data=SH_subset, start=c(ss=2.15,k=0.093))

#litter

j=2

fuel_type_j<-fuel_data[,c("veg", "tsf",fuel_types[j])]
subset_by_veg<-fuel_type_j[fuel_type_j$veg==veg_types[i],]
SH_subset<-na.omit(subset_by_veg)
SH_model_gnls_i<-gnls(litter~ss*(1-exp(-k*tsf)), data=SH_subset, start=c(ss=2.15,k=0.093))

#fine

j=3

fuel_type_j<-fuel_data[,c("veg", "tsf",fuel_types[j])]
subset_by_veg<-fuel_type_j[fuel_type_j$veg==veg_types[i],]
SH_subset<-na.omit(subset_by_veg)
SH_model_gnls_i<-gnls(fine~ss*(1-exp(-k*tsf)), data=SH_subset, start=c(ss=2.15,k=0.093))



plot( subset_by_veg$tsf, subset_by_veg$fuel_type)

log_lm<-lm(log(tsf)~log(fuel_type), data=subset_by_veg)

curve(predict(exp(log_lm), newdata=data.frame(tsf = x)), add=TRUE)


fuel_load_data_i<-fuel_data[fuel_data$veg==veg_types[i],c(fuel_types, "tsf")]

early <- year[grepl(paste0("e",years[i],"|ew",years[i]),year,ignore.case=FALSE)]

#all columns
Fuel_i<-fuel_data[fuel_data$veg==veg_types[i], ]     


#there's a subset command!!!!

#subset(fuel_data, fuel_data$veg=="EOF", select=c(tsf, grass))

#subset(fuel_data, fuel_data$veg==veg_types[i], select=c(tsf,))


#Amy's help
fuel_data[fuel_data$veg==veg_types[i],c("tsf",fuel_types[j])]
#data[rows,colums]


#fine fuels

#parameter values
fuel_paramters<-read.csv("/Users/finleyroberts/Documents/spatial_analysis/fuel mapping/parameter values (ss and k).csv")

# making SS rasters for each fuel type

#grass raster - WALFA

WALFA_fuel_grass_SS<-WALFA_veg_reclassied

#EOF and Closed forest
WALFA_fuel_grass_SS[WALFA_fuel_grass_SS==3]<-0.6763953
#W
WALFA_fuel_grass_SS[WALFA_fuel_grass_SS==1]<-1.5252610
#SH
WALFA_fuel_grass_SS[WALFA_fuel_grass_SS==2]<-2.1748202

writeRaster(WALFA_fuel_grass_SS, file="/Users/finleyroberts/Documents/spatial_analysis/fuel mapping/parameter rasters/WALFA_fuel_grass_SS.tif", overwrite=TRUE)

#litter raster - WALFA

WALFA_fuel_litter_SS<-WALFA_veg_reclassied

#EOF and Closed forest
WALFA_fuel_litter_SS[WALFA_fuel_litter_SS==3]<-4.126545
#W
WALFA_fuel_litter_SS[WALFA_fuel_litter_SS==1]<-3.9633268
#SH
WALFA_fuel_litter_SS[WALFA_fuel_litter_SS==2]<-2.6980156

writeRaster(WALFA_fuel_litter_SS, file="/Users/finleyroberts/Documents/spatial_analysis/fuel mapping/parameter rasters/WALFA_fuel_litter_SS.tif", overwrite=TRUE)

#fine raster - WALFA

WALFA_fuel_fine_SS<-WALFA_veg_reclassied

#EOF and Closed forest
WALFA_fuel_fine_SS[WALFA_fuel_fine_SS==3]<-5.408311
#W
WALFA_fuel_fine_SS[WALFA_fuel_fine_SS==1]<-5.5001841
#SH
WALFA_fuel_fine_SS[WALFA_fuel_fine_SS==2]<-18.4840905

writeRaster(WALFA_fuel_fine_SS, file="/Users/finleyroberts/Documents/spatial_analysis/fuel mapping/parameter rasters/WALFA_fuel_fine_SS.tif", overwrite=TRUE)



# making K rasters for each fuel type

#grass raster - WALFA

WALFA_fuel_grass_K<-WALFA_veg_reclassied

#EOF and Closed forest
WALFA_fuel_grass_K_EOF<-WALFA_veg_reclassied
WALFA_fuel_grass_K_EOF[WALFA_fuel_grass_K_EOF==3]<-2.1887441
#W
WALFA_fuel_grass_K[WALFA_fuel_grass_K==1]<-1.398351
#SH
WALFA_fuel_grass_K[WALFA_fuel_grass_K==2]<-0.5240727

writeRaster(WALFA_fuel_grass_K, file="/Users/finleyroberts/Documents/spatial_analysis/fuel mapping/parameter rasters/WALFA_fuel_grass_K.tif", overwrite=TRUE)

#litter raster - WALFA

WALFA_fuel_litter_K<-WALFA_veg_reclassied

#EOF and Closed forest
WALFA_fuel_litter_K[WALFA_fuel_litter_K==3]<-1.840828
#W
WALFA_fuel_litter_K[WALFA_fuel_litter_K==1]<-0.8870675
#SH
WALFA_fuel_litter_K[WALFA_fuel_litter_K==2]<-0.5724407

writeRaster(WALFA_fuel_litter_K, file="/Users/finleyroberts/Documents/spatial_analysis/fuel mapping/parameter rasters/WALFA_fuel_litter_K.tif", overwrite=TRUE)

#fine raster - WALFA

WALFA_fuel_fine_K<-WALFA_veg_reclassied

#EOF and Closed forest
WALFA_fuel_fine_K[WALFA_fuel_fine_K==3]<-1.311802
#W
WALFA_fuel_fine_K[WALFA_fuel_fine_K==1]<-0.9843566
#SH
WALFA_fuel_fine_K[WALFA_fuel_fine_K==2]<-0.1112513

writeRaster(WALFA_fuel_fine_K, file="/Users/finleyroberts/Documents/spatial_analysis/fuel mapping/parameter rasters/WALFA_fuel_fine_K.tif", overwrite=TRUE)


#####################

#calculating the fuel weight for 3 fuel types and 3 veg types

grass_fuel_K<-raster("/Users/finleyroberts/Documents/spatial_analysis/fuel mapping/parameter rasters/WALFA_fuel_grass_K.tif")
grass_fuel_SS<-raster("/Users/finleyroberts/Documents/spatial_analysis/fuel mapping/parameter rasters/WALFA_fuel_grass_SS.tif")
WALFA_tslf2011.6<-raster("/Users/finleyroberts/Documents/spatial_analysis/resampled WALFA tslf rasters/2011.6_walfa_tslf_resample.tif")

fuel_weight_function<-function(x,y,z) {(x*(1-exp(-y*z))) }

grass_fuel_raster<-overlay(grass_fuel_SS, grass_fuel_K, WALFA_tslf2011.6, fun=fuel_weight_function)


K_param_rasters<-c("WALFA_fuel_fine_K.tif",   "WALFA_fuel_grass_K.tif",  "WALFA_fuel_litter_K.tif")
SS_param_rasters<-c("WALFA_fuel_fine_SS.tif",   "WALFA_fuel_grass_SS.tif",  "WALFA_fuel_litter_SS.tif") 



parameter_rasters<-dir("/Users/finleyroberts/Documents/spatial_analysis/fuel mapping/parameter rasters", pattern=".tif",full.names=T)
WALFA_tslf_resampled_rasters<-dir("/Users/finleyroberts/Documents/spatial_analysis/resampled WALFA tslf rasters", pattern=".tif", full.names=T)

season_reverse_chrono_char<-c("2011.6",
                              "2011.4",
                              "2010.6",
                              "2010.4",
                              "2009.6",
                              "2009.4",
                              "2008.6",
                              "2008.4",
                              "2007.6",
                              "2007.4",
                              "2006.6",
                              "2006.4",
                              "2005.6",
                              "2005.4",
                              "2004.6",
                              "2004.4",
                              "2003.6",
                              "2003.4",
                              "2002.6",
                              "2002.4",
                              "2001.6",
                              "2001.4",
                              "2000.6",
                              "2000.4",
                              "1999.6",
                              "1999.4",
                              "1998.6",
                              "1998.4",
                              "1997.6",
                              "1997.4",
                              "1996.6",
                              "1996.4",
                              "1995.6",
                              "1995.4",
                              "1994.6",
                              "1994.4",
                              "1993.6",
                              "1993.4",
                              "1992.6",
                              "1992.4",
                              "1991.6",
                              "1991.4",
                              "1990.6",
                              "1990.4",
                              "1989.6",
                              "1989.4")

i=1
j=1

fuel_comp_stack<-c()

for (i in seq(season_reverse_chrono_char)){
  tslf_i<-raster(WALFA_tslf_resampled_rasters[grepl(season_reverse_chrono_char[i],WALFA_tslf_resampled_rasters)])
  for (j in seq(SS_param_rasters)){
    kk_raster_j<-raster(parameter_rasters[grepl(K_param_rasters[j],parameter_rasters)])
    ss_raster_j<-raster(parameter_rasters[grepl(SS_param_rasters[j],parameter_rasters)])  
    fuel_raster_for_comp_j<-overlay(ss_raster_j, kk_raster_j, tslf_i, fun=fuel_weight_function)
    fuel_comp_stack<-stack(fuel_raster_for_comp_j, fuel_comp_stack)  
  }
  fuel_weight_raster<-calc(fuel_comp_stack, fun=sum, filename=paste0("/Users/finleyroberts/Documents/spatial_analysis/fuel mapping/fuel_maps_by_season/",season_reverse_chrono_char[i],"_fuel_map.tif"), overwrite=TRUE)  
}



for(i in seq(years)){
  year <- reclassified_rasters[grepl(years[i],reclassified_rasters)]
  early <- year[grepl(paste0("e",years[i],"|ew",years[i]),year,ignore.case=FALSE)]
  early.stack <- stack(early) # load all the early rasters for year i
  early.mosaic <- mosaic(early.stack[[1]],early.stack[[2]],fun=max) # make sure you have add all the rasters in the stack
  writeRaster(early.mosaic,file=paste0("/Users/finleyroberts/Documents/spatial_analysis/landsat fire scars/Landsat_firescars/new Mosaic-ed rasters (Kakadu and WALFA)/",years[i],"_early.tif"),overwrite=T)
}








#extract function tries to turn into a function

gc()  

grass_fuel_SS*(1-exp(-grass_fuel_K*WALFA_tslf2011.6)))

EOW_model_gnls_i<-gnls(grass~ss*(1-exp(-k*tsf)), data=EOF_subset, start=c(ss=2.15,k=0.093))





EOF_fine<-summary(EOF_fine_model_gnls)$coefficients
EOF_fine["veg type"]<-"EOF"
EOF_fine["fuel type"]<-"fine"
EOF_fine<-as.data.frame(EOF_fine)

EW_fine<-summary(SH_fine_model_gnls)$coefficients
EW_fine["veg type"]<-"EW"
EW_fine["fuel type"]<-"fine"
EW_fine<-as.data.frame(EW_fine)

SH_fine<-summary(SH_fine_model_gnls)$coefficients
SH_fine["veg type"]<-"SH"
SH_fine["fuel type"]<-"fine"
SH_fine<-as.data.frame(SH_fine)

SW_fine<-summary(SH_fine_model_gnls)$coefficients
SW_fine["veg type"]<-"SW"
SW_fine["fuel type"]<-"fine"
SW_fine<-as.data.frame(SW_fine)

fuel_model_parameters<-cbind(EOF_fine, EW_fine, SH_fine, SW_fine)

#gras fuels
EOF_grass_model_gnls<-gnls(grass~ss*(1-exp(-k*tsf)), data=fuel_data[fuel_data$veg=="EOF",] , start=c(ss=9.15,k=0.093))
EW_grass_model_gnls<-gnls(grass~ss*(1-exp(-k*tsf)), data=fuel_data[fuel_data$veg=="EW",] , start=c(ss=9.15,k=0.093))
SH_grass_model_gnls<-gnls(grass~ss*(1-exp(-k*tsf)), data=fuel_data[fuel_data$veg=="SH",] , start=c(ss=9.15,k=0.093))
SW_grass_model_gnls<-gnls(grass~ss*(1-exp(-k*tsf)), data=fuel_data[fuel_data$veg=="SW",] , start=c(ss=9.15,k=0.093))

plot(fine ~ tsf, data=fuel_data, main="fine fuels")
plot(litter ~ tsf, data=fuel_data, main="litter fuels")

plot(grass ~ tsf, data=fuel_data[fuel_data$veg=="EOF",], main="EOF grass fuels")
plot(grass ~ tsf, data=fuel_data[fuel_data$veg=="EW",], main="EW grass fuels")
plot(grass ~ tsf, data=fuel_data[fuel_data$veg=="SH",], main="SH grass fuels")
plot(grass ~ tsf, data=fuel_data[fuel_data$veg=="SW",], main="SW grass fuels")

[fuel_data$veg=="EOF",])

#parameter values


EOF_grass<-summary(EOF_grass_model_gnls)$coefficients
EOF_grass["veg type"]<-"EOF"
EOF_grass["fuel type"]<-"grass"
EOF_grass<-as.data.frame(EOF_grass)

EW_grass<-summary(SH_grass_model_gnls)$coefficients
EW_grass["veg type"]<-"EW"
EW_grass["fuel type"]<-"grass"
EW_grass<-as.data.frame(EW_grass)

SH_grass<-summary(SH_grass_model_gnls)$coefficients
SH_grass["veg type"]<-"SH"
SH_grass["fuel type"]<-"grass"
SH_grass<-as.data.frame(SH_grass)

SW_grass<-summary(SH_grass_model_gnls)$coefficients
SW_grass["veg type"]<-"SW"
SW_grass["fuel type"]<-"grass"
SW_grass<-as.data.frame(SW_grass)

fuel_model_parameters<-cbind(EOF_fine, EW_fine, SH_fine, SW_fine)








write.csv(fuel_model_parameters, file="/Users/finleyroberts/Documents/spatial_analysis/fuel mapping/fuel_model_parameters.csv")



curve(predict(fine_model_nls, newdata=data.frame(tsb=x)), add=TRUE, col="red")

tsf_1996.6<-raster("/Users/finleyroberts/Documents/spatial_analysis/landsat fire scars/Landsat_firescars/TSF rasters/WALFA_TSF/1996.6_TSF_WALFA.tif")
plot(tsf_1996.6)
names(tsf_1996.6)[names(tsf_1996.6)=="1996.6_TSF_WALFA "]<-"tsf"
tsf<-tsf_1996.6


toto=matrix(0,5,5)
tata=c(1,2,3)
toto[1:length(tata)]=tata

#alternative is to vectorise and apply model equation directly to data


model<-lm(log(biomass)~log(tsf)+log(rainfall))

null<-lm(biomass~1)
olson1<-nls(biomass~ss*(1-exp(-k*tsf)), start=c(ss=9.15,k=0.093))
olson2<-nls(biomass~(ss1*rainfall+ss2)*(1-exp(-(k1*rainfall+k2)*tsf)), start=c(ss1=0.1, ss2=-10, k1=0.0005, k2=0.01))



fit1<-1-deviance(olson1)/deviance(null)
fit2<-1-deviance(olson2)/deviance(null)

plot(tsf, biomass)
points(tsf, predict(olson), pch=16)

plot(biomass, predict(olson2))



fine_model_lm<-lm(log(fine)~log(tsf))

plot(fine ~ tsf, xlab="time since last fire (years)", ylab="fine fuel biomass (t Ha-1)")
curve(exp(predict(fine_model, newdata=data.frame(tsb=x))), add=TRUE, col="blue")

fine_model_veg<-lm(log(fine)~log(tsf)+veg)

fine_model_nls<-nls(fine~ss*(1-exp(-k*tsf)), data=fine_fuels, start=c(ss=9.15,k=0.093))
