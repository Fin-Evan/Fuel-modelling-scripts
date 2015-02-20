

library(sp)
library(maptools)
library(raster)
library(rgeos)
library(rgdal)

stone_veg<-raster("/Users/finleyroberts/Documents/spatial_analysis/KNP Stone Country veg map/sandstone")
areas3raster<-raster("/Users/finleyroberts/Documents/spatial_analysis/3 areas new raster/3areas_ras.tif")
WALFA_veg_rast<-raster("/Users/finleyroberts/Documents/spatial_analysis/Walfa veg map/WALFA_veg_v2.tif")
new_WALFA_boundary<-raster("/Users/finleyroberts/Documents/spatial_analysis/New boundary rasters/old_WALFA_raster.tif")

new_walfa_resampled<-new_WALFA_boundary
new_walfa_resampled<-resample(new_WALFA_boundary,areas3raster, method="ngb")
WALFA_veg_rast_resampled<-WALFA_veg_rast
WALFA_veg_rast_resampled<-resample(WALFA_veg_rast,areas3raster, method="ngb")

WALFA_veg_rast_masked<-mask(WALFA_veg_rast_resampled,new_walfa_resampled,filename="/Users/finleyroberts/Documents/spatial_analysis/Walfa veg map/WALFA_veg_masked.tif",
                            overwrite=TRUE)
plot(WALFA_veg_rast_masked)

#reclassifying values for WALFA veg map so that they are consistent with stone country veg map and combining sandstone woodland and woodland

WALFA_veg_reclass<-matrix(c(1,3,2,3,3,1,4,1,5,2), ncol=2,byrow=TRUE) #from and to

WALFA_veg_reclassied<-reclassify(WALFA_veg_rast_masked, WALFA_veg_reclass, filename="/Users/finleyroberts/Documents/spatial_analysis/Walfa veg map/WALFA_veg_reclassified.tif",
                                 overwrite=TRUE)





#combining Open woodland and woodland in stone country veg map

stone_veg_resampled<-resample(stone_veg, areas3raster, method="ngb")

stone_veg_reclass<-matrix(c(1,1,2,1,3,2,4,3,5,3), ncol=2,byrow=TRUE) #from and to

stone_veg_reclassied<-reclassify(stone_veg_resampled, stone_veg_reclass, filename="/Users/finleyroberts/Documents/spatial_analysis/Walfa veg map/stone_veg_reclassified.tif",
                                 overwrite=TRUE)

#mosaic veg maps (combining)
veg_map_combined<-mosaic(WALFA_veg_reclassied, stone_veg_reclassied, fun=max, filename="/Users/finleyroberts/Documents/spatial_analysis/Walfa veg map/combined_veg.tif", overwrite=TRUE)







writeRaster(stone_veg_resampled, file="/Users/finleyroberts/Documents/spatial_analysis/KNP Stone Country veg map/stone_veg_resampled.tif")

fuel_data<-read.csv("/Users/finleyroberts/Documents/spatial_analysis/fuel mapping/fuels data from Brett (prefire only).csv", header=T)

names(fine_fuels)[names(fine_fuels)=="tsb"]<-"tsf"



WALFA_veg_rast_resampled<-resample(WALFA_veg_rast,areas_3_raster, method="ngb")

