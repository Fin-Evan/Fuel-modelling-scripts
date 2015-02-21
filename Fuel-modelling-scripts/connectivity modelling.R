install.packages("igraph")
library(gdistance)
library(sp)
library(maptools)
library(raster)
library(rgeos)
library(rgdal)


Walfa_tslf2000.4<-raster("/Users/finleyroberts/Documents/spatial_analysis/landsat fire scars/Landsat_firescars/TSF rasters/WALFA_TSF/2004.4_TSF_WALFA.tif")


transition_layer<-transition(Walfa_tslf2000.4, transitionFunction=mean, symm=FALSE,  directions = 8)
transition_layer<-geoCorrection(transition_layer, scl=FALSE)

WALFA_points<-readShapePoints("/Users/finleyroberts/Documents/spatial_analysis/ignition points/WALFA_10kmPt.shp")

WALFA_points_table<-as.data.frame(WALFA_points)
write.csv(WALFA_points_table, file="/Users/finleyroberts/Documents/spatial_analysis/ignition points/walfa_points.csv")
WALFA_points_table<-read.csv("/Users/finleyroberts/Documents/spatial_analysis/ignition points/walfa_points.csv")


WALFA_ig_pt_num<-c(1:244)
i=2

WALFA_points_table[i,]
ignition_point_i<-SpatialPoints(WALFA_points_table[i,], proj4string = CRS("+proj=utm +zone=53 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs"),)



plot(WALFA_boundary_raster)
plot(ignition_point_i, add=TRUE)

proj4string = CRS(+proj=utm +zone=53 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0),

length(WALFA_points_table$FID_Raster)

fuel_data[fuel_data$veg=="EOF",]


cost_surface<-accCost(transition_layer, WALFA_points)

WALFA_poly<-readShapePoly("/Users/finleyroberts/Documents/spatial_analysis/boundaries/old_WALFA.shp")

# generate random sample points within polygon
random.points <- spsample(WALFA_poly,n=1,type="random")
random.points2 <- spsample(WALFA_poly,n=1,type="random")

point_raster<-rasterize(random.points2, areas3raster, field=1)

areas3raster<-raster("/Users/finleyroberts/Documents/spatial_analysis/3 areas new raster/3areas_ras.tif")

#rasterizing points

point_raster<-rasterFromXYZ(WALFA_points_table[i,], res=c(100,100), digits = 5)
point_raster_ext<-raster(point_raster,new_walfa_resampled)

point_raster<-rasterize(WALFA_points_table[i,], field=2, new_walfa_resampled, background=1)
point_raster_masked<-mask(point_raster, new_walfa_resampled)


presence.absence.raster <- function (mask.raster,species.data,raster.label="") {
  require(raster)
  
  # set the background cells in the raster to 0
  new_walfa_resampled[!is.na(new_walfa_resampled)] <- 0
  
  #set the cells that contain points to 1
  ignitionRaster <- rasterize(ignition_point_i,new_walfa_resampled,field=1)
  ignitionRaster <- merge(ignitionRaster,new_walfa_resampled)
  ignitionRaster[ignitionRaster==1]<-NA
  igntion_euc_distance<-distance(ignitionRaster)
  igntion_euc_distance<-mask(igntion_euc_distance,new_walfa_resampled)
  
  
  #label the raster
  names(speciesRaster) <- raster.label
  return(speciesRaster)
}






point_raster[point_raster==2]<-NA

point_raster_stack<-stack(point_raster, new_walfa_resampled)

tsf_stack<-stack(year_tsf,tsf_stack)
}
tsf_mosaic<-calc(tsf_stack, fun=max)


r <- raster(nrow=10, ncol=10, xmn=0, xmx=10, ymn=0, ymx=10, crs=NA)
r[] <- runif(ncell(r))
r[r<0.5] <- NA
xyz <- rasterToPoints(r)



###############################

Kakadu_boundary<-readShapePoly("/Users/finleyroberts/Documents/spatial_analysis/boundaries/kakadu_reprojected_v2.shp")
plot(Kakadu_boundary)
kakadu_walfa_ext<-extent(161712.7, 407070.3, 8451190, 8662426)
kakadu_rast_ext<-raster(kakadu_walfa_ext, ncol=1000,nrow=1000)
Kakadu_boundary_raster<-rasterize(Kakadu_boundary, kakadu_rast_ext)
plot(Kakadu_boundary_raster)


plot(random.points2, add=TRUE)

cost_surface<-accCost(transition_layer, random.points2)
writeRaster(cost_surface,"/Users/finleyroberts/Documents/spatial_analysis/test_cost.tif")
cost_test<-raster("/Users/finleyroberts/Documents/spatial_analysis/test_cost.tif")
plot(cost_test)


cost_surface2<-cost_test
cost_surface2[is.na(cost_surface)]<-0
plot(cost_surface)


cost_surface2[cost_surface2==0]<-NA
euc_dist_for_connect<-distance(cost_surface2, filename='/Users/finleyroberts/Documents/spatial_analysis/test_euc.tif') 


Values_raster_point<-extract(cost_test, random.points2, method='simple', df=FALSE, cellnumbers=TRUE)

length(cost_test[cost_test==0])
length(cost_test[cost_test==NA])
cost_surface2<-cost_test
cost_surface2[cost_surface2==0]<-NA
length(cost_surface2[cost_surface2==NA])

cost_test[ncell=1957241]

euc_dist_for_connect<-distance(cost_surface2, filename='/Users/finleyroberts/Documents/spatial_analysis/test_euc2.tif', overwrite=TRUE) 

euc_dist_for_connect_resampled<-resample(euc_dist_for_connect, WALFA_boundary_raster, method="ngb")
plot(WALFA_boundary_raster)
euc_dist_for_connect_mask<-mask(euc_dist_for_connect_resampled, WALFA_boundary_raster)

plot(euc_dist_for_connect_mask)
euc_dist_calc[euc_dist_calc==0]<-NA





