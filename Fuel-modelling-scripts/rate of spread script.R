
#Rate of spread using McArthur Mk 5 grassland fire danger equations from Noble et al. 1980

fuel_map_rasters<-dir("/Users/finleyroberts/Documents/spatial_analysis/fuel mapping/fuel_maps_by_season",pattern=".tif", full.names=T)

#fuel_moisture_function<-function(x,y,z) {((97.7 + 4.06*x) / (y + 6.0)) - 0.00854*x +(3000.0/z-30.0)}
#where x is H, y is T and z is C
#assume that C=100 in LDS and 

#using the fuel moiature values from Williams et al. 1998 Seasonal changes in Fire behaviour in tropical savanna....

EDS_moisture<-WALFA_boundary_raster
EDS_moisture[EDS_moisture==0]<- 19.3
writeRaster(EDS_moisture, filename = "/Users/finleyroberts/Documents/spatial_analysis/Rate of spread/EDS_moisture.tif", overwrite=T)

LDS_moisture<-WALFA_boundary_raster
LDS_moisture[LDS_moisture==0]<- 11.3
writeRaster(LDS_moisture, filename = "/Users/finleyroberts/Documents/spatial_analysis/Rate of spread/LDS_moisture.tif", overwrite=T)



#when M is <18% (i.e. late dry season)
LDS_Fire_danger_function<-function(x,y,z) {3.35.x*exp(-0.0897*y + 0.0403*z)}
#where x is W (fuel weight), y is M(fuel moisture), z is V(average wind velocity)

#when M is between 18.8 and 30% (i.e. early dry season)
EDS_Fire_danger_function<-function(0.299*x.exp(-1.686 + 0.0403 *z) * (30 - y))
#where x is W (fuel weight), y is M(fuel moisture), z is V(average wind velocity)

  #rate of spread will then be R = 0.13.F
  

fuel_weight_function<-function(x,y,z) {(x*(1-exp(-y*z))) }