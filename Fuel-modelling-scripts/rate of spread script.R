
#Rate of spread using McArthur Mk 5 grassland fire danger equations from Noble et al. 1980

fuel_map_rasters<-dir("/Users/finleyroberts/Documents/spatial_analysis/fuel mapping/fuel_maps_by_season",pattern=".tif", full.names=T)

fuel_moisture_function<-function(x,y,z) {((97.7 + 4.06*x) / (y + 6.0)) - 0.00854*x +(3000.0/z-30.0)}
#where x is H, y is T and z is C

#assume that C=100 in LDS and 


#when M is <18% 
Fire_danger_function<-function(x,y,z) {3.35.x*exp(-0.0897*y + 0.0403*z)}
#where x is W (fuel weight), y is M(fuel moisture), z is V(average wind velocity)






fuel_weight_function<-function(x,y,z) {(x*(1-exp(-y*z))) }