
library(base)
library(sp)
library(maptools)
library(raster)
library(rgeos)
library(rgdal)

areas3raster<-raster("/Users/finleyroberts/Documents/spatial_analysis/3 areas new raster/3areas_ras.tif")
new_WALFA_boundary<-raster("/Users/finleyroberts/Documents/spatial_analysis/New boundary rasters/old_WALFA_raster.tif")
new_walfa_resampled<-new_WALFA_boundary
WALFA_boundary_raster<-resample(new_WALFA_boundary,areas3raster, method="ngb")


lw00<-raster("/Users/finleyroberts/Documents/spatial_analysis/landsat fire scars/Landsat_firescars/new reclassified rasters (2 areas)/lw00.tif")
lw00_reclass<-raster("/Users/finleyroberts/Documents/spatial_analysis/landsat fire scars/Landsat_firescars/new reclassified rasters (2 areas)/new WALFA reclass/lw00.tif")

#WALFA reclassify, stack and clip to park boundaries
setwd("/Users/finleyroberts/Documents/spatial_analysis/landsat fire scars/Landsat_firescars/new reclassified rasters (2 areas)/new WALFA reclass")
raw_walfa_raster<-dir("/Users/finleyroberts/Documents/spatial_analysis/landsat fire scars/Landsat_firescars/WALFA (LDS+EDS)", full.names = T)

walfa_rasters<-dir("/Users/finleyroberts/Documents/spatial_analysis/landsat fire scars/Landsat_firescars/new reclassified rasters (2 areas)/new WALFA reclass", pattern=".tif", full.names=T)

WALFA_firescars<-c(
  "ew00",
  "ew01",
  "ew02",
  "ew03",
  "ew04",
  "ew05",
  "ew06",
  "ew07",
  "ew08",
  "ew09",
  "ew10",
  "ew11",
  "ew90",
  "ew91",
  "ew92",
  "ew93",
  "ew94",
  "ew95",
  "ew96",
  "ew97",
  "ew98",
  "ew99",
  "lw00",
  "lw01",
  "lw02",
  "lw03",
  "lw04",
  "lw05",
  "lw06",
  "lw07",
  "lw08",
  "lw09",
  "lw10",
  "lw11",
  "lw90",
  "lw91",
  "lw92",
  "lw93",
  "lw94",
  "lw95",
  "lw96",
  "lw97",
  "lw98",
  "lw99")

i=20

#resampling to 3 areas raster
for(i in seq(WALFA_firescars)){
  r<-raster(paste0(WALFA_firescars[i],".tif"))
  r<-resample(r,WALFA_boundary_raster, method="ngb", filename=paste0("/Users/finleyroberts/Documents/spatial_analysis/landsat fire scars/Landsat_firescars/new reclassified rasters (2 areas)/3areas/",WALFA_firescars[i],"_3areas.tif"),overwrite=TRUE)
}



walfa_rasters<-dir("/Users/finleyroberts/Documents/spatial_analysis/landsat fire scars/Landsat_firescars/new reclassified rasters (2 areas)/3areas", pattern=".tif", full.names=T)

years_chrono<-c("90",
                "91",
                "92",
                "93",
                "94",
                "95",
                "96",
                "97",
                "98",
                "99",
                "00",
                "01",
                "02",
                "03",
                "04",
                "05",
                "06",
                "07",
                "08",
                "09",
                "10",
                "11")

years_chrono_numeric<-c(1990,
                        1991,
                        1992,
                        1993,
                        1994,
                        1995,
                        1996,
                        1997,
                        1998,
                        1999,
                        2000,
                        2001,
                        2002,
                        2003,
                        2004,
                        2005,
                        2006,
                        2007,
                        2008,
                        2009,
                        2010,
                        2011)

##### sub strings!!! getting characters 3 and 4

i=1


#early dry season
for(i in seq(years_chrono_numeric)){
  year_str<-substr(as.character(years_chrono_numeric[i]), 3, 4)
  year<-grep(year_str,walfa_rasters, value=TRUE)
  early_year<- year[grepl(paste0("ew",year_str),year,ignore.case=FALSE)]
  rast_year<-raster(early_year)
  rast_year[rast_year==1]<-years_chrono_numeric[i]+0.6
  writeRaster(rast_year,file=paste0("/Users/finleyroberts/Documents/spatial_analysis/landsat fire scars/Landsat_firescars/num_TSF/new version/",years_chrono_numeric[i],".6_num_tsf.tif"),overwrite=T)
  
}

#late dry season
for(i in seq(years_chrono_numeric)){
  year_str<-substr(as.character(years_chrono_numeric[i]), 3, 4)
  year<-grep(year_str,walfa_rasters, value=TRUE)
  late_year<- year[grepl(paste0("lw",year_str),year,ignore.case=FALSE)]
  rast_year<-raster(late_year)
  rast_year[rast_year==1]<-years_chrono_numeric[i]+0.9
  writeRaster(rast_year,file=paste0("/Users/finleyroberts/Documents/spatial_analysis/landsat fire scars/Landsat_firescars/num_TSF/new version/",years_chrono_numeric[i],".9_num_tsf.tif"),overwrite=T)
  
}

"/Users/finleyroberts/Documents/spatial_analysis/landsat fire scars/Landsat_firescars/num_TSF/new version"

num_tsf_1989<-WALFA_boundary_raster
num_tsf_1989[num_tsf_1989==0]<-1989
writeRaster(num_tsf_1989,"/Users/finleyroberts/Documents/spatial_analysis/landsat fire scars/Landsat_firescars/num_TSF/new version/1989.6_num_tsf.tif")
writeRaster(num_tsf_1989,"/Users/finleyroberts/Documents/spatial_analysis/landsat fire scars/Landsat_firescars/num_TSF/new version/1989.9_num_tsf.tif")




season_reverse_chrono<-c(2011.9,
                         2011.6,
                         2010.9,
                         2010.6,
                         2009.9,
                         2009.6,
                         2008.9,
                         2008.6,
                         2007.9,
                         2007.6,
                         2006.9,
                         2006.6,
                         2005.9,
                         2005.6,
                         2004.9,
                         2004.6,
                         2003.9,
                         2003.6,
                         2002.9,
                         2002.6,
                         2001.9,
                         2001.6,
                         2000.9,
                         2000.6,
                         1999.9,
                         1999.6,
                         1998.9,
                         1998.6,
                         1997.9,
                         1997.6,
                         1996.9,
                         1996.6,
                         1995.9,
                         1995.6,
                         1994.9,
                         1994.6,
                         1993.9,
                         1993.6,
                         1992.9,
                         1992.6,
                         1991.9,
                         1991.6,
                         1990.9,
                         1990.6,
                         1989.9,
                         1989.6)


Lw91<-raster("/Users/finleyroberts/Documents/spatial_analysis/landsat fire scars/Landsat_firescars/new reclassified rasters (2 areas)/3areas/lw91_3areas.tif")

num_tsf<-dir("/Users/finleyroberts/Documents/spatial_analysis/landsat fire scars/Landsat_firescars/num_TSF/new version", pattern=".tif",  full.names=T)

num_2011.9<-raster("/Users/finleyroberts/Documents/spatial_analysis/landsat fire scars/Landsat_firescars/num_TSF/new version/2011.9_num_tsf.tif")

#TSF at late dry season

j=1


for (i in seq(season_reverse_chrono)){
  earlier<-as.character(season_reverse_chrono[season_reverse_chrono<season_reverse_chrono[i]])
  tsf_stack<-stack()
  for (j in seq(earlier)){
    year_tsf<-num_tsf[grepl(substr(earlier[j],1,6),num_tsf)]
    tsf_stack<-stack(year_tsf,tsf_stack)
  }
  tsf_mosaic<-calc(tsf_stack, fun=max)
  tsf_mosaic[tsf_mosaic>0]=season_reverse_chrono[i]-tsf_mosaic[tsf_mosaic>0]
  writeRaster(tsf_mosaic, file=paste0("/Users/finleyroberts/Documents/spatial_analysis/landsat fire scars/Landsat_firescars/TSF rasters/WALFA_TSF/new version/",season_reverse_chrono[i], "_TSF_WALFA.tif"), overwrite=T)
}
