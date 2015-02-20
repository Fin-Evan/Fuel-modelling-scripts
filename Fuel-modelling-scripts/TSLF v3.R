
library(base)
library(sp)
library(maptools)
library(raster)
library(rgeos)
library(rgdal)

areas3raster<-raster("/Users/finleyroberts/Documents/spatial_analysis/3 areas new raster/3areas_ras.tif")


#resampling to 3 areas raster
for(i in seq(WALFA_firescars)){
  r<-raster(WALFA_firescars[i])
  r[is.na(r)]<-0
  r[r>1]<-1
  r<-mask(resample(r,WALFA_boundary_raster, method="ngb"),WALFA_boundary_raster, file=paste0("/Users/finleyroberts/Documents/spatial_analysis/landsat fire scars/Landsat_firescars/new reclassified rasters/3areas",WALFA_firescars[i],"_3areas.tif"),overwrite=TRUE)
  names(r)<-WALFA_firescars[i]
  
  assign(WALFA_firescars[i],r)
  
}



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

for(i in seq(years_chrono_numeric)){
  year_str<-substr(as.character(years_chrono_numeric[i]), 3, 4)
  year<-grep(year_str,walfa_rasters, value=TRUE)
  early_year<- year[grepl(paste0("ew",year_str),year,ignore.case=FALSE)]
  rast_year<-raster(early_year)
  rast_year[rast_year==1]<-years_chrono_numeric[i]+0.6
  writeRaster(rast_year,file=paste0("/Users/finleyroberts/Documents/spatial_analysis/landsat fire scars/Landsat_firescars/num_TSF/",years_chrono_numeric[i],".4_num_tsf.tif"),overwrite=T)
  
}

for(i in seq(years_chrono_numeric)){
  year_str<-substr(as.character(years_chrono_numeric[i]), 3, 4)
  year<-grep(year_str,walfa_rasters, value=TRUE)
  late_year<- year[grepl(paste0("lw",year_str),year,ignore.case=FALSE)]
  rast_year<-raster(late_year)
  rast_year[rast_year==1]<-years_chrono_numeric[i]+0.9
  writeRaster(rast_year,file=paste0("/Users/finleyroberts/Documents/spatial_analysis/landsat fire scars/Landsat_firescars/num_TSF/",years_chrono_numeric[i],".6_num_tsf.tif"),overwrite=T)
  
}


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



num_tsf<-dir("/Users/finleyroberts/Documents/spatial_analysis/landsat fire scars/Landsat_firescars/num_TSF", pattern=".tif",  full.names=T)

#TSF at late dry season

for (i in seq(season_reverse_chrono)){
  earlier<-as.character(season_reverse_chrono[season_reverse_chrono<season_reverse_chrono[i]])
  tsf_stack<-stack()
  for (j in seq(earlier)){
    year_tsf<-num_tsf[grepl(substr(earlier[j],1,6),num_tsf)]
    tsf_stack<-stack(year_tsf,tsf_stack)
  }
  tsf_mosaic<-calc(tsf_stack, fun=max)
  tsf_mosaic[tsf_mosaic>0]=season_reverse_chrono[i]-tsf_mosaic[tsf_mosaic>0]
  writeRaster(tsf_mosaic, file=paste0("/Users/finleyroberts/Documents/spatial_analysis/landsat fire scars/Landsat_firescars/TSF rasters/WALFA_TSF/",season_reverse_chrono[i], "_TSF_WALFA.tif"), overwrite=T)
}
