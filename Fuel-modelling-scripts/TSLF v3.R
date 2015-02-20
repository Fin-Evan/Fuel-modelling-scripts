
library(base)
library(sp)
library(maptools)
library(raster)
library(rgeos)
library(rgdal)

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
  rast_year[rast_year==1]<-years_chrono_numeric[i]+0.4
  writeRaster(rast_year,file=paste0("/Users/finleyroberts/Documents/spatial_analysis/landsat fire scars/Landsat_firescars/num_TSF/",years_chrono_numeric[i],".4_num_tsf.tif"),overwrite=T)
  
}

for(i in seq(years_chrono_numeric)){
  year_str<-substr(as.character(years_chrono_numeric[i]), 3, 4)
  year<-grep(year_str,walfa_rasters, value=TRUE)
  late_year<- year[grepl(paste0("lw",year_str),year,ignore.case=FALSE)]
  rast_year<-raster(late_year)
  rast_year[rast_year==1]<-years_chrono_numeric[i]+0.6
  writeRaster(rast_year,file=paste0("/Users/finleyroberts/Documents/spatial_analysis/landsat fire scars/Landsat_firescars/num_TSF/",years_chrono_numeric[i],".6_num_tsf.tif"),overwrite=T)
  
}


season_reverse_chrono<-c(2011.6,
                         2011.4,
                         2010.6,
                         2010.4,
                         2009.6,
                         2009.4,
                         2008.6,
                         2008.4,
                         2007.6,
                         2007.4,
                         2006.6,
                         2006.4,
                         2005.6,
                         2005.4,
                         2004.6,
                         2004.4,
                         2003.6,
                         2003.4,
                         2002.6,
                         2002.4,
                         2001.6,
                         2001.4,
                         2000.6,
                         2000.4,
                         1999.6,
                         1999.4,
                         1998.6,
                         1998.4,
                         1997.6,
                         1997.4,
                         1996.6,
                         1996.4,
                         1995.6,
                         1995.4,
                         1994.6,
                         1994.4,
                         1993.6,
                         1993.4,
                         1992.6,
                         1992.4,
                         1991.6,
                         1991.4,
                         1990.6,
                         1990.4,
                         1989.6,
                         1989.4)



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
