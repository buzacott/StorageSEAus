library(AWAPer)
library(dplyr)
library(raster)
library(sf)

setwd('/mnt/Data/AWAPer')

startDate = as.Date("1990-01-01","%Y-%m-%d")
endDate = as.Date("2018-12-31","%Y-%m-%d")

data(constants,package='Evapotranspiration')

# McMahon 2013 / Chiew 2003 constants for Aus
constants$b0 = 1.0
constants$fz = 29.2
constants$b1 = 13.4
constants$b2 = 1.13

hrs_sf = read_sf('~/Dropbox (Sydney Uni)/HRS/Shapefile/HRS_SE.shp')
hrs_sf = hrs_sf %>% dplyr::select(Station_Nu) %>% dplyr::rename(Number = Station_Nu)
hrs_sp = as_Spatial(hrs_sf)

DEM_9s = raster('DEM9s.tif')

morton_pet = extractCatchmentData(ncdfFilename='AWAP_1990_2018.nc',
                                  ncdfSolarFilename='AWAP_1990_2018_solar.nc',
                                  extractFrom=startDate,
                                  extractTo=endDate,
                                  catchments=hrs_sp,
                                  DEM=DEM_9s,
                                  ET.constants=constants,
                                  ET.function='ET.MortonCRAE',
                                  ET.Mortons.est='potential ET',
                                  ET.timestep='monthly')

morton_wet = extractCatchmentData(ncdfFilename='AWAP_1990_2018.nc',
                                  ncdfSolarFilename='AWAP_1990_2018_solar.nc',
                                  extractFrom=startDate,
                                  extractTo=endDate,
                                  catchments=hrs_sp,
                                  DEM=DEM_9s,
                                  ET.constants=constants,
                                  ET.function='ET.MortonCRAE',
                                  ET.Mortons.est='wet areal ET',
                                  ET.timestep='monthly')

morton_aet = extractCatchmentData(ncdfFilename='AWAP_1990_2018.nc',
                                  ncdfSolarFilename='AWAP_1990_2018_solar.nc',
                                  extractFrom=startDate,
                                  extractTo=endDate,
                                  catchments=hrs_sp,
                                  DEM=DEM_9s,
                                  ET.constants=constants,
                                  ET.function='ET.MortonCRAE',
                                  ET.Mortons.est='actual areal ET',
                                  ET.timestep='monthly')

rm('DEM_9s')

save.image('HRS_AWAP_Extraction.Rdata')
