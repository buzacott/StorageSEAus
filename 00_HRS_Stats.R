# HRS extended attributes

library(tidyverse)
library(sf)

hrs = read_csv('Data/hrs_station_details_220.csv', skip=11)

s = read_sf('Data/Shapefile/HRS_220.shp')

ext_attr = read_csv('Data/Tables/HRS_ExtAttr.csv') %>% 
  mutate(CAT_SEDIMENTARY = CAT_SILICSED + CAT_CARBNATESED + CAT_OTHERSED) %>% 
  select(station_no, STRDENSITY, RCHLEN, CATSLOPE, CATSTORAGE, CAT_A_KSAT,
         CAT_IGNEOUS, CAT_SEDIMENTARY, #CAT_SILICSED, CAT_CARBNATESED, CAT_OTHERSED,
         CAT_METAMORPH, CAT_SEDVOLC, CAT_OLDROCK, CAT_UNCONSOLDTED) %>%
  rename(Station_Nu = station_no,
         StreamDensity = STRDENSITY,
         StreamLength = RCHLEN,
         Slope = CATSLOPE,
         CatStorage = CATSTORAGE,
         KSat = CAT_A_KSAT,
         Igneous = CAT_IGNEOUS,
         Sedimentary = CAT_SEDIMENTARY,
         Metamorphic = CAT_METAMORPH,
         SedVolc = CAT_SEDVOLC,
         OldRock = CAT_OLDROCK,
         UnconsolRock = CAT_UNCONSOLDTED)
  

dem = read_csv('Data/Tables/exactextract/HRS_SE_DEM.csv')
pawc = read_csv('Data/Tables/exactextract/HRS_SE_PAWC.csv')
soil_depth = read_csv('Data/Tables/exactextract/HRS_SE_SoilDepth.csv')
regolith_depth = read_csv('Data/Tables/exactextract/HRS_SE_RegolithDepth.csv')
woody_cover = read_csv('Data/Tables/exactextract/HRS_SE_WoodyCover.csv') %>% 
  mutate(WoodyCover_mean = WoodyCover_mean*100)
woody_intensity = read_csv('Data/Tables/exactextract/HRS_SE_WoodyIntensity.csv')
awc = read_csv('Data/Tables/exactextract/HRS_SE_AWC.csv') %>% 
  mutate(mean = mean*10) # cm to mm/m
clay = read_csv('Data/Tables/exactextract/HRS_SE_Clay.csv')
silica = read_csv('Data/Tables/exactextract/HRS_SE_Silica.csv')

tbl = tibble(Station_Nu = hrs$`AWRC Station Number`,
             latitude = hrs$Latitude,
             longitude = hrs$Longitude,
             area = hrs$`Catchment Area (km2)`) %>% 
  left_join(dem %>% select(Station_Nu, mean, range) %>% rename(DEM_Mean = mean, DEM_Range = range)) %>% 
  left_join(pawc %>% select(Station_Nu, pawc_mean) %>% rename(PAWC = pawc_mean)) %>%
  left_join(soil_depth %>% select(Station_Nu, SoilDepth_mean) %>% rename(SoilDepth = SoilDepth_mean)) %>% 
  left_join(regolith_depth %>% select(Station_Nu, RegolithDepth_mean) %>% rename(RegolithDepth = RegolithDepth_mean)) %>% 
  left_join(woody_cover %>% select(Station_Nu, WoodyCover_mean) %>% rename(ForestCover = WoodyCover_mean)) %>% 
  left_join(woody_intensity %>% select(Station_Nu, WoodyIntensity_mean) %>% rename(FoliageCover = WoodyIntensity_mean)) %>% 
  left_join(awc %>% select(Station_Nu, mean) %>% rename(AWC = mean)) %>% 
  left_join(clay %>% select(Station_Nu, mean) %>% rename(Clay = mean)) %>% 
  left_join(silica %>% rename(Silica = mean, Station_Nu = Number) %>% select(Station_Nu, Silica)) %>%  
  left_join(ext_attr) %>% 
  select(Station_Nu, latitude, longitude, area, DEM_Mean, DEM_Range,
         Slope, StreamLength, StreamDensity, CatStorage, RegolithDepth,
         SoilDepth, Clay, KSat, AWC, PAWC, 
         ForestCover, FoliageCover, Igneous, Sedimentary, Metamorphic,
         SedVolc, OldRock, UnconsolRock, Silica)

# Some mismatch between shapefile and hrs index
tbl = tbl %>% filter(!is.na(DEM_Mean))

write_csv(tbl, 'Data/HRS_Ext_Attr_Combined.csv')