
hrs_mdb = sf::read_sf('Data/Shapefiles/HRS_MDB_Catchments_Filtered.gpkg')

foliage = raster::raster('~/Dropbox (Sydney Uni)/Data/AuscoverWoodyCover/AuscoverWoodyCover2000_2010_500m.tif')
foliage = raster::projectRaster(foliage, 4283)
foliage_mdb = exactextractr::exact_extract(foliage, hrs_mdb,  c('mean', 'stdev', 'min', 'max')) * 100

foliage_mdb %>% 
  as_tibble() %>% 
  rename_with(~paste('WoodyIntensity', .x, sep='_')) %>% 
  mutate(station_id = hrs_mdb$station_id) %>% 
  select(station_id, everything()) %>% 
  write_csv('Data/ExactExtract/WoodyIntensity.csv')

forest = raster::raster('~/Dropbox (Sydney Uni)/Data/AuscoverWoodyCover/AuscoverWoodyCover2000_2010_500m_Binary.tif')
forest_mdb = exactextractr::exact_extract(forest, hrs_mdb, c('mean', 'stdev', 'min', 'max')) * 100
forest_mdb %>% 
  as_tibble() %>% 
  rename_with(~paste('WoodyCover', .x, sep='_')) %>% 
  mutate(station_id = hrs_mdb$station_id) %>% 
  select(station_id, everything()) %>% 
  write_csv('Data/ExactExtract/WoodyCover.csv')

foliage_mdb %>% 
  as_tibble() %>% 
  rename_with(~paste('WoodyIntensity', .x, sep='_')) %>% 
  mutate(station_id = hrs_mdb$station_id) %>% 
  select(station_id, everything()) %>% 
  write_csv('Data/ExactExtract/WoodyIntensity.csv')


summary(foliage_mdb)

silica = raster::raster('~/Dropbox (Sydney Uni)/Data/silica_WGS84.tif')
silica = raster::projectRaster(silica, 4283)

silica_mdb = exactextractr::exact_extract(silica, hrs_mdb, c('mean', 'stdev', 'min', 'max'))


silica_mdb %>% 
  as_tibble() %>% 
  rename_with(~paste('silica', .x, sep='_')) %>% 
  mutate(station_id = hrs_mdb$station_id) %>% 
  select(station_id, everything()) %>% 
  write_csv('Data/ExactExtract/silica.csv')
