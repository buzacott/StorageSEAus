#------------------------------------------------------------------------------#
# Filter stations that are within the MDB
#------------------------------------------------------------------------------#
# This script selects HRS stations that are within the Murray-Darling Basin and
# meet certain criteria about timeseries length and availability of catchment
# characteristics from the Geofabric

library(tidyverse)
library(sf)

# Read in MDB HRS catchment shapefile
# Remove catchments with invalid polygons
mdb_hrs_catchments = read_sf('Data/Shapefiles/HRS_MDB_Catchments.gpkg') %>% 
  rowwise() %>% 
  mutate(Area = as.numeric(tryCatch(st_area(geom), error = function(e) return(NA))) / 10^6) %>% 
  filter(!is.na(Area)) %>% 
  select(-Area) %>% 
  ungroup() %>% 
  # Remove Paroo river (too large) %>% 
  filter(!station_id %in% c('424002', '424201A'))

# MDB Ghost Nodes
# Ghost nodes are AWRC stations that are present within Geofabric
mdb_hrs_gn = read_sf('Data/Shapefiles/MDB_HRS_GhostNodes.gpkg')

# Remove duplicate station numbers
mdb_hrs_gn = mdb_hrs_gn %>% 
  group_by(STATIONNO) %>% 
  mutate(n = n()) %>% 
  filter(n == 1) %>% 
  select(-n) %>% 
  ungroup()

# Read in the attributes that have been exported from the Geofabric using QGIS
mdb_hrs_attr = read_sf('Data/Shapefiles/SH_Catchment_MDB_HRS_Attr.gpkg')

# Match the ghost nodes to the extracted catchment points that have the attrs
mdb_hrs_gn_attr = st_intersection(mdb_hrs_gn, mdb_hrs_attr) 

# Match the ghost nodes to the catchment polygons
mdb_hrs_catchments_attr = mdb_hrs_catchments %>%
  left_join(st_drop_geometry(mdb_hrs_gn_attr), by = c('station_id' = 'STATIONNO')) %>% 
  filter(!is.na(HydroID)) %>% 
  # Fix up column names and remove unnecessary columns
  select(station_id, StreamName, contains(c('_CAT', 'RCHLEN', 'STRDENSITY'))) %>% 
  select(-contains('run71_00')) %>% 
  rename_with(~ str_split_fixed(.x, '_lut_', 2)[,2], contains('_lut_')) %>% 
  rename_with(~ str_replace(.x, 'CAT+_?+', '')) %>% 
  mutate(StreamName = str_to_title(StreamName)) %>% 
  select(station_id,
         stream_name = StreamName,
         Area = AREA,
         Slope = SLOPE,
         Relief = RELIEF,
         PVB = STORAGE,
         SnowQ = SNOW, # % of annual Q from above the snowline
         StreamDensity = STRDENSITY,
         StreamLength = RCHLEN,
         Unconsolidated = UNCONSOLDTED,
         Igneous = IGNEOUS,
         SilicSed = SILICSED,
         CarbonateSed = CARBNATESED,
         OtherSed = OTHERSED,
         Metamorphic = METAMORPH,
         SedVolc = SEDVOLC,
         OldRock = OLDROCK) # Modified land cover

# Make sure stations cover the start and end dates
hrs_facts = read_csv('Data/hrs_facts.csv') %>% 
  filter(as_date(start_date, format = '%d-%m-%Y') <= as_date('1990-01-01'),
         as_date(end_date, format = '%d-%m-%Y') >= as_date('2018-12-31'))

# Find Stream QC information
hrs_QC = lapply(mdb_hrs_catchments_attr$station_id, function(x) {
  read_csv(file.path('Data', 'HRS', paste0(x, '.csv')), skip=26,
           show_col_types = FALSE) %>% 
    group_by(`Bureau QCode`) %>% 
    tally() %>% 
    summarise(QC = sum(n[`Bureau QCode` %in% c('A', 'B')]) / sum(n))
})
names(hrs_QC) = mdb_hrs_catchments_attr$station_id

# Stations need to have 70% of QC codes in B or higher
hrs_QC = hrs_QC %>%
  bind_rows(.id = 'station_id') %>% 
  filter(QC >= 0.7)

mdb_hrs_catchments_attr = mdb_hrs_catchments_attr %>% 
  filter(station_id %in% hrs_facts$awrc,
         station_id %in% hrs_QC$station_id)

nrow(mdb_hrs_catchments_attr)
# 69 stations

# Write the filtered stations to a GeoPackage to use with exactextract
mdb_hrs_catchments_attr %>% 
  select(station_id, stream_name, geom) %>% 
  write_sf('Data/Shapefiles/HRS_MDB_Catchments_Filtered.gpkg')

# Read in external characteristics from exact extract
dem =  read_csv('Data/ExactExtract/DEM.csv') %>% 
  mutate(elev_range = DEM_max - DEM_min) %>% 
  select(station_id,
         elev_mean = DEM_mean,
         elev_range)
clay = read_csv('Data/ExactExtract/Clay1m.csv') %>% 
  select(station_id, clay=Clay)
silica = read_csv('Data/ExactExtract/silica.csv') %>% 
  select(station_id,
         Silica = silica_mean)
woody_cover = read_csv('Data/ExactExtract/WoodyCover.csv') %>% 
  select(station_id,
         woody_cover = WoodyCover_mean)
woody_intensity = read_csv('Data/ExactExtract/WoodyIntensity.csv') %>% 
  select(station_id,
         woody_intensity = WoodyIntensity_mean)
soil_depth = read_csv('Data/ExactExtract/SoilRegolithDepth.csv') %>% 
  select(station_id, soil_depth, regolith_depth)

mdb_hrs_catchments_attr = left_join(
  mdb_hrs_catchments_attr,
  purrr::reduce(list(dem,clay, woody_cover, woody_intensity,
                     silica, soil_depth), left_join)
)

mdb_hrs_catchments_attr %>% 
  st_drop_geometry() %>% 
  write_csv('Data/hrs_mdb_station_ext_attrs.csv')

mdb_hrs_gn %>% 
  filter(STATIONNO %in% mdb_hrs_catchments_attr$station_id) %>% 
  write_sf('Data/Shapefiles/HRS_MDB_Gauges_Filtered.gpkg')
