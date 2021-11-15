# Download catchment shapefiles from BoM website

library(readr)
library(dplyr)
library(sf)

# HRS stations
hrs = read_csv('Data/hrs_station_details.csv', skip=11)

base_url = "http://www.bom.gov.au/water/hrs/content/gis/"

dest_folder = 'Data/Shapefiles/HRS'

if(dir.exists(dest_folder) == FALSE) {
  dir.create(dest_folder, recursive=TRUE)
}

for(station in hrs$`AWRC Station Number`) {
  destfile = file.path(dest_folder, paste0(station, '.json'))
  
  if(file.exists(destfile)==FALSE) {
    download.file(paste0(base_url, paste0(station, '.json')),
                  destfile = destfile,
                  headers = c("User-Agent" = "")) 
  }
}

# Merge into single shapefile
hrs_sf_list = lapply(hrs$`AWRC Station Number`, function(station) {
  read_sf(file.path(dest_folder, paste0(station, '.json')))
})
hrs_sf = bind_rows(hrs_sf_list) %>% 
  mutate(station_id = hrs$`AWRC Station Number`)

write_sf(hrs_sf, 'Data/Shapefiles/HRS_467.gpkg', driver='gpkg')
