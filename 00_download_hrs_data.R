#------------------------------------------------------------------------------#
# Download the HRS station data from the BoM
#------------------------------------------------------------------------------#
library(tidyverse)

hrs = read_csv('Data/hrs_station_details.csv', skip=11, header=TRUE)
stations = hrs$`AWRC Station Number`

# Download HRS data
base_url = 'http://www.bom.gov.au/water/hrs/content/data'

dest = file.path('Data', 'HRSQ')
if(dir.exists(dest) == FALSE) {
  dir.create(dest, recursive = TRUE)
}

for(station in stations) {
  destfile = file.path(dest, paste0(station, '.csv'))
  if(file.exists(destfile) == FALSE) {
    Sys.sleep(0.5)
    download.file(
      paste(base_url, station, paste0(station, '_daily_ts.csv'), sep = '/'),
      destfile = destfile,
      headers = c("User-Agent" = "")
    )
  }
}

# Download HRS facts data
hrs_facts = vector("list", length(stations))
names(hrs_facts) = stations
for(station in stations) {
  Sys.sleep(0.5)
  print(station)
  url <- paste('http://www.bom.gov.au/water/hrs/content/json',
               station,
               paste0(station, '_station_attributes.json'),
               sep = '/')
  r = httr::GET(url, httr::user_agent(''))
  
  tbl = httr::content(r, "text", encoding = 'UTF-8') %>% 
    jsonlite::fromJSON() %>% 
    lapply(function(x) x[1]) %>% 
    as_tibble()
  
  hrs_facts[[station]] = tbl
  
}
hrs_facts = bind_rows(hrs_facts)
write_csv('Data/hrs_facts.csv')

