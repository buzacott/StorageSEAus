library(tidyverse)

hrs_stations <- read_csv('Data/hrs_station_details.csv', skip = 11)

hrs_facts = vector("list")

for(station in hrs_stations$`AWRC Station Number`) {
  Sys.sleep(0.5)
  print(station)
  url <- paste('http://www.bom.gov.au/water/hrs/content/json',
               station,
               paste0(station, '_station_attributes.json'),
               sep = '/')
  r = httr::GET(url, httr::user_agent('Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/14.1.1 Safari/605.1.15'))
  
  tbl = httr::content(r, "text", encoding = 'UTF-8') %>% 
    jsonlite::fromJSON() %>% 
    lapply(function(x) x[1]) %>% 
    as_tibble()
  
  hrs_facts[[station]] = tbl
  
}

bind_rows(hrs_facts) %>% 
  write_csv('Data/hrs_station_facts.csv')


hrs_facts = read_csv('Data/hrs_facts.csv')

hrs_facts
