#------------------------------------------------------------------------------#
# Join meteorological data and discharge data
#------------------------------------------------------------------------------#

library(tidyverse)

# HRS stations in the MDB
hrs_mdb = read_csv('Data/hrs_mdb_station_ext_attrs.csv')
# 
hrs_station_details = read_csv('Data/hrs_station_details.csv', skip=11) %>% 
  select(station_id = `AWRC Station Number`,
         area = `Catchment Area (km2)`) %>% 
  filter(station_id %in% hrs_mdb$station_id)

# Iterate over stations and join selected met and Q data
dest_dir = file.path('Data', 'Timeseries')
if(dir.exists(dest_dir) == FALSE) dir.create(dest_dir)

for(i in seq_len(nrow(hrs_station_details))) {
  station = hrs_station_details$station_id[i]
  area = hrs_station_details$area[i]
  
  met = read_csv(file.path('Data', 'SILO', paste0(station, '.csv'))) %>% 
    select(-station_id)
  Q = read_csv(file.path('Data', 'HRS', paste0(station, '.csv')),
               skip = 26,
               show_col_types = FALSE) %>% 
    filter(Date >= '1990-01-01', Date <= '2018-12-31') %>% 
    dplyr::rename(Q = `Flow (ML)`,
                  QC = `Bureau QCode`) %>% 
    # ML/d to mm/day and recode character QC to int
    mutate(Q = Q / area,
           QC = recode(QC,
                       'A' = 1, 'B' = 2, 'C' = 3, 'D' = 4,
                       'E' = 5, 'F' = 6, 'G' = 7))
  df = left_join(met, Q)
  
  if(any(is.na(df))) {
    stop(paste('NAs in', station))
  }
  #write_csv(df, file.path(dest_dir, paste0(station, '.csv')))
}


# Calculate extra metrics
# Load in data for these stations
hrs_data = lapply(hrs_mdb$station_id, function(station) {
  read_csv(file.path('Data', 'Timeseries', paste0(station, '.csv'))) %>% 
    dplyr::rename(QC_Q = QC)
})
names(hrs_data) = hrs_mdb$station_id

library(zoo)
library(lfstat)

hrs_data_stats = lapply(hrs_data, function(x) {
  lfo = as.lfobj( zoo(x %>% select(Q), x$Date), hyearstart = 7 )
  bfi = BFI(lfo)
  ar1 = pacf(x$Q, plot=F)$acf[1]
  
  y = x %>% 
    mutate(Year = lubridate::year(Date)) %>% 
    group_by(Year) %>% 
    summarise(P = sum(P), 
              PET = sum(PET),
              AET = sum(AET),
              Q = sum(Q),
              .groups = 'drop') %>% 
    select(-Year) %>% 
    summarise(Qcv = sd(Q)/mean(Q),
              P = mean(P),
              Q = mean(Q),
              PET = mean(PET),
              AET = mean(AET),
              `P/PET` = mean(P/PET),
              `Q/P` = mean(Q/P)) %>% 
    mutate(BFI = bfi,
           AR1 = ar1)
  
  return(y)
})
names(hrs_data_stats) = hrs_mdb$station_id

hrs_data_stats = bind_rows(hrs_data_stats, .id='station_id')

hrs_mdb %>% 
  select(-colnames(hrs_data_stats)[-1])

left_join(hrs_mdb %>% 
            select(-colnames(hrs_data_stats)[-1]), hrs_data_stats) %>% 
  write_csv('Data/hrs_mdb_station_ext_attrs.csv')
