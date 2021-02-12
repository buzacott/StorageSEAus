library(tidyverse)
library(lubridate)
library(zoo)
library(lfstat)

# HRS Station Data
hrs = read_csv('Data/hrs_station_details_220.csv', skip=11) %>% 
  dplyr::rename(Number = `AWRC Station Number`)
hrsext = read_csv('Data/HRS_Ext_Attr_Combined.csv') %>% 
  rename(Number = Station_Nu)

# Filter to south east
hrs_se = hrs %>% 
  filter(Jurisdiction %in% c('ACT', 'NSW', 'VIC', 'TAS')) %>%
  left_join(., hrsext, by='Number') %>% 
  drop_na() %>% 
  filter(Number != '424002') %>% # Drop Paroo river (too large)
  filter(!Number %in% c('225020A', '307473')) %>% # Weird data
  filter(!Number %in% c('212260', '406224', '408200'))

# Filter to Q availability
stations = hrs_se$Number
maxDate = vector(length = length(stations))
qc_a = vector(length = length(stations))
qc_b = vector(length = length(stations))

for(i in 1:length(stations)) {
  Q = read_csv(paste0('Data/220HRS/', stations[i], '.csv'), skip=26)
  maxDate[i] = max(Q$Date)
  
  QCtally = Q %>%
    group_by(`Bureau QCode`) %>%
    tally() %>%
    mutate(`QC%` = n/sum(n))
  
  a = QCtally %>% filter(`Bureau QCode`=='A') %>% .$`QC%`
  b = QCtally %>% filter(`Bureau QCode`=='B') %>% .$`QC%`
  
  qc_a[i] = ifelse(length(a)>0, a, 0)
  qc_b[i] = ifelse(length(b)>0, b, 0)
}

Qfilter = tibble(stations = stations,
                 maxDate = as_date(maxDate),
                 QC_a = qc_a,
                 QC_b = qc_b) %>% 
  filter(maxDate>='2018-12-31') %>% 
  mutate(QC = QC_a + QC_b) %>% 
  filter(QC > 0.7)

hrs_se = hrs_se %>% 
  filter(Number %in% Qfilter$stations) %>% 
  select(-`Station Name`, -Latitude, -Longitude, -Jurisdiction, -`Catchment Area (km2)`, -`Data owner code`)

# Add in catchment chars
# Load in data for these stations
hrs_chars = lapply(hrs_se$Number, function(s) {
  x = read_csv( sprintf('Data/TimeseriesAWAPer/%s.csv', s) )
  lfo = as.lfobj( zoo(x %>% select(Q), x$Date), hyearstart = 7 )
  bfi = BFI(lfo)
  ar1 = pacf(x$Q, plot=F)$acf[1]
    
  y = x %>% 
    mutate(Year = year(Date)) %>% 
    group_by(Year) %>% 
    summarise(P = sum(P), 
              PET = sum(WET),
              AET = sum(AET),
              Q = sum(Q),
              .groups = 'drop') %>% 
    select(-Year) %>% 
    summarise(Qcv = var(Q),
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
names(hrs_chars) =hrs_se$Number

hrs_stats = bind_rows(hrs_chars, .id='Number')

left_join(hrs_se, hrs_stats) %>% 
  write_csv('Data/hrs_se_ext_attr.csv')
