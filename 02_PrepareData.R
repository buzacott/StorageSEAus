library(tidyverse)
library(lubridate)
library(Evapotranspiration)
library(zoo)

load('Data/HRS_AWAP_Extraction.Rdata')

hrs = read_csv('Data/hrs_se_ext_attr.csv')

df = morton_aet$catchmentTemporal.mean %>% 
  as_tibble() %>% 
  mutate(Date = ymd(paste(year, month, day, sep='-'))) %>% 
  rename(P = precip_mm,
         VP = vprp,
         Rad = solarrad_interp,
         AET = ET_mm) %>% 
  mutate(PET = morton_pet$catchmentTemporal.mean$ET_mm,
         WET = morton_wet$catchmentTemporal.mean$ET_mm)

awaper = apply(hrs, MARGIN=1, function(x) {
  station = x['Number']
  area = as.numeric(x['area'])
  
  Q = read_csv(paste0('~/Dropbox (Sydney Uni)/HRS/220HRS/', station, '.csv'), skip=26) %>% 
    mutate(Q = `Flow (ML)`/area) %>% 
    select(Date, Q)
  
  station_df = df %>% 
    filter(Number == station) %>% 
    left_join(Q) %>% 
    select(-Number) %>% 
    select(Date, P, Q, PET, WET, AET, Tmin, Tmax, VP, Rad)
  
  write_csv(station_df, paste0('Data/TimeseriesAWAPer/', station, '.csv'))
})