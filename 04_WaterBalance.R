library(tidyverse)
library(lubridate)

# HRS difference in delta S

# HRS Station Data
hrs = read_csv('Data/hrs_mdb_station_ext_attrs.csv')
hrs_facts = read_csv('Data/hrs_facts.csv') %>% 
  filter(awrc %in% hrs$station_id)
hrs = left_join(hrs,
                hrs_facts %>% select(awrc, area), by=c('station_id' = 'awrc'))

hrs_details = read_csv('Data/hrs_station_details.csv', skip=11) %>% 
  rename(station_id = `AWRC Station Number`) %>% 
  select(station_id, Jurisdiction)

# Load in data for these stations
hrs_data = lapply(hrs$station_id, function(station) {
  read_csv(file.path('Data', 'Timeseries', paste0(station, '.csv')),
           show_col_types = FALSE)
})
names(hrs_data) = hrs$station_id

hrs_wb = lapply(hrs_data, function(x) {
  # Calculate scaling factor
  scaling = x %>% 
    mutate(Year = year(Date)) %>% 
    select(Year, P, Q, AET) %>% 
    pivot_longer(-Year) %>% 
    group_by(Year, name) %>% 
    summarise(value = sum(value), .groups='drop') %>% 
    group_by(name) %>% 
    summarise(Mean = mean(value),
              SD = sd(value),
              n = n()) %>% 
    mutate(LCI = Mean - qt(0.975, n-1)*(SD/sqrt(n)),
           UCI = Mean + qt(0.975, n-1)*(SD/sqrt(n)))
  
  sf = scaling %>% 
    dplyr::rename(Par=name) %>% 
    select(Par, Mean, LCI, UCI) %>% 
    pivot_longer(-Par, names_to = 'CI') %>% 
    pivot_wider(names_from=Par) %>% 
    mutate(sf = (P-Q) / AET) %>% 
    select(CI, sf)
  
  sfw = sf %>%  
    pivot_wider(names_from='CI', values_from='sf')
  
  # s_wb =  x %>%
  #   mutate(Year = year(Date),
  #          Month = month(Date)) %>%
  #   group_by(Year, Month) %>%
  #   summarise(Date = min(Date),
  #             P   = sum(P),
  #             Q   = sum(Q),
  #             AET = sum(AET),
  #             .groups='drop') %>% 
  #   select(-c(Year, Month)) %>% 
  #   mutate(LCI = AET*sfw$LCI,
  #          Mean = AET*sfw$Mean,
  #          UCI = AET*sfw$UCI) %>% 
  #   pivot_longer(-c(Date, P, Q, AET), names_to = 'CI', values_to = 'AET_sf') %>% 
  #   group_by(CI) %>% 
  #   mutate(dS = cumsum(P) - cumsum(Q) - cumsum(AET_sf)) %>% 
  #   left_join(sf)
  
  s_wb =  x %>% 
    select(Date, P, Q, AET) %>% 
    mutate(LCI = AET*sfw$LCI,
           Mean = AET*sfw$Mean,
           UCI = AET*sfw$UCI) %>% 
    pivot_longer(-c(Date, P, Q, AET), names_to = 'CI', values_to = 'AET_sf') %>% 
    group_by(CI) %>% 
    mutate(dS = cumsum(P) - cumsum(Q) - cumsum(AET_sf)) %>% 
    left_join(sf)
  
    return(s_wb)
})

saveRDS(hrs_wb, 'Results/HRS_MDB_WB.Rds')

wb_df = bind_rows(hrs_wb, .id = 'station_id')

ggplot(wb_df, aes(Date, dS, col=CI)) +
  geom_line() +
  facet_wrap(~station_id, scales='free_y')

s_wb_df = wb_df %>% 
  group_by(station_id, CI) %>% 
  summarise(Smax = max(dS),
            Smin = min(dS),
            Smean = mean(dS),
            sf = sf[1],
            S = Smax - Smin,
            .groups = 'drop')
  
tapply(s_wb_df$S, s_wb_df$CI, summary)

ggplot(s_wb_df, aes(S)) +
  geom_histogram() +
  facet_wrap(~CI)

write_csv(s_wb_df, 'Results/HRS_MDB_WB.csv')
