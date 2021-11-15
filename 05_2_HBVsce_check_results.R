library(tidyverse)
library(lubridate)
library(hydromad)

hbv_10x = read_csv('Results/HRS_MDB_HBV_CalResultsR2V_10x.csv')

hbv_10x_summary = hbv_10x %>% 
  select(-c(run, Message)) %>% 
  pivot_longer(-c(station_id), names_to = 'par') %>% 
  group_by(station_id, par) %>% 
  summarise(mean = mean(value),
            sd = sd(value)) %>% 
  pivot_longer(c(mean, sd)) %>% 
  pivot_wider(names_from = par)

  
hbv_10x %>% 
  select(-c(run, Message)) %>% 
  pivot_longer(-c(station_id), names_to = 'par') %>% 
  group_by(station_id, par) %>% 
  summarise(mean = mean(value),
            sd = sd(value)) %>% 
  arrange(desc(sd))

hbv_10x %>% 
  select(station_id, R2V) %>% 
  pivot_longer(-c(station_id), names_to = 'par') %>% 
  group_by(station_id, par) %>% 
  summarise(mean = mean(value),
            sd = sd(value)) %>% 
  arrange(desc(sd))



# Check uncertainty of storages
# HRS stations
r = apply(runlist, 1, function(x) {

  run = x[['run']]
  station = x[['station']]
  
  fname = file.path('Data', 'HBVResultsR2V', run, paste0(station, '.Rds'))
  fitR2V = readRDS(fname)
  
  out = bind_rows(broom::tidy(fitR2V$U), broom::tidy(fitR2V$fitted.values)) %>% 
    mutate(year = year(index)) %>% 
    filter(year>1990) %>% 
    group_by(series, year) %>% 
    summarise(max = max(value),
              min = min(value),
              .groups = 'drop') %>% 
    mutate(run = run,
           station_id = station)
  
  return(out)
})

hbv_run_storages = bind_rows(r) %>% 
  filter(series %in% c('SM', 'SLZ', 'SUZ', 'Snow')) %>% 
  group_by(run, station_id, series) %>% 
  summarise(max = max(max), .groups='drop') %>% 
  pivot_wider(names_from=series, values_from=max) %>% 
  mutate(#GW = SLZ + SUZ,
    S = Snow + SM + SUZ + SLZ) %>% 
  dplyr::select(run, station_id, Snow, SM, SUZ, SLZ, S)

write_csv(hbv_run_storages, 'Results/HRS_MDB_HBV_Storages_10x.csv')

hbv_run_storages %>% 
  pivot_longer(-c(run, station_id)) %>% 
  mutate(name = factor(name, levels = c('Snow', 'SM', 'SUZ', 'SLZ', 'S'))) %>% 
  ggplot(aes(name, value)) +
  geom_boxplot() +
  facet_wrap(~station_id)

# Average the timeseries
r = apply(hbv_10x, 1, function(x) {
  
  run = x[['run']]
  station = x[['station_id']]
  
  fname = file.path('Data', 'HBVResultsR2V', run, paste0(station, '.Rds'))
  fitR2V = readRDS(fname)
  
  out = bind_rows(broom::tidy(fitR2V$U), broom::tidy(fitR2V$fitted.values)) %>% 
    mutate(station_id = station,
           run = run)
  
  return(out)
})


hbv_storages_avgd = bind_rows(r) %>% 
  group_by(station_id, series, index) %>% 
  summarise(value = mean(value)) %>% 
  group_by(station_id, series) %>% 
  summarise(value = max(value)) %>% 
  filter(series %in% c('Snow', 'SM', 'SUZ', 'SLZ')) %>% 
  pivot_wider(names_from = series, values_from = value)

hbv_storages_avgd %>% 
  arrange(desc(SLZ))


# Calculate averages of states
r_avg = apply(runlist, 1, function(x) {
  
  run = x[['run']]
  station = x[['station']]
  
  fname = file.path('Data', 'HBVResultsR2V', run, paste0(station, '.Rds'))
  fitR2V = readRDS(fname)
  
  out = bind_rows(broom::tidy(fitR2V$U), broom::tidy(fitR2V$fitted.values)) %>% 
    group_by(series) %>% 
    summarise(max = max(value),
              min = min(value),
              mean = mean(value),
              median = median(value),
              sd = sd(value),
              .groups = 'drop') %>% 
    mutate(run = run,
           station_id = station)
  
  return(out)
})

sm_avg = bind_rows(r_avg) %>% 
  filter(series %in% c('SM', 'SLZ', 'SUZ', 'Snow')) %>% 
  group_by(station_id, series) %>% 
  summarise(max = mean(max),
            min = mean(min),
            mean = mean(mean),
            median = mean(median),
            sd = mean(sd)) %>% 
  filter(series == 'SM')
  
summary(sm_avg$mean)
summary(sm_avg$median)

hbv_run_storages %>% 
  group_by(station_id) %>% 
  summarise(across(Snow:S, .fns = list(mean = mean, sd=sd))) %>% 
  arrange(desc(S_sd))