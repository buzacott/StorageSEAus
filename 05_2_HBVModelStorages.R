library(tidyverse)
library(hydromad)
library(lubridate)

hrs = read_csv('Data/hrs_se_ext_attr.csv')

results = read_csv('Data/Results/HBV_se_HBVCalResultsR2V.csv') %>% 
  drop_na() %>% 
  filter(Number %in% hrs$Number)

hbvData = lapply(results$Number, function(x) {
  print(x)
  fit = readRDS(paste0('Data/HBVResultsR2V/', x, '.Rds'))
  
  out = bind_rows(broom::tidy(fit$U), broom::tidy(fit$fitted.values)) %>% 
    mutate(year = year(index)) %>% 
    filter(year>1990) %>% 
    group_by(series, year) %>% 
    summarise(max = max(value),
              min = min(value),
              .groups = 'drop')
  
  return(out)
})
names(hbvData) = results$Number

dynStor = bind_rows(hbvData, .id='Number') %>% 
  filter(series %in% c('SM', 'SLZ', 'SUZ')) %>% 
  group_by(Number, year) %>% 
  summarise(max = sum(max),
            min = sum(min)) %>% 
  mutate(S = max-min) %>% 
  group_by(Number) %>% 
  summarise(S = mean(S))

write_csv(dynStor, 'Data/Results/HRS_se_HBVDynamicStorage.csv')

extDynStor = bind_rows(hbvData, .id='Number') %>% 
  filter(series %in% c('SM', 'SLZ', 'SUZ', 'Snow')) %>% 
  group_by(Number, series) %>% 
  summarise(max = max(max), .groups='drop') %>% 
  pivot_wider(names_from=series, values_from=max) %>% 
  mutate(GW = SLZ + SUZ,
         S = GW + Snow + SM) %>% 
  dplyr::rename(Soil = SM) %>% 
  select(Number, GW, Soil, Snow, S)

write_csv(extDynStor, 'Data/Results/HRS_se_HBVExtDynamicStorage.csv')
