library(tidyverse)
library(lubridate)
library(zoo)

# HRS difference in delta S

# HRS Station Data
hrs = read_csv('Data/hrs_se_ext_attr.csv')

hrs_data = lapply(hrs$Number, function(x) {
  read_csv(paste0('Data/TimeseriesAWAPer/', x, '.csv'),
           col_types = list(Q = col_number())) %>% 
    select(Date, P, Q, AET) %>% 
    rename(AET = AET) %>% 
    mutate(Date = as_date(Date)) %>% 
    filter(Date>='1990-01-01', Date<='2018-12-31')
})
names(hrs_data) = hrs$Number


hrs_storage = lapply(hrs_data, function(x) {
  # Calculate scaling factor
  annual = x %>% 
    mutate(Year = year(Date)) %>% 
    group_by(Year) %>% 
    summarise(P   = sum(P),
              Q   = sum(Q),
              AET = sum(AET),
              .groups='drop') %>% 
    mutate(`P-Q` = P-Q,
           Fwb = `P-Q`/AET)
    
    Fwb = mean(annual$Fwb, na.rm=TRUE)
    
    # daily = x %>% 
    #   mutate(AET = AET*Fwb) %>% 
    #   mutate(deltaS = cumsum(P)-cumsum(Q)-cumsum(AET))
    
    monthly = x %>%
      mutate(Year = year(Date),
             Month = month(Date)) %>%
      group_by(Year, Month) %>%
      summarise(P   = sum(P),
                Q   = sum(Q),
                AET = sum(AET),
                .groups='drop') %>%
      mutate(Fwb = Fwb,
             AET = AET*Fwb) %>%
      mutate(deltaS = cumsum(P)-cumsum(Q)-cumsum(AET))
    
    return(monthly)
})

# saveRDS(bind_rows(hrs_storage, .id='Number'), 'Data/HRS_se_WB.Rds')

s_all = lapply(hrs_storage, function(x) x %>%
                 summarise(Smax = max(deltaS),
                           Smin = min(deltaS),
                           Smean = mean(deltaS),
                           S = Smax - Smin,
                           Fwb = unique(Fwb))) %>%
  bind_rows(.id='Number')
# 
write_csv(s_all, 'Data/Results/HRS_se_WB_AWAPer.csv')
# 
ggplot(s_all, aes(S)) +
  geom_histogram()

ggplot(s_all, aes(Fwb)) +
  geom_histogram()
