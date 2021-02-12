library(tidyverse)
library(lubridate)
library(zoo)

source('R/StorageDischargeFunctions.R')

# HRS Station Data
hrs = read_csv('Data/hrs_se_ext_attr.csv')

# Load in data for these stations
hrs_data = lapply(hrs$Number, function(x) {
  df = read_csv(paste0('Data/TimeseriesAWAPer/', x, '.csv'), col_types = list(Q=col_double())) %>% 
    select(Date, P, Q, AET) %>% 
    mutate(Date = as_date(Date)) %>% 
    mutate(QC_P=1) %>% 
    filter(Date>='1990-01-01', Date <= '2018-12-31') %>% 
    rename(Qmm = Q)
  
  Q_QC = read_csv(paste0('~/Dropbox (Sydney Uni)/HRS/220HRS/', x, '.csv'), skip=27,
               col_names = c('Date', 'Q', 'QC_Q')) %>% 
    mutate(QC_Q = as.integer(ifelse(QC_Q %in% c('A', 'B'), 1, 100)))
  return(left_join(df, Q_QC))
})
names(hrs_data) = hrs$Number
save(hrs_data, file='Data/hrs_data.Rda')
#------------------------------------------------------------------------------#
# Run for all
#------------------------------------------------------------------------------#
hrsSD = apply(hrs, MARGIN=1, function(x) {
  station = x['Number']
  area = as.numeric(x['area'])*86.4
  df = hrs_data[[station]] %>% 
    filter(Q > 0, QC_Q==1, QC_P==1) %>% 
    mutate(P = round(P, 2)) %>%
    filter(month(Date) > 5 & month(Date) < 9)
  sd = tryCatch({
    storageDischargeDaily_pl(df, area)
    }, error = function(e) {
    print(paste('Station', station, 'failed'))
    return(NULL)})
  return(sd)
})
names(hrsSD) = hrs$Number

# HRS Parameter sets
hrsPars = tibble(Number = names(hrsSD),
                 a = sapply(hrsSD, function(x) x$a),
                 b = sapply(hrsSD, function(x) x$b),
                 n = sapply(hrsSD, function(x) x$n),
                 r2 = sapply(hrsSD, function(x) x$r2)) %>% 
  mutate(b = if_else(is.na(b), 1, b))

# Calculate S using min and max observed Q
storagePowerLaw = bind_rows(hrs_data, .id='Number') %>%
  filter(Qmm>0, QC_Q==1) %>% 
  mutate(Year = year(Date)) %>% 
  group_by(Number, Year) %>% 
  summarise(Qmin = min(Qmm, na.rm=TRUE),
            Qavg = mean(Qmm, na.rm=TRUE),
            Qmax = max(Qmm, na.rm=TRUE),
            .groups = 'drop') %>% 
  group_by(Number) %>% 
  summarise(Qmin = mean(Qmin),
            Qavg = mean(Qavg),
            Qmax = mean(Qmax),
            .groups = 'drop') %>% 
  left_join(hrsPars, .) %>%
  rowwise() %>% 
  mutate(S = integrate(S_pl_int, Qmin, Qmax, a, b)$value,
         Smin = -integrate(S_pl_int, Qmin, Qavg, a, b)$value,
         Smax = integrate(S_pl_int, Qavg, Qmax, a, b)$value) %>% 
  arrange(desc(S))

write_csv(storagePowerLaw, 'Data/Results/HRS_se_SD_PowerLaw.csv')

hrs_fdf = lapply(hrsSD, function(x) x$fdf) %>% 
  bind_rows(.id='Number')
write_csv(hrs_fdf, 'Data/Results/HRS_se_fdf.csv')

hrs_fdfBin = lapply(hrsSD, function(x) x$fdfBin) %>% 
  bind_rows(.id='Number')
write_csv(hrs_fdfBin, 'Data/Results/HRS_se_fdfBin.csv')
