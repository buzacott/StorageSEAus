library(tidyverse)
library(lubridate)
# library(zoo)

# HRS Station Data
hrs = read_csv('Data/hrs_mdb_station_ext_attrs.csv')

# Load in data for these stations
hrs_data = lapply(hrs$station_id, function(station) {
  read_csv(file.path('Data', 'Timeseries', paste0(station, '.csv'))) %>% 
    dplyr::rename(QC_Q = QC)
})
names(hrs_data) = hrs$station_id
saveRDS(hrs_data, file='Data/hrs_data.Rds')

#------------------------------------------------------------------------------#
# Run for all
#------------------------------------------------------------------------------#
source('R/StorageDischargeFunctions.R')
hrsSD = apply(hrs, MARGIN=1, function(x) {
  station = x[['station_id']]
  
  df = hrs_data[[station]] %>% 
    filter(Q > 0, QC_Q <= 2) %>% 
    mutate(P = round(P, 1), QC_P = 1) %>% # SILO only has data to nearest 0.1 mm
    filter(month(Date) > 5 & month(Date) < 9)
  
  if(station == '422202B') {
    # data point prevents fitting
    df = df %>% filter(!Date %in% as.Date(c('1998-08-31', '2005-07-03')))
  }

  sd = storageDischargeDaily_pl(df)
  if(is.na(sd$a)) print(paste('Station', station, 'failed'))
  
  return(sd)
})
names(hrsSD) = hrs$station_id

# HRS Parameter sets

hrsPars = lapply(hrsSD, function(x) {
  unlist(x[c('a', 'a_std', 'b', 'b_std', 'n', 'r2')])
})  %>% 
  bind_rows('.id' = 'station_id') %>% 
  mutate(b = if_else(is.na(b), 1, b))

# Calculate S using min and max observed Q
storagePowerLaw = bind_rows(hrs_data, .id='station_id') %>%
  filter(station_id %in% hrsPars$station_id) %>% 
  filter(Q > 0, QC_Q <= 2) %>% 
  mutate(Year = year(Date)) %>% 
  group_by(station_id, Year) %>% 
  summarise(Qmin = min(Q, na.rm=TRUE),
            Qavg = mean(Q, na.rm=TRUE),
            Qmax = max(Q, na.rm=TRUE),
            .groups = 'drop') %>% 
  group_by(station_id) %>% 
  summarise(Qmin = mean(Qmin),
            Qavg = mean(Qavg),
            Qmax = mean(Qmax),
            .groups = 'drop') %>% 
  left_join(hrsPars, .) %>%
  rowwise() %>% 
  mutate(S = ifelse(!is.na(a), integrate(S_pl_int, Qmin, Qmax, a, b)$value, NA),
         Smin = ifelse(!is.na(a), -integrate(S_pl_int, Qmin, Qavg, a, b)$value, NA),
         Smax = ifelse(!is.na(a), integrate(S_pl_int, Qavg, Qmax, a, b)$value, NA)) %>% 
  arrange(desc(S))

write_csv(storagePowerLaw, 'Results/HRS_MDB_SD_PowerLaw.csv')
#storagePowerLaw = read_csv('Results/HRS_MDB_SD_PowerLaw.csv')

hrs_fdf = lapply(hrsSD, function(x) x$fdf) %>% 
  bind_rows(.id='station_id')
write_csv(hrs_fdf, 'Results/HRS_MDB_SD_fdf.csv')

hrs_fdfBin = lapply(hrsSD, function(x) x$fdfBin) %>% 
  bind_rows(.id='station_id')
write_csv(hrs_fdfBin, 'Results/HRS_MDB_SD_fdfBin.csv')

summary(storagePowerLaw$S)

ggplot(storagePowerLaw, aes(S)) +
  geom_histogram()

# Plot together
q_dq_p = ggplot() +
  geom_point(data=hrs_fdf, aes(Qavg, `-dQ`)) +
  geom_point(data=hrs_fdfBin, aes(Qmean, dQmean), col='red') +
  facet_wrap(~Number, ncol=6) +#, scales='free') +
  labs(x=expression(ln~Q~(mm.day^-1)), y=expression(ln~-dQdt~(mm.day^-2))) +
  scale_x_continuous(trans='log', labels = scales::trans_format("log", scales::math_format(10^.x))) +
  scale_y_continuous(trans='log', labels = scales::trans_format("log", scales::math_format(10^.x)))
q_dq_p
