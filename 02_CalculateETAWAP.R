# Calculate ET using Evapotranspiration package

library(tidyverse)
library(lubridate)
library(Evapotranspiration)
library(zoo)

hrs = read_csv('Data/hrs_se_ext_attr.csv')

et_constants = list(sigma = 4.903*10^-9,
                    epsilonMo = 0.92,
                    fz = 28,
                    b0 = 1,
                    b1 = 14,
                    b2 = 1.2,
                    gammaps = 0.66,
                    alphaMo = 17.27,
                    betaMo = 237.3,
                    sigmaMo = 5.67e-8,
                    lambdaMo = 28.5,
                    lambda = 2.45,
                    Gsc = 0.0820,
                    alphaPT = 1.26,
                    G = 0)

make_awap_wb = apply(hrs, 1, function(x) {
  station = x['Number']
  
  station_met_tbl = read_csv(paste0('Data/TimeseriesAWAP/', station, '.csv')) %>% 
    mutate(Rad = if_else(Rad < -10, NaN, Rad)) %>% 
    mutate(Tavg = round((Tmax + Tmin)/2, 2),
           SVP = round(6.11 * exp( (17.27*Tavg) / (237.3+Tavg) ), 2),
           RH = round(100 * VP/SVP, 2)) %>% 
    mutate(RH = if_else(RH>100, 100, RH))
  
  # station_met = read.csv.zoo(paste0('Data/TimeseriesAWAP/', station, '.csv'), tz='UTC')
  station_met = zoo(station_met_tbl[,-1], order.by = station_met_tbl$Date)
  station_met$Rad = na.approx(station_met$Rad)
  
  station_data = list(Date.daily = index(station_met),
                      Date.monthly = unique(as.yearmon(index(station_met))),
                      J = yday(index(station_met)),
                      i = month(unique(as.yearmon(index(station_met)))),
                      Ndays = days_in_month(unique(as.yearmon(index(station_met)))),
                      Precip = station_met$P,
                      Tmax  = station_met$Tmax,
                      Tmin  = station_met$Tmin,
                      va = station_met$VP/10,
                      vs = station_met$SVP/10,
                      Rs    = station_met$Rad,
                      RH = station_met$RH)
  
  station_constants = et_constants
  station_constants['Elev'] = as.numeric(x['DEM_Mean'])
  station_constants['lat_rad'] = as.numeric(x['latitude'])*pi/180
  station_constants['lat'] = as.numeric(x['latitude'])
  
  aet = ET.MortonCRAE(station_data,
                      station_constants,
                      ts='monthly',
                      solar='data',
                      est='actual areal ET',
                      Tdew=FALSE,
                      save.csv='no')
  
  # pet = ET.MortonCRAE(station_data,
  #                     station_constants,
  #                     ts='monthly',
  #                     solar='data',
  #                     est='potential ET',
  #                     Tdew=FALSE,
  #                     save.csv='no')
  
  pet = ET.PriestleyTaylor(station_data,
                           station_constants,
                           ts='daily',
                           solar='data',
                           est='potential ET',
                           Tdew=FALSE,
                           save.csv='no')
  
  station_mo = station_met_tbl %>% 
    mutate(Date = as.yearmon(Date)) %>% 
    group_by(Date) %>% 
    summarise(P = sum(P),
              Q = sum(Q),
              .groups='drop') %>% 
    mutate(AET = coredata(aet$ET.Monthly[,1]),
           PET = coredata(pet$ET.Monthly[,1])) %>% 
    mutate(deltaS = cumsum(P) - cumsum(Q) - cumsum(AET)) 
  
  aet_tbl = tibble(Date = index(aet$ET.Monthly),
                   AET  = coredata(aet$ET.Monthly)[,1]) %>% 
    mutate(Year = year(Date),
           Month = month(Date)) %>% 
    select(-Date)
  
  # aet_daily = station_met_tbl %>% 
  #   mutate(Year = year(Date),
  #          Month = month(Date)) %>% 
  #   select(Date, Year, Month, Tmax) %>% 
  #   left_join(., aet_tbl, by=c('Year', 'Month')) %>% 
  #   group_by(Year, Month) %>% 
  #   mutate(AET = AET * Tmax/sum(Tmax)) %>% 
  #   ungroup()

  aet_daily = tibble(Date = station_met_tbl$Date,
                     PET  = coredata(pet$ET.Daily)) %>% 
    mutate(Year = year(Date),
           Month = month(Date)) %>% 
    left_join(., aet_tbl, by=c('Year', 'Month')) %>% 
    group_by(Year, Month) %>% 
    mutate(AET = AET * PET/sum(PET)) %>% 
    ungroup()
    
  out = station_met_tbl %>% 
    mutate(AET = aet_daily$AET,
           PET = coredata(pet$ET.Daily)) %>% 
    select(Date, P, Q, AET, PET, everything())
  
  write_csv(out, paste0('Data/TimeseriesAWAP/', station, '.csv'))
  return('Success')
})
