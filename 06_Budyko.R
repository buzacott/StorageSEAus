library(tidyverse)
library(lubridate)
library(zoo)

# HRS difference in delta S

# HRS Station Data
hrs = read_csv('Data/hrs_se_ext_attr.csv')

hrs_data = lapply(hrs$Number, function(x) {
  read_csv(paste0('Data/TimeseriesAWAPer/', x, '.csv'),
           col_types = list(Q = col_number())) #%>% 
  #select(Date, P, Q, PET, PET_pt, AET) #%>% 
  # rename(PET = PET_pt)
})
names(hrs_data) = hrs$Number

# Annual data
hrs_data_ann = lapply(hrs_data, function(x) x %>%
                        mutate(Year = year(Date)) %>% 
                        group_by(Year) %>% 
                        summarise(P = sum(P),
                                  Q = sum(Q),
                                  PET = sum(WET), # Morton's wet is equiv to conventional PET
                                  AET = sum(AET),
                                  .groups='drop'))

# Fu formulation e.g. Zhang 2004
fu_f = function(w, x) {
  p = mean(x$P)
  q = mean(x$Q)
  pet = mean(x$PET)
  pq = mean(x$P - x$Q)
  
  obs = pq/p
  est = 1 + pet/p - (1 + (pet/p)^w)^(1/w)
  
  return(abs(obs-est))
}

# Zheng and Cai 2015 approach
zc_f = function(w, x) {
  p = x$P
  q = x$Q
  pet = x$PET
  pq = x$P - x$Q
  
  dS = rep(0, length(p))
  et_f = rep(0, length(p))
  
  for(i in 1:length(p)) {
    if(i==1) {
      et_f[i] = p[i] * (1 + pet[i]/p[i] - (1 + (pet[i]/p[i])^w)^(1/w))
      dS[i] = p[i] - q[i] - et_f[i] 
    } else {
      p_ds = max(p[i] + dS[i-1], 0.1)
      et_f[i] = p_ds * (1 + pet[i]/p_ds - (1 + (pet[i]/p_ds)^w)^(1/w))
      dS[i] = dS[i-1] + p[i] - q[i] - et_f[i] 
    }
  }
  
  return( (mean(pq) - mean(et_f))^2 )
}

hrs_budyko = lapply(hrs_data_ann, function(df) {
  opt = optimize(fu_f, c(1,10), tol=0.0001, x=df)
  w = opt$minimum
  obj= opt$objective
  
  p = df$P
  q = df$Q
  pet = df$PET
  
  dS = rep(0, length(p))
  et_f = rep(0, length(p))
  
  for(i in 1:length(p)) {
    if(i==1) {
      et_f[i] = p[i] * (1 + pet[i]/p[i] - (1 + (pet[i]/p[i])^w)^(1/w))
      dS[i] = p[i] - q[i] - et_f[i] 
    } else {
      p_ds = max(p[i] + dS[i-1], 1)
      et_f[i] = p_ds * (1 + pet[i]/p_ds - (1 + (pet[i]/p_ds)^w)^(1/w))
      dS[i] = dS[i-1] + p[i] - q[i] - et_f[i] 
    }
  }
  
  df = df %>% 
    mutate(ET_f =et_f) %>% 
    mutate(deltaS = cumsum(P) - cumsum(Q) - cumsum(ET_f),
           w = w,
           obj = obj)
  
  return(df)
})

hrs_budyko_df = hrs_budyko %>% 
  bind_rows(.id = 'Number') 

saveRDS(hrs_budyko_df, 'Data/hrs_budyko_df.Rds')

budyko_s = hrs_budyko_df %>% 
  group_by(Number) %>% 
  summarise(Smin = min(deltaS),
            Smax = max(deltaS),
            w = unique(w),
            .groups = 'drop') %>% 
  mutate(S = Smax - Smin)

write_csv(budyko_s, 'Data/Results/HRS_se_Budyko_S.csv')