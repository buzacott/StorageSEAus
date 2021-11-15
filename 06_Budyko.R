library(tidyverse)
library(lubridate)

# Fu formulation e.g. Zhang 2004
fu_f = function(w, x) {
  p = x$P
  q = x$Q
  pet = x$PET
  pq = x$P - x$Q

  obs = pq/p
  est = 1 + pet/p - (1 + (pet/p)^w)^(1/w)
  
  return(sum( (obs-est)^2, na.rm=T ))
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
      if(p[i] > 0) {
        et_f[i] = p[i] * (1 + pet[i]/p[i] - (1 + (pet[i]/p[i])^w)^(1/w))
      } else {
        et_f[i] = 0
      }
      dS[i] = p[i] - q[i] - et_f[i] 
    } else {
      p_ds = max(p[i] + dS[i-1], 0)
      if(p_ds > 0) {
        et_f[i] = p_ds * (1 + pet[i]/p_ds - (1 + (pet[i]/p_ds)^w)^(1/w))
      } else {
        et_f[i] = 0
      }
      dS[i] = dS[i-1] + p[i] - q[i] - et_f[i] 
    }
  }
  
  # return( (mean(pq) - mean(et_f))^2 )
  return(sum( (pq - et_f)^2 ))
}

# Du et al 2016
du_f = function(w, l=0, x) {
  p = x$P
  q = x$Q
  pet = x$PET
  pq = x$P - x$Q
  
  l = ifelse(p == 0, -1, l)
  
  obs = pq/p
  est = 1 + pet/p - (1 + (pet/p)^w + l)^(1/w)
  
  return(sum( (obs-est)^2 ))
}


# HRS Station Data
hrs = read_csv('Data/hrs_mdb_station_ext_attrs.csv')
hrs_facts = read_csv('Data/hrs_facts.csv') %>% 
  filter(awrc %in% hrs$station_id)
hrs = left_join(hrs,
                hrs_facts %>% select(awrc, area), by=c('station_id' = 'awrc'))

# Load in data for these stations
hrs_data = lapply(hrs$station_id, function(station) {
  read_csv(file.path('Data', 'Timeseries', paste0(station, '.csv')),
           show_col_types = FALSE)
})
names(hrs_data) = hrs$station_id

# Annual data
hrs_data_ann = lapply(hrs_data, function(x) x %>%
                        mutate(Year = year(Date)) %>% 
                        group_by(Year) %>% 
                        summarise(P = sum(P),
                                  Q = sum(Q),
                                  PET = sum(PET),
                                  AET = sum(AET),
                                  .groups='drop'))



hrs_budyko = lapply(hrs_data_ann, function(df) {
  
  opt = optimize(zc_f, c(1,10), tol=0.0001, x=df)
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
names(hrs_budyko) = hrs$station_id

hrs_budyko_df = hrs_budyko %>% 
  bind_rows(.id = 'station_id') 

#saveRDS(hrs_budyko_df, 'Data/hrs_budyko_df.Rds')

budyko_s = hrs_budyko_df %>% 
  group_by(station_id) %>% 
  summarise(Smin = min(deltaS),
            Smax = max(deltaS),
            w = unique(w),
            .groups = 'drop') %>% 
  mutate(S = Smax - Smin)

#write_csv(budyko_s, 'Results/HRS_MDB_Budyko_S.csv')

#------#
# Plot #
#------#

# Histogram of storages
ggplot(budyko_s, aes(S)) +
  geom_histogram() +
  xlim(0, NA)

# Budyko plot
budyko_plot_df = hrs_budyko_df %>% 
  select(station_id, P, Q, PET) %>%
  group_by(station_id) %>% 
  summarise_if(is.numeric, mean) %>% 
  left_join(budyko_s %>% select(station_id, S, w)) %>% 
  mutate(PET_P = PET/P,
         ET_P = (P-Q)/P)

limits = tibble(x=seq(0,ceiling(max(budyko_plot_df$PET/budyko_plot_df$P))),
                y=c(0,1,rep(1, max(x)-1)))

w = c(2, 2.63, 5, 10)
w_lines = lapply(w, function(x) {
  tibble(w=x,
         pet_p = seq(0, 3, 0.1),
         et_p =  1 + pet_p - (1 + (pet_p)^x)^(1/x) )
}) %>% bind_rows() %>% 
  mutate(w = factor(w))

ggplot(budyko_plot_df) +
  geom_line(data=w_lines, aes(pet_p, et_p, col=w)) +
  geom_line(data=limits, aes(x, y), linetype=2) +
  geom_point(aes(PET_P, ET_P, fill=S), shape=21, size=2.5, col='white') +
  labs(x=expression(bar(PET)/bar(P)),
       y=expression(bar(ET)/bar(P)),
       fill='S (mm)',
       col=expression(italic(w))) +
  scale_fill_continuous(trans='log10')+
  scale_x_continuous(expand=c(0,0), limits=c(0,3)) +
  scale_y_continuous(expand=c(0,0), breaks=seq(0, 1, 0.2), limits=c(0,1.1)) +
  annotate('text', x=0.4, y=0.5, angle=62.5, label='Energy limit') +
  annotate('text', x=2, y=1.05, label='Water limit')


hrs_budyko_df %>% 
  filter(station_id %in% (budyko_s %>% filter(S < 100) %>% pull(station_id))) %>% 
  ggplot(aes(Year, deltaS, col=station_id)) +
  geom_line()

hrs_budyko_df %>% 
  #filter(station_id %in% (budyko_s %>% filter(S >500) %>% pull(station_id))) %>% 
  ggplot(aes(Year, deltaS, col=station_id)) +
  geom_line()

#------------------------------------------------------------------------------#
# Monthly using Zeng Cai
#------------------------------------------------------------------------------#

hrs_budyko_mon = lapply(hrs_data, function(df) {
  #df = hrs_data[[1]]
  df = df %>% 
    select(Date, P, Q, AET, PET)
  
  df_ann = df %>%
    mutate(Year = year(Date)) %>%
    group_by(Year) %>%
    summarise(across(P:PET, sum))
  
  df_mon = df %>% 
    mutate(Year = year(Date),
           Month = month(Date)) %>% 
    group_by(Year, Month) %>% 
    summarise(Date = min(Date),
              across(P:PET, sum),
              .groups = 'drop') 
  
  opt = optimize(zc_f, c(1,10), tol=0.0001, x=df_ann)
  #opt = optimize(zc_f, c(1,10), tol=0.0001, x=df_mon)
  #opt = optimize(fu_f, c(1,10), tol=0.0001, x=df)
  w = opt$minimum
  obj= opt$objective
  
  p = df_mon$P
  q = df_mon$Q
  pet = df_mon$PET
  
  dS = rep(0, length(p))
  et_f = rep(0, length(p))
  
  for(i in 1:length(p)) {
    if(i==1) {
      if(p[i] > 0) {
        et_f[i] = p[i] * (1 + pet[i]/p[i] - (1 + (pet[i]/p[i])^w)^(1/w))
      } else {
        et_f[i] = 0
      }
      dS[i] = p[i] - q[i] - et_f[i] 
    } else {
      p_ds = max(p[i] + dS[i-1], 0)
      if(p_ds > 0) {
        et_f[i] = p_ds * (1 + pet[i]/p_ds - (1 + (pet[i]/p_ds)^w)^(1/w))
      } else {
        et_f[i] = 0
      }
      dS[i] = dS[i-1] + p[i] - q[i] - et_f[i] 
    }
  }
  
  df_mon = df_mon %>% 
    mutate(ET_f =et_f) %>% 
    mutate(deltaS = dS,
           w = w,
           obj = obj)
  
  return(df_mon)
})

hrs_budyko_mon_df = hrs_budyko_mon %>% 
  bind_rows(.id = 'station_id') 

budyko_mon_s = hrs_budyko_mon_df %>% 
  group_by(station_id) %>% 
  summarise(Smin = min(deltaS),
            Smax = max(deltaS),
            w = unique(w),
            .groups = 'drop') %>% 
  mutate(S = Smax - Smin)

ggplot(budyko_mon_s, aes(S)) +
  geom_histogram() +
  xlim(0, NA)

summary(budyko_mon_s$S)

hrs_budyko_mon_df %>% 
  ggplot(aes(Date, deltaS)) +
  geom_line() +
  facet_wrap(~station_id)

budyko_plot_mon_df = hrs_budyko_mon_df %>% 
  select(station_id, P, Q, PET) %>%
  group_by(station_id) %>% 
  summarise_if(is.numeric, mean) %>% 
  left_join(budyko_mon_s %>% select(station_id, S, w)) %>% 
  mutate(PET_P = PET/P,
         ET_P = (P-Q)/P)

w = c(2, 2.77, 5, 10)
w_lines = lapply(w, function(x) {
  tibble(w=x,
         pet_p = seq(0, 3, 0.1),
         et_p =  1 + pet_p - (1 + (pet_p)^x)^(1/x) )
}) %>% bind_rows() %>% 
  mutate(w = factor(w))
limits = tibble(x=seq(0,ceiling(max(budyko_plot_mon_df$PET/budyko_plot_mon_df$P))),
                y=c(0,1,rep(1, max(x)-1)))


ggplot(budyko_plot_mon_df) +
  geom_line(data=w_lines, aes(pet_p, et_p, col=w)) +
  geom_line(data=limits, aes(x, y), linetype=2) +
  geom_point(aes(PET_P, ET_P, fill=S), shape=21, size=2.5, col='white') +
  labs(x=expression(bar(PET)/bar(P)),
       y=expression(bar(ET)/bar(P)),
       fill='S (mm)',
       col=expression(italic(w))) +
  scale_fill_continuous(trans='log10')+
  scale_x_continuous(expand=c(0,0), limits=c(0,3)) +
  scale_y_continuous(expand=c(0,0), breaks=seq(0, 1, 0.2), limits=c(0,1.1)) +
  annotate('text', x=0.4, y=0.5, angle=62.5, label='Energy limit') +
  annotate('text', x=2, y=1.05, label='Water limit')


saveRDS(hrs_budyko_mon_df, 'Results/HRS_MDB_Budyko_df.Rds')
write_csv(budyko_mon_s, 'Results/HRS_MDB_Budyko_S.csv')


#------------------------------------------------------------------------------#
# With errors
#------------------------------------------------------------------------------#

hrs_budyko = lapply(hrs_data_ann, function(df) {
  
  df_ci = df %>% 
    pivot_longer(-Year) %>% 
    group_by(name) %>% 
    summarise(mean = mean(value),
              sd = sd(value),
              n = n()) %>% 
    mutate(lci = mean - sd/sqrt(n) * qt(0.975, n-1),
           uci = mean + sd/sqrt(n) * qt(0.975, n-1)) %>% 
    select(-c(sd, n)) %>% 
    pivot_longer(-name, names_to='var') %>% 
    pivot_wider(names_from=name) %>% 
    group_by(var) %>% 
    nest() %>% 
    mutate(data = map(data, function(x) {
      opt = optimize(fu_f, c(1,10), tol=0.0001, x=x)
      
      x %>%
        mutate(w = opt$minimum, obj = opt$objective)
    })) %>% 
    unnest('data')
  
  w_lci = df_ci %>% filter(var == 'lci') %>% pull(w)
  w_mean = df_ci %>% filter(var == 'mean') %>% pull(w)
  w_uci = df_ci %>% filter(var == 'uci') %>% pull(w)
  
  out = df %>% 
    mutate(w_lci = df_ci %>% filter(var == 'lci') %>% pull(w),
           w_mean = df_ci %>% filter(var == 'mean') %>% pull(w),
           w_uci = df_ci %>% filter(var == 'uci') %>% pull(w)) %>% 
    pivot_longer(c(w_lci, w_mean, w_uci), values_to='w') %>% 
    mutate(ET_f = P * (1 + PET/P - (1 + (PET/P)^w)^(1/w))) %>% 
    group_by(name) %>% 
    mutate(deltaS = cumsum(P) - cumsum(Q) - cumsum(ET_f)) %>% 
    
    return(out)
})

hrs_budyko_df = hrs_budyko %>% 
  bind_rows(.id = 'station_id') 

saveRDS(hrs_budyko_df, 'Results/HRS_MDB_Budyko_df.Rds')

budyko_s = hrs_budyko_df %>% 
  group_by(station_id, name) %>% 
  summarise(Smin = min(deltaS),
            Smax = max(deltaS),
            w = unique(w),
            .groups = 'drop') %>% 
  mutate(S = Smax - Smin)

write_csv(budyko_s, 'Results/HRS_MDB_Budyko_S.csv')

ggplot(budyko_s, aes(S)) +
  geom_histogram() +
  facet_wrap(~name)