# Functions

decimalplaces <- function(x) {
  if (abs(x - round(x)) > .Machine$double.eps^0.5) {
    nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed = TRUE)[[1]][[2]])
  } else {
    return(0)
  }
}

# Standard error
std = function(x) sd(x)/sqrt(length(x))

# Integrate to find storage
# Power law
S_pl_int = function(Q, a, b) {
  1/(exp(a)*Q^(b-1))
}
# Polynomial
S_poly_int = function(Q, c1, c2, c3) {
  return( 1/( exp(c1) * Q^(c2-1) * Q^(c3*log(Q)) ) )
}

S_Q = function(Q, a, b) {
  # Q: streamflow timeseries in mm/h
  # a and b are power law coefficients
  s_Q = 1/exp(a) * 1/(2-b) * Q^(2-b)
  return(s_Q)
}

S = function(Q, a, b, minQ) {
  # Storage is determined by adding a reference storage level
  # Output is negative from power law, (-- -> +)
  S = S_Q(Q, a, b) - S_Q(minQ, a, b)
  return(S)
}

# dQdt
dQdt_pl = function(a,b,Q) return(a*Q^b)
dQdt_poly = function(Q, c1, c2, c3) return(exp(c1 + c2*log(Q) + c3*(log(Q)^2)))

# Binning as described in Kirchner 2009
kirchnerBin = function(x) {
  # x: Pass the filtered dataframe
  
  # Copy the df and arrange by highest Q to lowest
  tmp = x %>% 
    arrange(desc(Q))
  
  # Take the log of Q and find out 1% of the range
  Qlog = log(tmp$Qavg)
  Qrange = diff(range(Qlog))/100
  
  # Initialise variables for the while loop
  i = 1
  j = 2
  binC = 1
  tmp$Bin = NA
  
  # While there are still values of Q to bin
  while(j < length(Qlog)) {
    
    # Calculate the range of Q
    lRange = diff(range(Qlog[i:j]))
    
    # If the two points don't exceed the 1% of the logarithmic Q range
    # add more points until it does
    while(lRange < Qrange & j<length(Qlog)) {
      j = j + 1
      if(j>length(Qlog)) break
      lRange = diff(range(Qlog[i:j]))
    }
    
    # Calculate the mean the mean and standard error of dQ for those values of Q
    dQmean = abs(mean(tmp$`-dQ`[i:j]))
    dQstd  = std(tmp$`-dQ`[i:j])
    
    # Check the criterion that std.err(-dQ/dt) <= mean(dQ/dt)/2
    # Extend the range of the bin until satisfied
    while(dQstd > (dQmean/2)) {
      j = j + 1
      if(j>length(Qlog)) break
      dQmean = abs(mean(tmp$`-dQ`[i:j]))
      dQstd  = std(tmp$`-dQ`[i:j])
    }
    
    if(j>length(Qlog)) break
    # Insert values of the bin into the df
    tmp$Bin[i:j] = binC
    
    # Increment counters and reset variables
    i = j + 1
    j = j + 1
    binC = binC + 1
  }
  if('Datetime' %in% colnames(tmp)) {
    tmp = tmp %>% arrange(Datetime)
  } else {
    tmp = tmp %>% arrange(Date)
  }
  return(tmp)
}

# Function to calculate the storage-discharge relationship at the hourly level
# Requires that you have filtered to between sunrise and sunset
storageDischargeHourly = function(df, area=3.6) {
  # df: a table with Datetime, Q and P 
  # area: area of catchment if the data supplied is in m3/s
  
  # Time series of Q
  n = nrow(df)
  Qt = df$Q
  # Time series of Qt-1
  Qtm1 = c(NaN, Qt[1:(n-1)])
  # Vector of dates
  Dt = df$Datetime
  # Determine dQ, dt and Qavg. dt should always be 1 hour
  round = 6
  pdf = df %>% 
    mutate(`-dQ`= Qtm1-Qt,
           dt   = c(NaN, diff(Dt)),
           Qavg = ((Qtm1 + Qt)/2)) %>% 
    mutate(`-dQ` =(`-dQ`*3.6/area),
           Q = (Q*3.6/area),
           Qavg = (Qavg*3.6/area)) %>% 
    filter(month(Date)>=5 & month(Date)<=9) %>% 
    filter(abs(`-dQ`) > (0.001 * 3.6/147))
  
  pHours = which(pdf$P>0)
  pFilter = sapply(-7:2, function(x) x+pHours) %>% 
    as.vector() %>% unique()
  pFilter = pFilter[pFilter>=1 & pFilter<=nrow(pdf)]
  # Filter
  fdf = pdf %>% 
    slice(-pFilter) %>% 
    filter(!is.na(P),
           !is.na(Qavg),
           QC_Q %in% c(1,2),
           QC_P %in% c(1,2),
           Qavg > 0,
           dt == 1,
           Datetime<(Sunrise-3600) | Datetime>(Sunset+3600))
  
  # Binning using the Kirchner method
  fdf = kirchnerBin(fdf)
  
  # Binned data
  # Filter using Kirchner test criterion std.error(-dQ/dt) <= mean(dQ/dt)/2
  fdfBin = fdf %>% 
    group_by(Bin) %>% 
    summarise(Qmean  = mean(Qavg),
              Qstd   = std(Qavg),
              dQmean = mean(`-dQ`),
              dQstd  = std(`-dQ`),
              n      = n(),
              .groups = 'drop') %>% 
    filter(!is.na(Bin))
  
  # Fit a polynomial to binned values: Equation 9 in Kirchner 2009
  # ln(-dQ/dt) = c1 + c2(logQ) + c3(logQ)^2
  ln_dQdt = lm(log(dQmean) ~ log(Qmean) + I(log(Qmean)^2), data=fdfBin %>% filter(dQmean>0), weights = 1/(log(Qstd)^2))
  c1 = unname(ln_dQdt$coefficients[1])
  c2 = unname(ln_dQdt$coefficients[2])
  c3 = ifelse(summary(ln_dQdt)$coefficients[3,4] < 0.1,
              unname(ln_dQdt$coefficients[3]),
              0)
  
  # Power law function
  # ln(-dQ/dt) = -dQ/dt = a*Q^b
  pwl_dQdt = lm(log(dQmean)~log(Qmean), data=fdfBin %>% filter(dQmean>0), weights = 1/(log(Qstd)^2))
  a = unname(pwl_dQdt$coefficients[1])
  b = unname(pwl_dQdt$coefficients[2])
  
  # Plots
  pl_p_title = paste0('ln(-dQ/dt) = ',round(a, 2),' + ', round(b, 2), '.ln(Q), R2 = ', 
                      round(summary(pwl_dQdt)$adj.r.squared,2))
  pl_p = ggplot() +
    geom_point(data=fdf %>% filter(`-dQ`>0), aes(Qavg, `-dQ`), size=0.5) +
    geom_point(data=fdfBin, aes(Qmean, dQmean), col='red', size=0.5) + 
    labs(y='ln -dQdt (mm/hr)', x='ln Q (mm/hr)') +
    geom_line(aes(exp(pwl_dQdt$model$`log(Qmean)`), exp(pwl_dQdt$fitted.values)), col='green') +
    scale_x_continuous(trans='log', breaks=c(0.001,0.1,1,10)) + scale_y_continuous(trans='log', breaks=c(0.001, 0.01, 0.1, 1)) +
    theme_bw() +
    ggtitle(pl_p_title)
  
  poly_p_tile = paste0('ln(-dQ/dt) = ',round(c1, 2),' + ',
                       round(c2, 2), '.ln(Q) + ', round(c3, 2), '.ln(Q)^2, ',
                       'R2 = ', round(summary(pwl_dQdt)$adj.r.squared,2))
  
  poly_p = ggplot() +
    geom_point(data=fdf %>% filter(`-dQ`>0), aes(Qavg, `-dQ`), size=0.5) +
    geom_point(data=fdfBin, aes(Qmean, dQmean), col='red', size=0.5) + 
    labs(y='ln -dQdt (mm/hr)', x='ln Q (mm/hr)') +
    geom_line(aes(exp(pwl_dQdt$model$`log(Qmean)`), exp(pwl_dQdt$fitted.values)), col='green') +
    scale_x_continuous(trans='log', breaks=c(0.001,0.1,1,10)) + scale_y_continuous(trans='log', breaks=c(0.001, 0.01, 0.1, 1)) +
    theme_bw() +
    ggtitle(poly_p_tile)
  
  
  out = list(fdf=fdf,
             fdfBin=fdfBin,
             a = a,
             b = b,
             n = sum(fdfBin$n),
             pl_p = pl_p,
             poly_p = poly_p,
             c1 = c1,
             c2 = c2,
             c3 = c3)
  return(out)
}

# Function to calculate the storage-discharge relationship at the daily level
storageDischargeDaily  = function(df, area=86.4) {
  # df: a table with Date, Q, P, QC_Q, QC_P
  # area of the catchment if Q is in m3/s
  
  # Time series of Q
  n = nrow(df)
  Qt = df$Q
  # Time series of Qt-1
  Qtm1 = c(NA, Qt[1:(n-1)])
  # Vector of dates
  Dt = df$Date
  # Vector of dates Dt-1
  Dtm1 = c(as.Date(NA), Dt[1:(n-1)])
  
  # Determine dQ, dt and Qavg. dt should always be 1 day
  pdf = df %>%
    mutate(`-dQ` = Qtm1 - Qt,
           dt    = -as.integer(Dtm1-Dt),
           Qavg  = (Qtm1 + Qt)/2) %>%
    mutate(`-dQ` = (`-dQ`*86.4/area),
           Q     = (Q*86.4/area),
           Qavg  = (Qavg*86.4/area)) %>%
    filter(abs(`-dQ`) >= (0.001 * 86.4/area)) %>% 
    filter(month(Date)>=5 & month(Date)<=9)
  
  pFilter = unique((c(which(pdf$P>0), which(pdf$P>0)+1)))
  
  # Filter
  fdf = pdf %>% 
    slice(-pFilter) %>%
    filter(!is.na(P),
           !is.na(Qavg),
           QC_Q %in% c(10),
           QC_P %in% c(10),
           dt==1,
           Qavg > 0) 
  # Binning using the Kirchner method
  fdf = kirchnerBin(fdf)
  
  # Filter using Kirchner test criterion std.error(-dQ/dt) <= mean(dQ/dt)/2
  fdfBin = fdf %>% 
    group_by(Bin) %>% 
    summarise(Qmean  = mean(Qavg),
              Qstd   = std(Qavg),
              dQmean = mean(`-dQ`),
              dQstd  = std(`-dQ`),
              n      = n(),
              .groups = 'drop') %>% 
    filter(!is.na(Bin))
  
  # Fit a polynomial to binned values: Equation 9 in Kirchner 2009
  # ln(-dQ/dt) = c1 + c2(logQ) + c3(logQ)^2
  ln_dQdt = lm(log(dQmean) ~ log(Qmean) + I(log(Qmean)^2), data=fdfBin %>% filter(dQmean>0), weights = 1/(log(Qstd)^2))
  c1 = unname(ln_dQdt$coefficients[1])
  c2 = unname(ln_dQdt$coefficients[2])
  c3 = ifelse(summary(ln_dQdt)$coefficients[3,4] < 0.1,
              unname(ln_dQdt$coefficients[3]),
              0)

  # Power law function
  # ln(-dQ/dt) = -dQ/dt = a*Q^b
  pwl_dQdt = lm(log(dQmean)~log(Qmean), data=fdfBin %>% filter(dQmean>0), weights = 1/(log(Qstd)^2))
  a = unname(pwl_dQdt$coefficients[1])
  b = unname(pwl_dQdt$coefficients[2])
  
  # Plots
  pl_p_title = paste0('ln(-dQ/dt) = ',round(a, 2),' + ', round(b, 2), '.ln(Q), R2 = ', 
                      round(summary(pwl_dQdt)$adj.r.squared,2))
  pl_p = ggplot() +
    geom_point(data=fdf %>% filter(`-dQ`>0), aes(Qavg, `-dQ`), size=0.5) +
    geom_point(data=fdfBin, aes(Qmean, dQmean), col='red', size=0.5) + 
    labs(y=expression(ln~-dQdt~(mm.day^-2)), x=expression(ln~Q~(mm.day^-1))) +
    geom_line(aes(exp(pwl_dQdt$model$`log(Qmean)`), exp(pwl_dQdt$fitted.values)), col='green') +
    scale_x_continuous(trans='log', breaks=c(0.001,0.1,1,10)) + scale_y_continuous(trans='log', breaks=c(0.001, 0.01, 0.1, 1)) +
    theme_bw() +
    ggtitle(pl_p_title)
  
  poly_p_tile = paste0('ln(-dQ/dt) = ',round(c1, 2),' + ',
                       round(c2, 2), '.ln(Q) + ', round(c3, 2), '.ln(Q)^2, ',
                       'R2 = ', round(summary(pwl_dQdt)$adj.r.squared,2))

  poly_p = ggplot() +
    geom_point(data=fdf %>% filter(`-dQ`>0), aes(Qavg, `-dQ`), size=0.5) +
    geom_point(data=fdfBin, aes(Qmean, dQmean), col='red', size=0.5) +
    labs(y=expression(ln~-dQdt~(mm.day^-2)), x=expression(ln~Q~(mm.day^-1))) +
    geom_line(aes(exp(pwl_dQdt$model$`log(Qmean)`), exp(pwl_dQdt$fitted.values)), col='green') +
    scale_x_continuous(trans='log', breaks=c(0.001,0.1,1,10)) + scale_y_continuous(trans='log', breaks=c(0.001, 0.01, 0.1, 1)) +
    theme_bw() +
    ggtitle(poly_p_tile)


  out = list(fdf=fdf,
             fdfBin=fdfBin,
             a = a,
             b = b,
             n = sum(fdfBin$n),
             pl_p = pl_p,
             poly_p = poly_p,
             c1 = c1,
             c2 = c2,
             c3 = c3)
  return(out)
}

storageDischargeDaily_pl  = function(df, area=86.4) {
  # df: a table with Date, Q, P, QC_Q, QC_P
  # area of the catchment if Q is in m3/s
  
  # Time series of Q
  Qt = df$Q
  # Time series of Qt-1
  Qtm1 = c(NA, df$Q[1:(nrow(df)-1)])
  # Vector of dates
  Dt = df$Date
  # Vector of dates Dt-1
  Dtm1 = c(NA, df$Date[1:(nrow(df)-1)])
  
  # Determine dQ, dt and Qavg. dt should always be 1 day
  pdf = df %>%
    mutate(`-dQ`= Qtm1-Qt,
           dt   = as.integer(Dt-Dtm1),
           Qavg = round((Qtm1 + Qt)/2, 3)) %>%
    mutate(`-dQ` = `-dQ`*86.4/area,
           Q     = Q*86.4/area,
           Qavg  = Qavg*86.4/area)
  
  # pFilter = unique((c(which(pdf$P>0), which(pdf$P>0)+1)))
  # pFilter = unique((c(which(pdf$P>0), which(pdf$P>0)-1)))
  pFilter = which(pdf$P > 0)
  
  # Filter
  fdf = pdf %>% 
    slice(-pFilter) %>%
    filter(!is.na(P),
           !is.na(Qavg),
           QC_Q %in% c(1,2),
           QC_P %in% c(1,2,26),
           dt==1) 
  # Binning using the Kirchner method
  fdf = kirchnerBin(fdf)
  
  # Filter using Kirchner test criterion std.error(-dQ/dt) <= mean(dQ/dt)/2
  fdfBin = fdf %>% 
    group_by(Bin) %>% 
    summarise(Qmean  = mean(Qavg),
              Qstd   = std(Qavg),
              dQmean = mean(`-dQ`),
              dQstd  = std(`-dQ`),
              n      = n(),
              .groups = 'drop') %>% 
    filter(!is.na(Bin))
  

  # Power law function
  # ln(-dQ/dt) = -dQ/dt = a*Q^b
  pwl_dQdt = lm(log(dQmean)~log(Qmean), data=fdfBin %>% filter(dQmean>0), weights = 1/(log(Qstd)^2))
  a = unname(pwl_dQdt$coefficients[1])
  b = unname(pwl_dQdt$coefficients[2])
  
  # Plots
  pl_p_title = paste0('ln(-dQ/dt) = ',round(a, 2),' + ', round(b, 2), '.ln(Q), R2 = ', 
                      round(summary(pwl_dQdt)$adj.r.squared,2))
  pl_p = ggplot() +
    geom_point(data=fdf %>% filter(`-dQ`>0), aes(Qavg, `-dQ`), size=0.5) +
    geom_point(data=fdfBin, aes(Qmean, dQmean), col='red', size=0.5) + 
    labs(y='ln -dQdt (mm/day)', x='ln Q (mm/day)') +
    geom_line(aes(exp(pwl_dQdt$model$`log(Qmean)`), exp(pwl_dQdt$fitted.values)), col='green') +
    scale_x_continuous(trans='log', breaks=c(0.001,0.1,1,10)) + scale_y_continuous(trans='log', breaks=c(0.001, 0.01, 0.1, 1)) +
    theme_bw() +
    ggtitle(pl_p_title)
  
  out = list(fdf=fdf,
             fdfBin=fdfBin,
             a = a,
             b = b,
             n = sum(fdfBin$n),
             r2 = round(summary(pwl_dQdt)$adj.r.squared,2),
             pl_p = pl_p)
  return(out)
}

