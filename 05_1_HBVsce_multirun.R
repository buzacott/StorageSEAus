#!/usr/bin/env Rscript
library(hydromad)
#library(Rhpc)

runHBV = function(x) {
  run = x[['run']]
  station = x[['station']]
  cal_start = as.Date(x[['cal_start']])
  cal_end = as.Date(x[['cal_end']])
  
  fname = file.path('Data', 'HBVResultsR2V', run, paste0(station, '.Rds'))
  
  if(!file.exists(fname)) {
    # Write to log file
    sink("HBV_log.txt", append=TRUE)
    cat(paste(Sys.time(), "Calibrating", station, "\n"))
    sink()
    
    # Read in timeseries
    river = read.csv.zoo(paste0('Data/Timeseries/', station, '.csv'), header=TRUE)
    # Estimate avg T
    river$Tavg = (river$Tmax + river$Tmin) / 2
    # Calibrate only on high quality data points
    river$Q = ifelse(river$QC > 2, NaN, river$Q)
    # Subset and rename columns
    river = river[,c('P','Q','PET','Tavg')]
    colnames(river) = c('P', 'Q', 'E', 'T')
    
    # Set cal period
    river = window(river, start = cal_start, end = cal_end)

    # Create hydromad variable
    CMod = hydromad(DATA = river,
                    sma = 'hbv',
                    routing = 'hbvrouting',
                    tt     = c(-2, 0.5),
                    cfmax  = c(1, 10),
                    sfcf   = c(0.4, 1.6),
                    cwh    = c(0, 0.2),
                    cfr    = c(0, 0.1),
                    fc     = c(50, 550),
                    lp     = c(0.3, 1),
                    beta   = c(1, 6),
                    perc   = c(0, 3),
                    uzl    = c(10, 100),
                    k0     = c(0.05, 0.5),
                    k1     = c(0.01, 0.4),
                    k2     = c(0.001, 0.15),
                    maxbas = c(1, 14),
                    return_state = TRUE,
                    return_components = TRUE)
    
    # Fit with NSE objective function
    tryCatch( {
      fitR2V = fitBySCE(CMod,
                        objective = r2v,
                        control = list(ncomplex = 15))
      # Write
      saveRDS(fitR2V, file = fname) 
      
      resultR2V  = c('run' = run,
                     'station_id' = station,
                     'Message' = fitR2V$fit.result$message,
                     'tt' = fitR2V$parlist$tt,
                     'cfmax' = fitR2V$parlist$cfmax,
                     'sfcf' = fitR2V$parlist$sfcf,
                     'cfr' = fitR2V$parlist$cfr,
                     'cwh' = fitR2V$parlist$cwh,
                     'fc' = fitR2V$parlist$fc,
                     'lp' = fitR2V$parlist$lp,
                     'beta' = fitR2V$parlist$beta,
                     'perc' = fitR2V$parlist$perc,
                     'uzl' = fitR2V$parlist$uzl,
                     'k0' = fitR2V$parlist$k0,
                     'k1' = fitR2V$parlist$k1,
                     'k2' = fitR2V$parlist$k2,
                     'maxbas' = fitR2V$parlist$maxbas,
                     'R2V' = objFunVal(fitR2V, objective=r2v))
    }, error = function(e) {
      resultR2V  = c('run' = run,
                     'station_id' = station,
                     'Message' = NA,
                     'tt' = NA,
                     'cfmax' = NA,
                     'sfcf' = NA,
                     'cwh' = NA,
                     'cfr' = NA,
                     'fc' = NA,
                     'lp' = NA,
                     'beta' = NA,
                     'k0' = NA,
                     'k1' = NA,
                     'k2' = NA,
                     'uzl' = NA,
                     'perc' = NA,
                     'maxbas' = NA,
                     'R2V' = NA) }
    )
  } else {
    fitR2V = readRDS(fname)
    resultR2V  = c('run' = run,
                   'station_id' = station,
                   'Message' = fitR2V$fit.result$message,
                   'tt' = fitR2V$parlist$tt,
                   'cfmax' = fitR2V$parlist$cfmax,
                   'sfcf' = fitR2V$parlist$sfcf,
                   'cwh' = fitR2V$parlist$cwh,
                   'cfr' = fitR2V$parlist$cfr,
                   'fc' = fitR2V$parlist$fc,
                   'lp' = fitR2V$parlist$lp,
                   'beta' = fitR2V$parlist$beta,
                   'k0' = fitR2V$parlist$k0,
                   'k1' = fitR2V$parlist$k1,
                   'k2' = fitR2V$parlist$k2,
                   'uzl' = fitR2V$parlist$uzl,
                   'perc' = fitR2V$parlist$perc,
                   'maxbas' = fitR2V$parlist$maxbas,
                   'R2V' = objFunVal(fitR2V, objective=r2v))
  }
  
  return(resultR2V)
}

# HRS stations
stations = read.csv('Data/hrs_mdb_station_ext_attrs.csv')$station_id

runlist = data.frame(run = paste0('run_', sprintf('%02d', rep(1:10, each = length(stations)))), 
                     station  = rep(stations, times = 10),
                     cal_start = '1990-01-01',
                     cal_end   = '2018-12-31')

# Set up dirs for output
for(run in unique(runlist$run)) {
  dest = file.path('Data', 'HBVResultsR2V', run)
  if(dir.exists(dest) == FALSE) {
    dir.create(dest)
  }
}

r2v = function(Q, X, ...) {
  ok <- complete.cases(X, Q)
  Q <- Q[ok]
  X <- X[ok]
  nseStat(Q, X, ...) - 0.1 * abs(sum(X-Q)) / sum(Q)
}

# Create log file to check progress
writeLines(c(""), 'HBV_log.txt')

# Initialise
Rhpc_initialize()

# Setup the cluster
cl = Rhpc_getHandle()

Rhpc_EvalQ(cl, library(hydromad))
Rhpc_Export(cl, 'r2v')

# Parallel apply
r = Rhpc_apply(cl, runlist, 1, runHBV)

# Close the cluster
Rhpc_finalize()

r = apply(runlist, 1, runHBV)

write.csv(as.data.frame(t(r)), 'Results/HRS_MDB_HBV_CalResultsR2V_10x.csv', row.names=FALSE)
