#!/usr/bin/env Rscript
library(hydromad)
library(Rhpc)

runHBV = function(x) {
  suppressMessages(library(hydromad))
  station = x[['Station']]
  calStart = x[['calStart']]
  calEnd = x[['calEnd']]
  
  r2v = function(Q, X, ...) {
    # Eq 5 in Lindström 1997
    hmadstat("r.squared")(Q, X, ...) - 0.1 * abs(sum(X-Q)) / sum(Q)
  }
  
  fname = paste0('Data/HBVResultsR2V/', station, ".Rds")
  
  if(!file.exists(fname)) {
    # Write to log file
    sink("HBV_log.txt", append=TRUE)
    cat(paste(Sys.time(), "Calibrating", station, "\n"))
    sink()
    
    river = read.csv.zoo(paste0('Data/TimeseriesAWAPer/', station, '.csv'))
    river$`T` = (river$Tmax + river$Tmin) / 2
    river$E = river$PET
    river = river[,c('P','Q','E','T')]
    
    # Set cal period and create hydromad variable
    river.cal = window(river, start = calStart, end = calEnd)
    
    CMod = hydromad(DATA = river.cal,
                    sma = 'hbv',
                    routing = 'hbvrouting',
                    tt     = c(-2, 0.5),
                    cfmax  = c(1, 10),
                    sfcf   = c(0.4, 1),
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
                    maxbas = c(1, 7),
                    return_state = TRUE,
                    return_components = TRUE)
    
    # Fit with NSE objective function
    tryCatch( {
      fitR2V = fitBySCE(CMod,
                        objective = r2v,
                        control = list(ncomplex = 15))

      # Write
      saveRDS(fitR2V, file = fname) 
      
      resultR2V  = c('Number' = station,
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
                     'R2V' = objFunVal(fitR2V, objective=r2v)) },
      error = function(e) {
        resultR2V  = c('Number' = station,
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
    resultR2V  = c('Number' = station,
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

# Set up dir for output
if(dir.exists('Data/HBVResultsR2V') == FALSE) {
  dir.create('Data/HBVResultsR2V', recursive = TRUE)
}

# Stations
stations = read.csv('Data/hrs_se_ext_attr.csv')

runlist = data.frame(Station = stations$Number,
                     calStart = as.Date('1990-01-01'),
                     calEnd   = as.Date('2018-12-31'))

# Create log file to check progress
writeLines(c(""), 'HBV_log.txt')

# Initialise
Rhpc_initialize()

# Setup the cluster
cl = Rhpc_getHandle()

# Parallel apply
r = Rhpc_apply(cl, runlist, 1, runHBV)

# Close the cluster
Rhpc_finalize()

write.csv(as.data.frame(t(r)), 'Data/Results/HRS_se_HBVCalResultsR2V.csv', row.names=FALSE)
