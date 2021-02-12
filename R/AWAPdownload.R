library(AWAPer)

# Set working directory.
setwd('/mnt/Data/AWAPer')

makeNetCDF_file(ncdfFilename = 'AWAP_1990_2018.nc',
                ncdfSolarFilename = 'AWAP_1990_2018_solar.nc',
                updateFrom = as.Date('1990-01-01'),
                updateTo   = as.Date('2018-12-31'))
