# Download HRS station data from BoM

download_dir = 'Data/Qts'

if(!dir.exists(download_dir)) {
  dir.create(download_dir, recursive = TRUE)
}

stations = read.csv('Data/hrs_se_ext_attr.csv')$Number
base_url = 'http://www.bom.gov.au/water/hrs/content/data/'

# Download files
for(station in stations) {
  download.file(
    url = file.path(base_url, station, paste0(station, '_daily_ts.csv')),
    destfile = file.path(download_dir, paste0(station, '.csv'))
  )
}
