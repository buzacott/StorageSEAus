import os

p = '../../Data/Shapefile/HRS_SE.shp'
f = 'Station_Nu'
s = 'mean'

# DEM
var = 'DEM'
r = '/mnt/Data/GIS/Data_9secDEM_D8/dem-9s/hdr.adf'
os.system('exactextract -p %s -r %s:%s -f %s -s "%s(%s)" -s "min(DEM)" -s "max(DEM)" -o %s.csv' % (p, var, r, f, s, var, var))

# Silica
var = 'silica'
r = '../../Data/SilicaIndexHRSse.tif'
os.system('exactextract -p %s -r %s:%s -f %s -s "%s(%s)" -o %s.csv' % (p, var, r, f, s, var, var))

# Clay
var = 'clay_000_005'
r = '/media/HGIS/SLGA/Clay/CLY_000_005_EV_N_P_AU_NAT_C_20140801.tif'
os.system('exactextract -p %s -r %s:%s -f %s -s "%s(%s)" -o %s.csv' % (p, var, r, f, s, var, var))

var = 'clay_005_015'
r = '/media/HGIS/SLGA/Clay/CLY_005_015_EV_N_P_AU_NAT_C_20140801.tif'
os.system('exactextract -p %s -r %s:%s -f %s -s "%s(%s)" -o %s.csv' % (p, var, r, f, s, var, var))

var = 'clay_015_030'
r = '/media/HGIS/SLGA/Clay/CLY_015_030_EV_N_P_AU_NAT_C_20140801.tif'
os.system('exactextract -p %s -r %s:%s -f %s -s "%s(%s)" -o %s.csv' % (p, var, r, f, s, var, var))

var = 'clay_030_060'
r = '/media/HGIS/SLGA/Clay/CLY_030_060_EV_N_P_AU_NAT_C_20140801.tif'
os.system('exactextract -p %s -r %s:%s -f %s -s "%s(%s)" -o %s.csv' % (p, var, r, f, s, var, var))

var = 'clay_060_100'
r = '/media/HGIS/SLGA/Clay/CLY_060_100_EV_N_P_AU_NAT_C_20140801.tif'
os.system('exactextract -p %s -r %s:%s -f %s -s "%s(%s)" -o %s.csv' % (p, var, r, f, s, var, var))

# Woody
var = 'WoodyIntensity' 
r = '/mnt/Data/GIS/AuscoverWoodyCover2000_2010_500m.tif'
os.system('exactextract -p %s -r %s:%s -f %s -s "%s(%s)" -o %s.csv' % (p, var, r, f, s, var, var))

var = 'WoodyCover' 
r = '/mnt/Data/GIS/AuscoverWoodyCover2000_2010_500m_Binary.tif'
os.system('exactextract -p %s -r %s:%s -f %s -s "%s(%s)" -o %s.csv' % (p, var, r, f, s, var, var))

# Depth of soil
var = 'SoilDepth'
r = '/media/HGIS/SLGA/DES/DES_000_200_EV_N_P_AU_NAT_C_20140801.tif'
os.system('exactextract -p %s -r %s:%s -f %s -s "%s(%s)" -o %s.csv' % (p, var, r, f, s, var, var))

# Depth to regolith
var = 'RegolithDepth'
r = '/media/HGIS/SLGA/DER/DER_000_999_EV_N_P_AU_NAT_C_20150601_fixed.tif'
os.system('exactextract -p %s -r %s:%s -f %s -s "%s(%s)" -o %s.csv' % (p, var, r, f, s, var, var))

# PAWC
var = 'pawc'
r = '/mnt/Data/GIS/PAWC_1m/pawc_1m/hdr.adf'
os.system('exactextract -p %s -r %s:%s -f %s -s "%s(%s)" -o %s.csv' % (p, var, r, f, s, var, var))