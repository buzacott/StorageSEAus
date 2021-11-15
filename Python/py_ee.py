import os

p = '../Data/Shapefiles/HRS_MDB_Catchments_Filtered.gpkg'
f = 'station_id'
s = 'mean'

out_path = os.path.join('..', 'Data', 'exact_extract')
if not os.path.exists(out_path):
    os.mkdir(out_path)

slga_path = '/media/PRJ-MuttamaCreek/data/soil/SLGA_national'

variables = {
    'DEM': '/home/abuzacott/Data/AWAPer/DEM9s.tif',
    'silica': '/media/PRJ-CCH/Cotter_ABuzacott/Data/SilicaIndex/silica_WGS84.tif',
    'clay_000_005': os.path.join(slga_path, 'CLY_000_005_EV_N_P_AU_NAT_C_20140801.tif'),
    'clay_005_015': os.path.join(slga_path, 'CLY_005_015_EV_N_P_AU_NAT_C_20140801.tif'),
    'clay_015_030': os.path.join(slga_path, 'CLY_015_030_EV_N_P_AU_NAT_C_20140801.tif'),
    'clay_030_060': os.path.join(slga_path, 'CLY_030_060_EV_N_P_AU_NAT_C_20140801.tif'),
    'clay_060_100': os.path.join(slga_path, 'CLY_060_100_EV_N_P_AU_NAT_C_20140801.tif'),
    'WoodyIntensity': '/media/PRJ-CCH/Cotter_ABuzacott/Data/AuscoverWoodyCover/AuscoverWoodyCover2000_2010_500m.tif',
    'WoodyCover': '/media/PRJ-CCH/Cotter_ABuzacott/Data/AuscoverWoodyCover/AuscoverWoodyCover2000_2010_500m_Binary.tif'
}

for k, v in variables.items():
    print(f'exactly extracting {k}')
    dest = os.path.join(out_path, f'{k}.csv')
    os.system(f'exactextract -p {p} -r {k}:{v} -f {f} -s "mean({k})" -s "stdev({k})" -s "min({k})" -s "max({k})" -o {dest}')
