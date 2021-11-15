import os
import fnmatch
import ftplib

url = 'qld.auscover.org.au'

with ftplib.FTP(url) as ftp:
    try:
        ftp.login()
        ftp.cwd('tern-soils/Products/National_digital_soil_property_maps')
        dirs = []
        ftp.dir(dirs.append)
    except ftplib.all_errors as e:
        print('FTP error:', e)

    ftp.quit()

# Write directory
out_path = os.path.realpath('/media/PRJ-CCH/Cotter_ABuzacott/Data/SLGA')
if not os.path.exists(out_path):
    os.mkdir(out_path)

# To download everything
#dirs = [d.strip().split(' ')[-1] for d in dirs]
# Else manually pick variable e.g. dirs = ['Clay']
dirs = ['Depth_of_Regolith', 'Depth_of_Soil']

with ftplib.FTP(url) as ftp:
    try:
        ftp.login()
        for d in dirs:
            ftp.cwd('tern-soils/Products/National_digital_soil_property_maps/' + d)
            out_path_d = os.path.join(out_path, d)
            if not os.path.exists(out_path_d):
                os.mkdir(out_path_d)
            
            files = []
            ftp.dir(files.append)
            files = [f.strip().split(' ')[-1] for f in files]
            pattern = '*_EV_*' # Get expected value rasters only       
            files = fnmatch.filter(files, pattern)

            for f in files:
                fname = os.path.join(out_path_d, f)
                if not os.path.exists(fname):
                    print('Downloading ' + f)
                    try:
                        with open(fname, 'wb') as o:
                            ftp.retrbinary('RETR ' + f, o.write)
                    except ftplib.all_errors as e:
                        print('Failed downloading ' + f)
    except ftplib.all_errors as e:
        print('FTP error:', e)
    
    ftp.quit()
    