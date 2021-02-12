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

# Strip to dirs only
dirs = [d.strip().split(' ')[-1] for d in dirs]

# Write directory
writeDir = os.path.join('/media', 'HGIS', 'SLGA', 'Clay')
if not os.path.exists(writeDir):
    os.mkdir(writeDir)

# Download the clay files only
with ftplib.FTP(url) as ftp:
    try:
        ftp.login()
        ftp.cwd('tern-soils/Products/National_digital_soil_property_maps/Clay')
        files = []
        ftp.dir(files.append)
        files = [f.strip().split(' ')[-1] for f in files]
        pattern = '*_EV_*' # Get expected value rasters only
        files = fnmatch.filter(files, pattern)
        for f in files:
            fname = os.path.join(writeDir, f)
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

# To download everything
# with ftplib.FTP(url) as ftp:
#     try:
#         ftp.login()
#         for d in dirs:
#             ftp.cwd('tern-soils/Products/National_digital_soil_property_maps/' + d)
#             writeDir = os.path.join('/media', 'HGIS', 'SLGA', d)
#             if not os.path.exists(writeDir):
#                 os.mkdir(writeDir)
            
#             files = []
#             ftp.dir(files.append)
#             files = [f.strip().split(' ')[-1] for f in files]
#             pattern = '*_EV_*' # Get expected value rasters only       
#             files = fnmatch.filter(files, pattern)

#             for f in files:
#                 fname = os.path.join(writeDir, f)
#                 if not os.path.exists(fname):
#                     print('Downloading ' + f)
#                     try:
#                         with open(fname, 'wb') as o:
#                             ftp.retrbinary('RETR ' + f, o.write)
#                     except ftplib.all_errors as e:
#                         print('Failed downloading ' + f)
#     except ftplib.all_errors as e:
#         print('FTP error:', e)
    
#     ftp.quit()

