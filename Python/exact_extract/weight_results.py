import pandas as pd
import os

os.chdir('Results')

files = os.listdir()

clay_files = [s for s in files if 'clay' in s]

clay_dfs = []
for f in clay_files:
    split =  f.split('_') 
    depth = int(split[2][0:3]) - int(split[1])
    df = pd.read_csv(f)
    df[f.split('.')[0]+'_mean'] = df[f.split('.')[0]+'_mean'] * depth/100
    df.set_index('Station_Nu', inplace=True)
    clay_dfs.append(df)

clay = pd.concat(clay_dfs, axis=1)
clay['mean'] = clay.sum(axis=1)

clay[['mean']].to_csv('Clay.csv')

awc_files = [s for s in files if 'awc' in s]

awc_dfs = []
for f in awc_files:
    split =  f.split('_') 
    depth = int(split[2][0:3]) - int(split[1])
    df = pd.read_csv(f)
    df[f.split('.')[0]+'_mean'] = df[f.split('.')[0]+'_mean'] * depth/100
    df.set_index('Station_Nu', inplace=True)
    awc_dfs.append(df)

awc = pd.concat(awc_dfs, axis=1)
awc['mean'] = awc.sum(axis=1)

awc[['mean']].to_csv('AWC.csv')