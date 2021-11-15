import pandas as pd
import os

os.chdir('../Data/ExactExtract')

files = os.listdir()

clay_files = [s for s in files if 'clay' in s]

clay_dfs = []
for f in clay_files:
    name = f.split('.')[0]
    split = name.split('_')
    
    depth = int(split[2]) - int(split[1])
    
    df = pd.read_csv(f)
    df.set_index('station_id', inplace=True)

    df[f'{name}_mean'] = df[f'{name}_mean'] * depth/100
    
    clay_dfs.append( df[f'{name}_mean'] )

clay = pd.concat(clay_dfs, axis=1)
clay['Clay'] = clay.sum(axis=1)
clay[['Clay']].to_csv('Clay1m.csv')