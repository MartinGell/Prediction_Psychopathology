

# %%
# Imports
from pathlib import Path
import sys
import os
import pandas as pd
import numpy as np
import func.datalad_stuff as datalad_stuff

from freesurfer_stats import CorticalParcellationStats

import nest_asyncio
nest_asyncio.apply()
import datalad.api

#from func.calc_mean_tissue_signal import mean_tissue


# %%
################### SET UP #######################
# Which network/s to use
#network = ['seitzman_nodes.txt','imp_nodes_extended.txt','imp_nodes.txt'] # 'MotorHeckner_VOIs.txt', 'power_nodes.txt'

# Where is the dataset
#path2dataset = Path('/data/project/impulsivity/dataset/')
path2dataset = Path('/tmp/')
#path2dataset = Path('/home/mgell/scratch/')

# Which session to run
#sesh = 'rfMRI_REST1_AP' #'rfMRI_REST2_PA' # concat: 'rfMRI_REST' indiv: 'rfMRI_REST1_AP' 'tfMRI_CARIT_PA'
sesh = ''
file = 'lh.aparc.stats' #'Movement_RelativeRMS_mean.txt'

# Dataset info
data = 'abcd/derivatives/freesurfer-5.3.0-HCP' #'hcp/hcp_aging'
#folder_structure = Path('MNINonLinear/Results')
folder_structure = Path('ses-baselineYear1Arm1/stats')

# Grey matter mask to use
#GM_mask = 'FSL_MNI152_GM025_overlap_vox_seitzman.nii' # FSL or CAT (in MaskenEtc)
###################################################



# Variables (relative path to current working directory (CWD))
wd = os.getcwd()
wd = Path(os.path.dirname(wd))
wd = Path(os.path.dirname(wd))

#sub = f'{str(sys.argv[1])}_V1_MR'
subs = pd.read_csv('/data/project/impulsivity/pfactors/data/subs.txt',header=None)

res_dir = wd / 'res'

print('current conda env:')
print(os.environ['CONDA_DEFAULT_ENV'])
print(f'INPUT dir {path2dataset}')
print(f'OUTPUT dir: {res_dir}')
print(f'Running seshion: {sesh}')
print(f'{subs}')
# %%
# Get and prepare data
folder_structure = folder_structure / sesh

#def prepare_data_lifespan(folder_structure, wd, sub, sesh, conf=None, movement=None):
# get paths right

FD = np.empty((0,1))
#subs = np.empty((0,1))         # subs should be subs*1


# Install superdataset
path2sprdataset = path2dataset / 'inm7-superds' # also relative to CWD
#dataset_url = 'https://jugit.fz-juelich.de/inm7/datasets/datasets_repo.git'  #ria+http://hcp-a.ds.inm7.de
#dataset_url = 'ria+http://hcp-a.ds.inm7.de'
dataset_url = 'git@jugit.fz-juelich.de:inm7/datasets/datasets_repo.git'

print(f'Cloning superdataset {path2sprdataset}')
dataset = datalad.api.install(path = path2sprdataset.as_posix(), source = dataset_url)
print('Superdataset cloned')

for sub in subs.values:
    #subject = Path(f'{sub[0]}_V1_MR')
    #subject = sub[0]
    #subject = subs.iloc[2]
    #subject = subject[0]
    subject = 'sub-NDARINV00LH735Y'

    print(f'Running subject: {subject}')

    # Get FD
    path2dataset = path2sprdataset / 'original' / data
    path2file = path2dataset / subject / folder_structure / file

    print(f'Getting data: {path2file}')
    dataset.get(path2file.as_posix(), source='inm7-storage')
    print('Got')


    stats = CorticalParcellationStats.read(path2file)

    cols = stats.structural_measurements.keys()
    struct = stats.structural_measurements

    #cols[3] + '_' + struct['structure_name']
    surf  = struct['structure_name'] + '_' + cols[2]
    vol   = struct['structure_name'] + '_' + cols[3]
    thick = struct['structure_name'] + '_' + cols[4]

    new_cols = surf.append((vol,thick))
    new_struct = np.concatenate((struct['surface_area_mm^2'].to_numpy(),struct['gray_matter_volume_mm^3'].to_numpy(),struct['average_thickness_mm'].to_numpy()), axis=None)

    d = pd.DataFrame(np.atleast_2d(new_struct), columns=new_cols)

    # #FD = pd.read_table(path2FD)
    # with open(path2FD, 'r') as f: sub_FD = f.read().strip()
    # FD = np.append(FD, np.atleast_2d(float(sub_FD)), axis=0)
    # subs = np.append(subs, np.atleast_2d(sub[0]), axis=0)

    datalad_stuff.remove_data(dataset,path2file)


# Save to a new file
d = pd.concat([pd.DataFrame(subs, columns=['subID']), pd.DataFrame(data)], axis=1)


print(FD)

FDd = pd.DataFrame(FD)
FDd.to_csv(f'{res_dir}/FD_{sesh}.csv')

print('FINISHED')




wd = Path('/home/mgell/scratch/inm7-superds/original/abcd/derivatives/freesurfer-5.3.0-HCP/sub-NDARINVZZZP87KR/ses-baselineYear1Arm1/stats')
file = 'lh.aparc.stats'
path2subfile = wd / file
stats = CorticalParcellationStats.read(path2subfile)

# stats.structural_measurements

# stats.whole_brain_measurements



cols = stats.structural_measurements.keys()
struct = stats.structural_measurements

#cols[3] + '_' + struct['structure_name']
surf  = struct['structure_name'] + '_' + cols[2]
vol   = struct['structure_name'] + '_' + cols[3]
thick = struct['structure_name'] + '_' + cols[4]

new_cols = surf.append((vol,thick))
#list((surf.to_list(),vol.to_list(),thick.to_list()))
new_struct = np.concatenate((struct['surface_area_mm^2'].to_numpy(),struct['gray_matter_volume_mm^3'].to_numpy(),struct['average_thickness_mm'].to_numpy()), axis=None)

d = pd.DataFrame(np.atleast_2d(new_struct), columns=new_cols)