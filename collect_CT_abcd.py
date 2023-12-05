

# %%
# Imports
from pathlib import Path
import sys
import os
import pandas as pd
import numpy as np
import datatable as dt

import nilearn
import nibabel as nib


# %%
################### SET UP #######################
# What to extract:
#derivative = sys.argv[1] # FC_pearson, FC_partial, Reho, Falf
file = '_ses-baselineYear1Arm1_space-fsLR32k_thickness.dscalar.nii'

# Which network/s to use
#network = ['seitzman_nodes.txt','imp_nodes_extended.txt','imp_nodes.txt'] # 'MotorHeckner_VOIs.txt', 'power_nodes.txt'

# Where is the dataset
path2dataset = Path('/data/project/impulsivity/pfactors/code/juseless/res')
###################################################


# Variables (relative path to current working directory (CWD))
wd = os.getcwd()
wd = Path(os.path.dirname(wd))
wd = Path(os.path.dirname(wd))


subs_fc = pd.read_csv('/data/project/impulsivity/pfactors/data/abcd_matched_groups_good_QC.csv')
subs = subs_fc[(subs_fc['matched_group'] == 2)]
subs = pd.DataFrame(subs)

outpath = wd / 'res'

print('current conda env:')
print(os.environ['CONDA_DEFAULT_ENV'])
print(f'INPUT dir {path2dataset}')
print(f'OUTPUT dir: {outpath}')
#print(f'Running seshion: {sesh}')
print(f'{subs}')
# %%
# Get and prepare data
sub_sessions = np.empty((0,360))
subs_w_data = np.empty((0,1))         # subs should be subs*1


# Install superdataset
#path2sprdataset = path2dataset / 'inm7-superds' # also relative to CWD
#dataset_url = 'https://jugit.fz-juelich.de/inm7/datasets/datasets_repo.git'  #ria+http://hcp-a.ds.inm7.de
#dataset_url = 'ria+http://hcp-a.ds.inm7.de'
#dataset_url = 'git@jugit.fz-juelich.de:inm7/datasets/datasets_repo.git'

#print(f'Cloning superdataset {path2sprdataset}')
#dataset = datalad.api.install(path = path2sprdataset.as_posix(), source = dataset_url)
#print('Superdataset cloned')

for sub in subs.values:
    subject = sub[0]
    #subject = 'sub-NDARINV5NVYVN9F' #sub-NDARINVZG4U8G7X   sub-NDARINV5NVYVN9F

    #print(f'Running subject: {subject}')

    # Get
    #path2dataset = path2sprdataset / 'original' / data
    #path2file = path2dataset / subject / folder_structure / f'{subject}{file}'

    # try:
    #     print(f'Getting data: {path2file}')
    #     #dataset.get(path2file.as_posix(), source='inm7-storage')
    #     datalad.api.get(path2file.as_posix(), source='inm7-storage', recursive=True, dataset = path2dataset.as_posix())
    #     print('Got')
        
    #     subs_w_data = np.append(subs_w_data, np.atleast_2d(subject), axis=0)
    #     #print(subs_w_data)
    # except:
    #     print('\nMISSING DATA!!\n')
    #     continue
    
    try:
        # img = nib.load(path2file.as_posix())
        # tc = img.get_fdata()
        #
        # timeseries = np.array(tc)
        #
        # if derivative == 'FC_pearson':
        #     conn_measure = ConnectivityMeasure(
        #         kind="correlation",
        #         vectorize=True,
        #         discard_diagonal=True,
        #         cov_estimator=EmpiricalCovariance()
        #     )
        #     connectome = conn_measure.fit_transform([timeseries])[0]

        # elif derivative == 'FC_partial':
        #     estimator = GraphicalLassoCV()
        #     estimator.fit(timeseries)
        #     connectome = -estimator.precision_ #??
        
        CT = pd.read_table(f'{path2dataset}/{subject}.txt',header=None)

        sub_sessions = np.append(sub_sessions, np.transpose(np.atleast_2d(CT)), axis=0)
        subs_w_data = np.append(subs_w_data, np.atleast_2d(subject), axis=0)

    except:
        print(f'{subject} has MISSING DATA!!')
        continue


# 

sub_ids = pd.DataFrame(subs_w_data, columns=['subID'])
sub_ids.to_csv(f'/data/project/impulsivity/pfactors/data/abcd_subs_w_imaging_CT_{len(subs_w_data)}.txt')


d = pd.concat([pd.DataFrame(subs_w_data, columns=['subID']), pd.DataFrame(sub_sessions)], axis=1)
names = file.replace('.','-').replace('_','-').rsplit('-')

# Save
out = f'{outpath}/HCP2016FreeSurferSubcortical_abcd_{names[2]}_{names[5]}_{names[4]}_{len(subs_w_data)}.csv'
print(f'Saving to {out}')

d.to_csv(out)

#DT = dt.Frame(d)
#DT.to_jay(out)
#print('FINISHED')




# wd = Path('/home/mgell/scratch/inm7-superds/original/abcd/derivatives/freesurfer-5.3.0-HCP/sub-NDARINVZZZP87KR/ses-baselineYear1Arm1/stats')
# file = 'lh.aparc.stats'
# path2subfile = wd / file
# stats = CorticalParcellationStats.read(path2subfile)

# # stats.structural_measurements

# # stats.whole_brain_measurements



# cols = stats.structural_measurements.keys()
# struct = stats.structural_measurements

# #cols[3] + '_' + struct['structure_name']
# surf  = struct['structure_name'] + '_' + cols[2]
# vol   = struct['structure_name'] + '_' + cols[3]
# thick = struct['structure_name'] + '_' + cols[4]

# new_cols = surf.append((vol,thick))
# #list((surf.to_list(),vol.to_list(),thick.to_list()))
# new_struct = np.concatenate((struct['surface_area_mm^2'].to_numpy(),struct['gray_matter_volume_mm^3'].to_numpy(),struct['average_thickness_mm'].to_numpy()), axis=None)

# d = pd.DataFrame(np.atleast_2d(new_struct), columns=new_cols)




# datalad.api.get(path = '/data/project/impulsivity/dataset/inm7-superds/original/abcd/derivatives/abcd-hcp-pipeline/sub-NDARINV003RTV85/ses-baselineYear1Arm1/func/sub-NDARINV003RTV85_ses-baselineYear1Arm1_task-rest_bold_atlas-HCP2016FreeSurferSubcortical_desc-filtered_timeseries.ptseries.nii', source='inm7-storage', recursive=True)
