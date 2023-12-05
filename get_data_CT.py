

# %%
# Imports
import sys
from pathlib import Path
import nest_asyncio
import datalad.api


# %%
################### SET UP #######################
# # What to extract:
# derivative = sys.argv[1] # FC_pearson, FC_partial, Reho, Falf

# # Where is the dataset
# path2dataset = Path('/data/project/impulsivity/dataset/')

# # Which session to run
# #sesh = 'rfMRI_REST1_AP' #'rfMRI_REST2_PA' # concat: 'rfMRI_REST' indiv: 'rfMRI_REST1_AP' 'tfMRI_CARIT_PA'
# sesh = ''
# #file = 'lh.aparc.stats' #'Movement_RelativeRMS_mean.txt'
# file = '_ses-baselineYear1Arm1_task-rest_bold_atlas-HCP2016FreeSurferSubcortical_desc-filtered_timeseries.ptseries.nii'

# # Dataset info
# data = 'abcd/derivatives/abcd-hcp-pipeline'
# folder_structure = Path('ses-baselineYear1Arm1/func')
# ###################################################



# # Variables (relative path to current working directory (CWD))
# wd = os.getcwd()
# wd = Path(os.path.dirname(wd))
# wd = Path(os.path.dirname(wd))


# subs_fc = pd.read_csv('/data/project/impulsivity/pfactors/data/abcd_matched_groups_good_QC.csv')
# subs = subs_fc[(subs_fc['matched_group'] == 2)]
# subs = pd.DataFrame(subs)

# outpath = wd / 'res'

# print('current conda env:')
# print(os.environ['CONDA_DEFAULT_ENV'])
# print(f'INPUT dir {path2dataset}')
# print(f'OUTPUT dir: {outpath}')
# print(f'Running seshion: {sesh}')
# print(f'{subs}')
# %%
# # Get and prepare data
# folder_structure = folder_structure / sesh

# #def prepare_data_lifespan(folder_structure, wd, sub, sesh, conf=None, movement=None):
# # get paths right

# sub_sessions = np.empty((0,71631))
# subs_w_data = np.empty((0,1))         # subs should be subs*1


# # Install superdataset
# path2sprdataset = path2dataset / 'inm7-superds' # also relative to CWD
# #dataset_url = 'https://jugit.fz-juelich.de/inm7/datasets/datasets_repo.git'  #ria+http://hcp-a.ds.inm7.de
# #dataset_url = 'ria+http://hcp-a.ds.inm7.de'
# dataset_url = 'git@jugit.fz-juelich.de:inm7/datasets/datasets_repo.git'

# #print(f'Cloning superdataset {path2sprdataset}')
# #dataset = datalad.api.install(path = path2sprdataset.as_posix(), source = dataset_url)
# #print('Superdataset cloned')

# for sub in subs.values:
#     subject = sub[0]
#     #subject = 'sub-NDARINV5NVYVN9F' #sub-NDARINVZG4U8G7X   sub-NDARINV5NVYVN9F

#     #print(f'Running subject: {subject}')

#     # Get
#     path2dataset = path2sprdataset / 'original' / data
#     path2file = path2dataset / subject / folder_structure / f'{subject}{file}'

path2file = sys.argv[1]

try:
    print(f'Getting data: {path2file}')
    datalad.api.get(path2file, source='inm7-storage', recursive=True) #dont need this because we are in the dataset already, dataset = path2dataset.as_posix())
    print('Got')
    
    #subs_w_data = np.append(subs_w_data, np.atleast_2d(subject), axis=0)
    #print(subs_w_data)
except:
    print('\nMISSING DATA!!\n')
#    continue
    
#     try:
#         img = nib.load(path2file.as_posix())
#         tc = img.get_fdata()

#         timeseries = np.array(tc)
        
#         if derivative == 'FC_pearson':
#             conn_measure = ConnectivityMeasure(
#                 kind="correlation",
#                 vectorize=True,
#                 discard_diagonal=True,
#                 cov_estimator=EmpiricalCovariance()
#             )
#             connectome = conn_measure.fit_transform([timeseries])[0]

#         elif derivative == 'FC_partial':
#             estimator = GraphicalLassoCV()
#             estimator.fit(timeseries)
#             connectome = -estimator.precision_ #??
        
#         sub_sessions = np.append(sub_sessions, np.atleast_2d(connectome), axis=0)

#         subs_w_data = np.append(subs_w_data, np.atleast_2d(subject), axis=0)
#     except:
#         print(f'{subject} has MISSING DATA!!')
#         continue


# # 

# sub_ids = pd.DataFrame(subs_w_data, columns=['subID'])
# sub_ids.to_csv(f'/data/project/impulsivity/pfactors/data/abcd_subs_w_imaging_{len(subs_w_data)}.txt')


# d = pd.concat([pd.DataFrame(subs_w_data, columns=['subID']), pd.DataFrame(sub_sessions)], axis=1)
# names = file.replace('.','-').replace('_','-').rsplit('-')

# # Save as .jay
# out = f'{outpath}/{names[7]}_abcd_{names[2]}_{names[4]}_{len(subs_w_data)}.jay'
# print(f'Saving to {out}')

# DT = dt.Frame(d)
# DT.to_jay(out)
# print('FINISHED')

