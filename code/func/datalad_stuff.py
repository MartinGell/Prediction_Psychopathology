# imports
from pathlib import Path
import nest_asyncio
nest_asyncio.apply()
import datalad.api

__all__ = ['prepare_data','remove_data','prepare_data_lifespan']


def prepare_data(folder_structure, wd, sub, conf = None):
    """
    This function does the datalad stuff and outputs paths to requested
    files
    """

    # get paths right
    wd = Path(wd)
    subject = Path(sub)

    # Install datalad dataset
    path2dataset = wd / 'hcp-functional-connectivity' # also relative to CWD)
    dataset_url = 'https://github.com/datalad-datasets/hcp-functional-connectivity.git'

    print(f'Cloning dataset {path2dataset}')
    dataset = datalad.api.install(path = path2dataset, source = dataset_url)
    print('Dataset cloned')

    # Get rsFMRI
    # paths relative to datalad dataset
    path2sub_files = subject / folder_structure
    path2rsfmri = path2sub_files / 'rfMRI_REST1_LR_hp2000_clean.nii.gz'

    print(f'Getting data {path2rsfmri}')
    dataset.get(path2rsfmri)
    print('Got')

    # Entire path to BOLD image not relative to dataset
    path2bold = path2dataset / path2rsfmri

    return dataset, path2bold





def prepare_data_lifespan(folder_structure, wd, sub, sesh, conf=None, movement=None):
    """
    This function does the datalad stuff and outputs paths to requested
    files
    """

    # get paths right
    wd = Path(wd)
    subject = Path(sub)

    # Install superdataset
    path2sprdataset = wd / 'inm7-superds' # also relative to CWD)
    #dataset_url = 'https://jugit.fz-juelich.de/inm7/datasets/datasets_repo.git'
    dataset_url = 'git@jugit.fz-juelich.de:inm7/datasets/datasets_repo.git'

    print(f'Cloning superdataset {path2sprdataset}')
    dataset = datalad.api.install(path = path2sprdataset, source = dataset_url)
    print('Superdataset cloned')

    # Get HCP aging
    path2dataset = path2sprdataset / 'original' / 'hcp' / 'hcp_aging'
    path2bold = path2dataset / subject / folder_structure / f'{sesh}_hp0_clean.nii.gz'

    print(f'Getting data: {path2bold}')
    dataset.get(path2bold, source='inm7-storage')
    print('Got')

    # Get confounds file if requested
    if conf:
        # location of atlas with tissue classes
        path2atlas = path2dataset / subject / 'MNINonLinear' / 'ROIs' / 'Atlas_wmparc.2.nii.gz'
        
        print(f'Getting tissue classes info {path2atlas}')
        dataset.get(path2atlas)
        print('Got')

        return dataset, path2bold, path2atlas
    else:
        # To do: add option for getting motion params
        return dataset, path2bold





def remove_data(dataset,*paths2files2drop):
    #need to check if it works with entire path2bold, if not push it outside the script and have above function outuput dataset path and fmri path
    for arg_i in paths2files2drop:
        print(f'Droping: {arg_i}')
        dataset.drop(arg_i)
        print('Droped')

