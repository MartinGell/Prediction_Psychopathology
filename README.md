
# Disentangling Brain-Psychopathology Associations

This is the code repo for our manuscript "Disentangling Brain-Psychopathology Associations: A Systematic Evaluation of Transdiagnostic Latent Factor Models" available [here](https://www.biorxiv.org/content/10.64898/2025.12.21.695029v3.full.pdf+html)

<br />


## Prediction of Psychopathology factors

This code is based on a prediction pipeline from https://github.com/MartinGell/Prediction_Reliability with minor changes and additions. For more detailed overivew see that repo.

<br />

Example files to run the updated prediction script in this repo can be found here: 
- Example functional connectivity file is in `/input/` 
- Example behavioural data file is in `/inpuy/pheno/` 

Alongside with the two input files, two additional arguments are required: (i) the column to be predicted from the file with behavioural data and (ii) the prediction model to run (see `code/func/models.py` for available options). To recreate our findings, the user should choose the `ridgeCV_zscore_group_2Fold_confound_removal_wcategorical` model, which fits a Ridge regression on z-scored data after removing specified confounds (these can be specified in `prediction_nestedCV_val.py`), in this case the 'sex' variable. Therefore assuming the example files are the actual processed ABCD data which cannot be shared freely here, the analyses for this project would be recreated thus: 
```
$ python3 prediction_nestedCV_val.py Example_Schaefer400x17_data.csv Example_factors_with_grouping.csv P_CLRK ridgeCV_zscore_group_2Fold_confound_removal_wcategorical
```

Other parameters (e.g. cross-validation, confound removal) have to be changed within the script itself.

<br />

## Required Conda Environment
For this script to work a number of python modules are required. The easiest way to get these is using miniconda.

<br />

### Miniconda
In 'reqs' folder use the env_setup.yml to create the environemnt which will be called 'prediction':  
`conda env create -f (cloned_dir)/reqs/env_setup.yml`

Check env was installed correctly:  
`conda info --envs`

There should now be a ((miniconda_dir)/envs/prediction) visible

To activate env:  
`conda activate prediction`

References: https://medium.com/swlh/setting-up-a-conda-environment-in-less-than-5-minutes-e64d8fc338e4

