
# Collect predictions
from glob import glob
from pathlib import Path
import sys
import pandas as pd
import numpy as np
import os


# sample used
analysis = sys.argv[1]

# FC
#opts = '_averaged-source_HCP2016FreeSurferSubcortical_abcd_baselineYear1Arm1_rest_allsubs_zscored-beh_abcd_cbcl_grps_model_fits_baseline'
#opts = '_averaged-source_HCP2016FreeSurferSubcortical_abcd_baselineYear1Arm1_rest_allsubs_zscored-beh_abcd_cbcl_grps_model_fits_followup'
#opts = '_averaged-source_HCP2016FreeSurferSubcortical_abcd_baselineYear1Arm1_rest_allsubs_zscored-beh_abcd_cbcl_grps_model_fits_cor_factors_baseline'

# CT
#opts = '_averaged-source_HCP2016FreeSurferSubcortical_abcd_baselineYear1Arm1_thickness_fsLR32k_allsubs_zscored-beh_abcd_cbcl_grps_model_fits_cor_factors_baseline'

# paths
wd = os.getcwd()
wd = Path(os.path.dirname(wd))
wd = Path(os.path.dirname(wd))

in_path =  wd / 'res/mean_accuracy'
out_path = wd / 'res/collected'

# which behaviors
behs = pd.read_table(f'{wd}/code/juseless/opts/behs.txt', header=None)

# which beh was simulated? excluding rel values.
# e.g.: 'interview_age_wnoise'
if analysis == 'ridgeCV_z':
    pipe = 'ridgeCV_zscore'
    first = '_cbcl_scr_syn_totprob_t'
    # First load empirical results, then append all simulation res to it
    res = pd.read_csv(f'{in_path}/pipe_{pipe}{opts}_cbcl_scr_syn_totprob_t-rseed_123456-res.csv')
    res['beh'] = first
elif analysis == 'ridgeCV_zscore_group_2Fold':
    pipe = 'ridgeCV_zscore_group_2Fold'
    first = '_cbcl_scr_syn_totprob_t'
    # First load empirical results, then append all simulation res to it
    res = pd.read_csv(f'{in_path}/pipe_{pipe}{opts}_cbcl_scr_syn_totprob_t-rseed_123456-cv_res.csv')
    res['beh'] = first
elif analysis == 'ridgeCV_zscore_2Fold_confs':
    pipe = 'ridgeCV_zscore_group_2Fold_confound_removal_wcategorical'
    first = '_cbcl_scr_syn_totprob_t'
    # First load empirical results, then append all simulation res to it
    #res = pd.read_csv(f'{in_path}/pipe_{pipe}{opts}_AD_DTZDP-rseed_123456-cv_res.csv')
    res = pd.read_csv(f'{in_path}/pipe_{pipe}{opts}_aggr_sum_bin-rseed_123456-cv_res.csv')
    res['beh'] = first
elif analysis == 'xgboost_confs':
    pipe = 'xgboost_group_2Fold_confound_removal_wcategorical'
    first = 'P_ACH2F'
    # First load empirical results, then append all simulation res to it
    # pipe_xgboost_group_2Fold_confound_removal_wcategorical_averaged-source_HCP2016FreeSurferSubcortical_abcd_baselineYear1Arm1_rest_3517_zscored-beh_abcd_cbcl_grps_model_fits_baseline_SOC_ACH8-rseed_123456-cv_res.csv
    res = pd.read_csv(f'{in_path}/pipe_{pipe}{opts}_P_ACH2F-rseed_123456-cv_res.csv')
    res['beh'] = first
else:
    print('NOT KNOWN')

# which files
f_designator = pipe+opts
print(f'Looking for: {f_designator}*')

for beh_i in behs[0]:

    beh_i_str = str(beh_i)
    print(beh_i_str)
    files = glob(f"{in_path}/pipe_{f_designator}_{beh_i_str}-*")

    for f_i in files:
        f = pd.read_csv(f_i)
        f['beh'] = beh_i_str
        res = pd.concat([res, f], ignore_index=True)

# Save
out_path.mkdir(parents=True, exist_ok=True)
out_file = out_path / f'{pipe}{opts}_all_behs.csv'
res.to_csv(out_file, index=False)

print(f'Saved to: {out_file}')
