
# Collect predictions
from glob import glob
from pathlib import Path
import sys
from unittest import skip
import pandas as pd
import numpy as np


# sample used
pipe = sys.argv[1]

#"pipe_{pipe}_averaged-beh_{beh}-avg_{src_fc}_and_{val_FC_file}-rseed_{rs}-res.csv"
opts = '_averaged-beh'

# paths 
in_path =  Path('/data/project/impulsivity/pfactors/res/disc_rep/manual_two_fold_CV')
out_path = Path('/data/project/impulsivity/pfactors/res/disc_rep/collected')

# which beh was simulated? excluding rel values.
# e.g.: 'interview_age_wnoise'
if pipe == 'ridgeCV_z_fc':
    desginator = 'FC'
    pipe = 'ridgeCV_zscore'
    first = '_cbcl_scr_syn_totprob_t'
    # First load empirical results, then append all simulation res to it
    # ridgeCV_averaged-source_Schaefer400x17_WM+CSF+GS_hcpaging_695-beh_interview_age_interview_age-rseed_123456-cv_res.csv
    res = pd.read_csv(f'{in_path}/pipe_{pipe}{opts}_cbcl_scr_syn_totprob_t-rseed_123456-res.csv')
    res['beh'] = first
    behs = pd.read_table('/data/project/impulsivity/pfactors/code/juseless/opts/behs.txt', header=None)
if pipe == 'ridgeCV_z_CT':
    desginator = 'FC'
    pipe = 'ridgeCV_zscore'
    first = '_cbcl_scr_syn_totprob_t'
    # First load empirical results, then append all simulation res to it
    # ridgeCV_averaged-source_Schaefer400x17_WM+CSF+GS_hcpaging_695-beh_interview_age_interview_age-rseed_123456-cv_res.csv
    res = pd.read_csv(f'{in_path}/pipe_{pipe}{opts}_cbcl_scr_syn_totprob_t-rseed_123456-res.csv')
    res['beh'] = first
    behs = pd.read_table('/data/project/impulsivity/pfactors/code/juseless/opts/behs.txt', header=None)

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
        res = res.append(f, ignore_index=True)

# Save
out_path.mkdir(parents=True, exist_ok=True)
out_file = out_path / f'{pipe}{opts}_all_behs_{desginator}.csv'
res.to_csv(out_file, index=False)
