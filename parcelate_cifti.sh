#!/bin/bash

# args:
#   $1: fmri file path + filename
#   should be just 'sub-NDARINV00LJVZK2'
#   $2: Output path
out=/data/project/impulsivity/pfactors/code/juseless/res

#glasser_parc_path="/home/mgell/Q1-Q6_RelatedValidation210.CorticalAreas_dil_Final_Final_Areas_Group_Colors.32k_fs_LR.dlabel.nii" 
glasser_parc_path="/data/project/impulsivity/dataset/inm7-superds/original/abcd/derivatives/abcd-hcp-pipeline/HCP2016FreeSurferSubcortical_dparc.dlabel.nii"

#mean_fame+="${2}mean.dscalar.nii"
#stdev_fpath+="${2}stdev.dscalar.nii"
#zscored_fpath+="${2}z.dtseries.nii"

infile="${1}/ses-baselineYear1Arm1/anat/${1}_ses-baselineYear1Arm1_space-fsLR32k_thickness.dscalar.nii" #sub-NDARINV00LJVZK2/ses-baselineYear1Arm1/anat/sub-NDARINV00LJVZK2_ses-baselineYear1Arm1_space-fsLR32k_thickness.dscalar.nii
#intermed_f="${2}.pscalar.nii"
intermed_f="/tmp/throw_away_${1}.pscalar.nii"
res_fpath="$out/${1}.txt"

echo "Using parcellation: $glasser_parc_path"

# zscore first
#wb_command -cifti-reduce "$1" MEAN "$mean_fame"

#wb_command -cifti-reduce "$1" STDEV "$stdev_fpath"

#wb_command -cifti-math '(x - mean) / stdev' "$zscored_fpath" -fixnan 0 -var x "$1" -var mean "$mean_fame" -select 1 1 -repeat -var stdev "$stdev_fpath" -select 1 1 -repeat
#echo "z-scoring ts done, saving $zscored_fpath"

# datalad stuff
#echo "getting $infile"
#datalad get -d "/data/project/impulsivity/dataset/inm7-superds/original/abcd/derivatives/abcd-hcp-pipeline/sub-NDARINV003RTV85/ses-baselineYear1Arm1/anat/" "$1"
#datalad get "$infile" # has to be ran from /data/.../derivatives/abcd-hcp-pipeline/
#python /data/project/impulsivity/pfactors/code/juseless/get_data_CT.py "$infile"

# parcellate
#wb_command -cifti-parcellate "$zscored_fpath" "$glasser_parc_path" COLUMN "$3" -only-numeric
wb_command -cifti-parcellate "$infile" "$glasser_parc_path" COLUMN "$intermed_f" -only-numeric -legacy-mode
echo "parcellated, saving $intermed_f"

# extract to text file
wb_command -cifti-convert -to-text "$intermed_f" "$res_fpath"
echo "writing file $res_fpath"

