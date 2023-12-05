#!/bin/bash

out=/data/project/impulsivity/pfactors/code/juseless/res

# Path to the text file
file_path="/data/project/impulsivity/pfactors/code/juseless/opts/grp2.txt"

glasser_parc_path="/data/project/impulsivity/dataset/inm7-superds/original/abcd/derivatives/abcd-hcp-pipeline/HCP2016FreeSurferSubcortical_dparc.dlabel.nii"

echo "Using parcellation: $glasser_parc_path"

while IFS= read -r fmri
do
  infile="/data/project/impulsivity/dataset/inm7-superds/original/abcd/derivatives/abcd-hcp-pipeline/${fmri}/ses-baselineYear1Arm1/anat/${fmri}_ses-baselineYear1Arm1_space-fsLR32k_thickness.dscalar.nii"
  intermed_f="/tmp/throw_away_${fmri}.pscalar.nii"
  res_fpath="$out/${fmri}.txt"

  # parcellate
  wb_command -cifti-parcellate "$infile" "$glasser_parc_path" COLUMN "$intermed_f" -only-numeric -legacy-mode
  echo "parcellated, saving $intermed_f"

  # extract to text file
  wb_command -cifti-convert -to-text "$intermed_f" "$res_fpath"
  echo "writing file $res_fpath"
done < "$file_path"