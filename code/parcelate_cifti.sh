#!/bin/bash
#
# Parcellate a single subject's cortical thickness cifti file.
# Designed to be called once per subject, e.g. as the command line of a
# SLURM array job where each array task supplies a different subject ID
# (e.g. from a subject-list file indexed by $SLURM_ARRAY_TASK_ID).
#
# Usage:
#   parcelate_cifti.sh <subject-id> [output-dir]
#
# Args:
#   $1  subject ID, e.g. 'sub-NDARINV00LJVZK2'
#   $2  output directory for the parcellated text file (optional,
#       defaults to $out below)

set -euo pipefail

deriv_root="/data/project/impulsivity/dataset/inm7-superds/original/abcd/derivatives/abcd-hcp-pipeline"
glasser_parc_path="${deriv_root}/HCP2016FreeSurferSubcortical_dparc.dlabel.nii"
out="${2:-/data/project/impulsivity/pfactors/code/juseless/res}"

if [[ $# -lt 1 ]]; then
    echo "Usage: $0 <subject-id> [output-dir]" >&2
    exit 1
fi
sub="$1"

infile="${deriv_root}/${sub}/ses-baselineYear1Arm1/anat/${sub}_ses-baselineYear1Arm1_space-fsLR32k_thickness.dscalar.nii"
if [[ ! -f "$infile" ]]; then
    echo "Input file not found: $infile" >&2
    exit 1
fi

mkdir -p "$out"
res_fpath="${out}/${sub}.txt"

tmp_dir=$(mktemp -d)
trap 'rm -rf "$tmp_dir"' EXIT
intermed_f="${tmp_dir}/${sub}.pscalar.nii"

echo "Subject: $sub"
echo "Using parcellation: $glasser_parc_path"
echo "Input file: $infile"

# parcellate
wb_command -cifti-parcellate "$infile" "$glasser_parc_path" COLUMN "$intermed_f" -only-numeric -legacy-mode
echo "Parcellated, intermediate file: $intermed_f"

# extract to text file
wb_command -cifti-convert -to-text "$intermed_f" "$res_fpath"
echo "Wrote result: $res_fpath"
