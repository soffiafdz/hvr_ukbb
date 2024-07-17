#!/usr/bin/env bash

## Apply CNN ensemble models to UKBB subjects
## Uses *shuffled* list to allow parallel run on diff computers

## Need to load pytorch-1.10.1 environment!!

TMPDIR=$(mktemp -d --tmpdir)
trap "rm -rf $TMPDIR" 0 1 2 15

set -xue

BASE_DIR=/ipl/ipl27/sfernandez/hvr_ukbb
QC_DIR=${BASE_DIR}/plots/qc_segmentations
OUT_DIR=${BASE_DIR}/data/derivatives/segmentations
LIB_DIR=${BASE_DIR}/library_cnn

function apply_cnn() {
	# Local variables
	local input=$1
	local output=$2

	local ref=${LIB_DIR}/ref_extra.mnc
	local model=${LIB_DIR}/ensemble_hvr_extra.pth

	local bname=$(basename $1 .mnc)

	local flip=$TMPDIR/flip.xfm
	[[ -e $flip ]] || param2xfm -scales -1 1 1 $flip

	# Left/Right
	local left=${TMPDIR}/${bname}_left.mnc
	local right=${TMPDIR}/${bname}_right.mnc

	[[ -e $right ]] || itk_resample $input --clobber --like $ref $right

	[[ -e $left ]] || itk_resample $input \
		--clobber --like $ref  \
		--transform $flip \
		$left

	# Apply on left
	date
	local left_seg=${TMPDIR}/${bname}_left_seg.mnc
	python ${BASE_DIR}/code/py_deep_seg/apply_multi_model.py \
		--stride 32 --patch 96 --cpu --crop 8 \
		$model $left $left_seg

	# Apply on right
	date
	local right_seg=${TMPDIR}/${bname}_right_seg.mnc
	python ${BASE_DIR}/code/py_deep_seg/apply_multi_model.py \
		--stride 32 --patch 96 --cpu --crop 8 \
		$model $right $right_seg

	date
	local left_rec=${TMPDIR}/${bname}_left_seg_recoded.mnc
	itk_resample --clobber --labels --byte --like $input \
		--transform $TMPDIR/flip.xfm \
		--lut-string "1 111; 2 112; 3 113; 4 121; 5 122; 6 123; 7 130" \
		$left_seg $left_rec

	local right_rec=${TMPDIR}/${bname}_right_seg_recoded.mnc
	itk_resample --clobber --labels --byte --like $input \
		--lut-string "1 211; 2 212; 3 213; 4 221; 5 222; 6 223; 7 230" \
		$right_seg $right_rec

	minccalc -q -clobber -labels -byte -express 'A[0]>0?A[0]:A[1]' \
		$left_rec $right_rec $output
}

function qc_plot() {
	# Local variables
	local img=$1
	local labels=$2

	local bname=$(basename $labels .mnc)
	local plotter=${BASE_DIR}/code/qc_plot.pl

	[[ -d $QC_DIR ]] || mkdir -p $QC_DIR
	local qcImg=${QC_DIR}/${bname}.jpg

	$plotter $img $labels -title $bname -clobber $qcImg
}

# Read subject list into an Array
mapfile -t IDS < ${BASE_DIR}/lists/ukbb_list.lst
IDS=( $(shuf -e "${IDS[@]}") )

# Main
for id in "${IDS[@]}"
do
	sub=$(printf $id | cut -d, -f1)
	session=$(printf $id | cut -d, -f2)

	in_mri=${BASE_DIR}/data/ukbb_t1w/${sub}/tal_${sub}_${session}_t1w.mnc
	bname=${sub}_${session}_hcvcag
	output=${OUT_DIR}/${bname}.mnc

	if [[ ! -e $output ]]
	then
		touch $output
		apply_cnn $in_mri $output
		qc_plot $in_mri $output
	fi
done
