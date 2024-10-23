#!/usr/bin/env bash

## Apply CNN ensemble models to UKBB subjects
## Longitudinal version
## Uses *shuffled* list to allow parallel run on diff computers

## Need to load pytorch-1.10.1 environment!!

TMPDIR=$(mktemp -d --tmpdir)
trap "rm -rf $TMPDIR" 0 1 2 15

set -xue

BASE_DIR=/ipl/ipl27/sfernandez/hvr_ukbb
QC_DIR=${BASE_DIR}/plots/qc_segmentations_2024
OUT_DIR=${BASE_DIR}/data/derivatives/segmentations_2024
LIB_DIR=${BASE_DIR}/library_cnn

# Read subject list into an Array
mapfile -t IDS < ${BASE_DIR}/lists/subs_lng_2run.csv
#IDS=( $(shuf -e "${IDS[@]}") )
IDS=( "${IDS[@]:$1:$2}" )
echo "" > ${BASE_DIR}/lists/subs_lng_2run.csv

# Main
for id in "${IDS[@]}"
do
	sub=$(printf $id | cut -d, -f1)
	session=$(printf $id | cut -d, -f2)

	in_mri=${BASE_DIR}/data/ukbb_t1w_lng/${sub}/stx2_${sub}_${session}_t1.mnc

	if [ ! -f $in_mri ]
	then
		echo $id >> ${BASE_DIR}/lists/unfound_subs_lng.lst
		continue
	fi

	bname=${sub}_${session}_hcvcag
	output=${OUT_DIR}/${bname}.mnc

	[ -e $output ] && continue
	echo $id >> ${BASE_DIR}/lists/subs_lng_2run.csv
done
