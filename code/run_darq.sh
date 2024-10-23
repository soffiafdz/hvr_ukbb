#!/usr/bin/env bash

## Apply CNN ensemble models to UKBB subjects
## Longitudinal version
## Uses *shuffled* list to allow parallel run on diff computers

BASE_DIR=/ipl/ipl27/sfernandez/hvr_ukbb
MRI_DIR=${BASE_DIR}/data/ukbb_t1w_lng
QC_DIR=${BASE_DIR}/data/darq_output
LIST=${BASE_DIR}/lists/subs_lng.csv
DARQ=${BASE_DIR}/code/DARQ/python/aqc_apply.py

set -xu

# Read subject list into an Array
mapfile -t IDS < $LIST
IDS=( $(shuf -e "${IDS[@]}") )

# Main
[ -d $QC_DIR ] || mkd $QC_DIR
for id in "${IDS[@]}"
do
	sub=$(printf $id | cut -d, -f1)
	session=$(printf $id | cut -d, -f2)

	in_mri=${MRI_DIR}/${sub}/stx2_${sub}_${session}_t1.mnc

	[ ! -f $in_mri ] && continue

	out_qc=${QC_DIR}/${sub}_${session}.txt

	[ -f $out_qc ] && continue

	printf "%s,%s,%s\n" \
		$sub $session \
		$(python3 $DARQ --volume $in_mri --net r34 --raw) > $out_qc
done
