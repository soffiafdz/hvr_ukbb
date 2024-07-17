#!/usr/bin/env bash

## Shell script for extracting the ICC and SCALE_factor
## and exporting them into a list

set -xu

HERE=/ipl/ipl27/sfernandez/hvr_ukbb
DATA=${HERE}/data/ukbb_t1w
LIST=${HERE}/data/ukbb_t1_ids.txt
VOLUMES=${HERE}/data/icc_scale.csv

echo "PTID,ICC,SCALEFACTOR" > $VOLUMES

mapfile -t IDS < $LIST

for id in ${IDS[@]}
do
	stx=${DATA}/${id}/tal_${id}_ses-2_t1w.mnc
	mask=${DATA}/${id}/tal_${id}_ses-2_mask.mnc
	xfm=${DATA}/${id}/tal_xfm_${id}_ses-2.xfm

	# Failsafes
	[ ! -f $mask ] \
		&& printf "%s does not exist.\n" $mask \
		&& continue

	[ ! -f $xfm ] \
		&& printf "%s does not exist.\n" $xfm \
		&& continue

	# SCALEFACTOR from STX2 xfm
	scale=$(xfm2param $xfm |
		awk '/-scale/{print $2*$3*$4}')

	# ICC (native space)
	icc=$(print_all_labels $mask |
		awk -v scale=$scale '{printf "%.10f", $NF / scale}')

	printf "%s,%f,%f\n" \
		$id $icc $scale >> $VOLUMES
done < $LIST
