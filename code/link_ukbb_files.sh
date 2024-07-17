#! /usr/bin/env bash

## Link ukbb tal files listed in subject list
set -xeu

## T1w & T1w mask
WORK_PATH=/ipl/ipl27/sfernandez/hvr_ukbb

## List: QC >= 75%; 35_351 // 39_677
# ID,SESS,QC
QC_LIST=${WORK_PATH}/lists/aqc_pass.csv

mapfile -t IDS < $QC_LIST
[ ${#IDS[@]} -eq 0 ] && echo "$QC_LIST is empty" && exit 1
unset IDS[0] # Remove header

UKBB_PATH=/ipl/uk-biobank/ukbb/out_20210122
OUT_PATH=${WORK_PATH}/data/ukbb_t1w

LINK_LIST=${WORK_PATH}/lists/links_ukbb.csv
printf "sub,sess,t1_source,mask_source,t1_link,mask_link\n" > $LINK_LIST

for id in ${IDS[@]}
do
	sub=$(echo $id | cut -d, -f1)
	sess=$(echo $id | cut -d, -f2)
	outdir=${OUT_PATH}/${sub}
	[[ -d $outdir ]] || mkdir -p $outdir

	t1w=${UKBB_PATH}/${sub}/${sess}/tal/tal_${sub}_${sess}_t1w.mnc
	if [[ -f $t1w ]]
	then
		t1w_present=1
		ln -sfv $t1w $outdir
	else
		t1w_present=0
	fi

	[[ -f ${outdir}/$(basename $t1w) ]] && t1w_link=1 || t1w_link=0

	mask=${t1w/t1w/mask}
	if [[ -f $mask ]]
	then
		mask_present=1
		ln -sfv $mask $outdir
	else
		mask_present=0
	fi

	[[ -f ${outdir}/$(basename $mask) ]] && mask_link=1 || mask_link=0

	printf "%s,%s,%i,%i,%i,%i\n" \
		$sub $sess $t1w_present $mask_present $t1w_link $mask_link \
		>> $LINK_LIST
done
