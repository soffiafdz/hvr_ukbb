#! /usr/bin/env bash

## Link ukbb files listed in subject list
set -ux

## T1w & T1w mask
HERE=/ipl/ipl27/sfernandez/hvr_ukbb
SOURCE=/ipl/uk-biobank/ukbb/out_20240408_lng
OUTDIR=${HERE}/data/ukbb_t1w_lng

LINK_LIST=${HERE}/lists/links_ukbb_lng.csv
SUBS_SES=${HERE}/lists/subs_lng.csv

mapfile -t IDS < $SUBS_SES

printf "sub,sess,t1_src,mask_src,xfm_src,t1_ln,mask_ln,xfm_ln\n" > $LINK_LIST

for id in ${IDS[@]}
do
	sub=$(echo $id | cut -d, -f1)
	sess=$(echo $id | cut -d, -f2)
	outdir=${OUTDIR}/${sub}
	[[ -d $outdir ]] || mkdir -p $outdir

	t1w=${SOURCE}/${sub}/${sess}/stx2/stx2_${sub}_${sess}_t1.mnc

	if [[ -f $t1w ]]
	then
		t1w_exist=1
		ln -sfv $t1w $outdir
	else
		t1w_exist=0
	fi

	[[ -f ${outdir}/$(basename $t1w) ]] && t1w_ln=1 || t1w_ln=0

	mask=${t1w/t1/mask}
	if [[ -f $mask ]]
	then
		mask_exist=1
		ln -sfv $mask $outdir
	else
		mask_exist=0
	fi

	[[ -f ${outdir}/$(basename $mask) ]] && mask_ln=1 || mask_ln=0

	xfm=${t1w/mnc/xfm}
	if [[ -f $xfm ]]
	then
		xfm_exist=1
		ln -sfv $xfm $outdir
	else
		xfm_exist=0
	fi

	[[ -f ${outdir}/$(basename $xfm) ]] && xfm_ln=1 || xfm_ln=0

	printf "%s,%s,%i,%i,%i,%i,%i,%i\n" \
		$sub $sess $t1w_exist $mask_exist $xfm_exist $t1w_ln $mask_ln $xfm_ln\
		>> $LINK_LIST
done
