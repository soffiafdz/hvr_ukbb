#! /usr/bin/env bash

## Link ukbb xfm from Vlad's pipelines for QC
#set -ux

# TODO: Change `lng`
HERE=/ipl/ipl27/sfernandez/hvr_ukbb
PPDIR=${HERE}/data/pp-vlad_t1
PPLNGDIR=${HERE}/data/pp-vlad_lng_t1
DESTDIR=${HERE}/data/pp-xfms_vlad
LIST_NOTSUB=${HERE}/lists/missingv2fromv1_sub.txt
LIST_NOTXFM=${HERE}/lists/missingv2fromv1_xfm.txt

# Reinit lists
> $LIST_NOTSUB
> $LIST_NOTXFM

# Loop through Vlad's files
fd -d 2 "xfm" "$PPDIR" |
	# There are 35350 xfm files
	pv -l -s 35350 | \
	while read -r xfm_v1
	do
		sub=$(basename $(dirname $xfm_v1))

		# Check that same subject was preprocessed by the lng pp
		printf -v lngdir "%s/%s" $PPLNGDIR $sub
		[ ! -d $lngdir ] && echo $sub >> $LIST_NOTSUB && continue

		xfm_v2=$(fd -d 1 "ses-2_t1.xfm" "$lngdir")
		[ -z $xfm_v2 ] && echo $sub >> $LIST_NOTXFM && continue

		# Create outdirectory
		printf -v outdir "%s/%s" $DESTDIR $sub
		[ -d $outdir ] || mkdir -p $outdir

		# Link Vlad's xfm file
		printf -v xfm_out1 "%s/%s_v1.xfm" $outdir $sub
		ln -sf $xfm_v1 $xfm_out1
		printf -v xfm_out2 "%s/%s_v2.xfm" $outdir $sub
		ln -sf $xfm_v2 $xfm_out2
	done
