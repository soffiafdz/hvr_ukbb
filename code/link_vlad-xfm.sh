#! /usr/bin/env bash

## Link ukbb xfm from Vlad's pipelines for QC
#set -ux

# TODO: Change `lng`
HERE=/ipl/ipl27/sfernandez/hvr_ukbb
PPDIR=${HERE}/data/pp-vlad_t1
PPLNGDIR=${HERE}/data/pp-vlad_lng_t1
DESTDIR=${HERE}/data/pp-xfms_vlad

# Loop through Vlad's files
subdirs=($(fd -d 1 -t "d" "sub" "$PPDIR"))
printf "%s\n" ${subdirs[@]} | \
	pv -l -s ${#subdirs[@]} | \
	while read -r subdir
	do
		sub=$(basename $subdir)
		# Check that xfm exists
		xfm_v1=$(fd -d 1 "xfm" "$subdir")
		[ -z $xfm_v1 ] && continue

		# Check that same subject was preprocessed by the lng pp
		printf -v lngdir "%s/%s" $PPLNGDIR $sub
		[ -d $lngdir ] || continue

		xfm_v2=$(fd -d 1 "ses-2_t1.xfm" "$lngdir")
		[ -z $xfm_v2 ] && continue

		# Create outdirectory
		printf -v outdir "%s/%s" $DESTDIR $sub
		[ -d $outdir ] || mkdir -p $outdir

		# Link Vlad's xfm file
		printf -v xfm_out1 "%s/%s_v1.xfm" $outdir $sub
		ln -sf $xfm_v1 $xfm_out1
		printf -v xfm_out2 "%s/%s_v2.xfm" $outdir $sub
		ln -sf $xfm_v2 $xfm_out2
	done
