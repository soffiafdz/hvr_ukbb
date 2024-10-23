#! /usr/bin/env bash

## Link ukbb xfm from Reza's pipeline for QC
#set -ux

# TODO: Change `lng`
HERE=/ipl/ipl27/sfernandez/hvr_ukbb
REZADIR=${HERE}/data/pp-reza_xfm
VLADDIR=${HERE}/data/pp-vlad_lng_t1
DESTDIR=${HERE}/data/pp-xfms
OUTLIST=${HERE}/lists/missing_vlad-reza_xfms.txt
missing_xfm=()

# Loop through Reza's files
subs=($(fd -d 1 "sub" "$REZADIR"))
# Named: sub-\d{7}.xfm
printf "%s\n" ${subs[@]} | \
	pv -l -s ${#subs[@]} | \
	while read -r sub
	do
		sub=$(basename $f .xfm)

		# Check that same subject was preprocessed by Vlad
		printf -v vlad_xfm "%s/%s/stx2_%s_ses-2_t1.xfm" $VLADDIR $sub $sub
		if [ ! -f $vlad_xfm ]
		then
			missing_xfm+=( $sub )
			continue
		fi

		# Create outdirectory
		printf -v outdir "%s/%s" $DESTDIR $sub
		[ -d $outdir ] || mkdir -p $outdir

		# Invert Reza's transformation so they're comparable
		printf -v reza_xfm "%s/%s" $REZADIR $f
		printf -v reza_inv "%s/%s_pp-reza_inv.xfm" $outdir $sub
		[ -f $reza_inv ] || xfminvert $reza_xfm $reza_inv

		# Link Vlad's xfm file
		printf -v vlad_ln "%s/%s_pp-vlad.xfm" $outdir $sub
		ln -sf $vlad_xfm $vlad_ln
	done

## Check if there were missing xfms (vlad's)
if [ ! ${#missing_xfm[@]} -eq 0 ]
then
	> $OUTLIST
	for sub in "${missing_xfm[@]}"
	do
		echo "$sub" >> $OUTLIST
	done
fi
