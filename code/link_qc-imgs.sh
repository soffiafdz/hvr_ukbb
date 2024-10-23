#! /usr/bin/env bash

## Parse list of qc-imgs and link paths to 5k-sized directories for QC

set -ux

HERE=/ipl/ipl27/sfernandez/hvr_ukbb
SOURCE=/ipl/uk-biobank/ukbb
PATHS=${HERE}/lists/qc_manual_post-darq.csv
LINKS=${HERE}/plots/qc_ukbb-lng_post-darq
OUTDIRS=( {5,10,15}k_qc-imgs )

declare -i counter
counter=0

declare -i dir
dir=0

while IFS= read -r line
do
	# Outdir
	printf -v outdir "%s/%s" $LINKS ${OUTDIRS[$dir]}
	[ -d $outdir ] || mkdir $outdir

	# Source image
	path=$(echo $line | cut -d, -f1)
	printf -v qc_in "%s/%s" $SOURCE $path

	# Out image
	darq_score=$(echo $line | cut -d, -f2)
	bname=$()
	printf -v qc_out "%s/%s_darq-%s.jpg" \
		$outdir \
		$(basename $path .jpg | cut -d_ -f4,5) \
		${darq_score:(-3)}

	# Link images and increase counters
	[ -f $qc_in ] && cp $qc_in $qc_out
	[ $? -eq 0 ] && counter+=1
	[ $counter -eq 5000 ] && counter=0 && dir+=1
done < $PATHS
