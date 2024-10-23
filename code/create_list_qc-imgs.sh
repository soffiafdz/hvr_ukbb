#! /usr/bin/env bash

## Parse lists of subs & paths to create a single list with paths to
## stx/stx2 qc imgs

set -ux

HERE=/ipl/ipl27/sfernandez/hvr_ukbb
QC_DIR=${HERE}/lists/aqc_jpgs

SUBS_LISTS=$(ls ${QC_DIR}/*_subs.lst)

LIST=${QC_DIR}/paths.lst
[ -f $LIST ] && rm $LIST

MISSING=${QC_DIR}/missing.lst
[ -f $MISSING ] && rm $MISSING

for sub_list in $SUBS_LISTS
do
	path_list=${sub_list/subs/paths}
	path_list2=${sub_list/subs/paths2}

	while IFS= read -r line
	do
		# Replace "/" -> "_"
		sub_ses=$(echo "$line" | tr '/' '_')

		printf "%s: Checking %s" $(date +%H:%M:%S) $sub_ses
		grep $sub_ses $path_list >> $LIST \
			|| grep $sub_ses $path_list2 >> $LIST \
			|| echo $sub_ses >> $MISSING
	done < $sub_list
done
